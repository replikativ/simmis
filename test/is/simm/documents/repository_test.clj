(ns is.simm.documents.repository-test
  (:require [clojure.test :refer [deftest is testing]]
            [datahike.api :as d]
            [geschichte.repo :as repo]
            [is.simm.documents.markdown :as markdown]
            [is.simm.documents.repository :as documents]))

(defn- with-repository [f]
  (let [cfg {:store {:backend :memory :id (random-uuid)}
             :schema-flexibility :write :keep-history? true :branch-history? true
             :max-string-length 0 :max-bytes-length 0 :max-tuple-string-length 0}]
    (d/create-database cfg)
    (let [conn (d/connect cfg)]
      (try
        (documents/initialize! conn)
        (f conn)
        (finally (d/release conn))))))

(defn- node-count [conn]
  (count (d/q '[:find ?e :where [?e :document.node/kind]] @conn)))

(deftest source-and-queryable-syntax-publish-in-one-transaction
  (with-repository
    (fn [conn]
      (let [id (random-uuid)
            source "---\ncustom: [a, b]\n---\n# Hello\n\n- one\n  - two\n"
            observed (atom [])]
        (d/listen conn ::atomic
                  (fn [{:keys [db-after]}]
                    (when-let [doc (d/q '[:find (pull ?e [*]) . :in $ ?id
                                          :where [?e :document/id ?id]] db-after id)]
                      (swap! observed conj
                             {:revision (:document/revision doc)
                              :source (markdown/decode (repo/read (atom db-after) "notes/hello.md"))
                              :kinds (set (d/q '[:find [?kind ...] :where
                                                 [_ :document.node/kind ?kind]] db-after))}))))
        (try
          (documents/create! conn {:id id :path "notes/hello.md" :source source})
          (is (= 1 (count @observed)))
          (is (= source (:source (first @observed))))
          (is (= (markdown/revision source) (:revision (first @observed))))
          (is (contains? (:kinds (first @observed)) :Heading))
          (is (contains? (:kinds (first @observed)) :BulletList))
          (is (= source (:source (documents/read-document conn id))))
          (is (= {"custom" '("a" "b")} (get-in (documents/read-document conn id)
                                                [:frontmatter :properties])))
          (finally (d/unlisten conn ::atomic)))))))

(deftest edits-are-cas-checked-and-unchanged-views-do-not-write
  (with-repository
    (fn [conn]
      (let [id (random-uuid)]
        (documents/create! conn {:id id :path "hello.md" :source "# Hello\n\nKeep.\n"})
        (let [before (documents/read-document conn id)
              tx (:max-tx @conn)
              nodes (node-count conn)]
          (is (:unchanged? (documents/edit! conn id (:revision before) [])))
          (is (= tx (:max-tx @conn)))
          (documents/edit! conn id (:revision before) [{:from 2 :to 7 :text "World"}])
          (is (= "# World\n\nKeep.\n" (:source (documents/read-document conn id))))
          (is (= nodes (node-count conn)) "Old component nodes are not leaked")
          (is (thrown? clojure.lang.ExceptionInfo
                       (documents/edit! conn id (:revision before) [{:from 2 :to 7 :text "Lost"}])))
          (is (= "# World\n\nKeep.\n" (markdown/decode (repo/read conn "hello.md")))))))))

(deftest moves-preserve-document-identity-and-refuse-collisions
  (with-repository
    (fn [conn]
      (let [id (random-uuid)]
        (documents/create! conn {:id id :path "old.md" :source "# Heading\n"})
        (let [before (documents/read-document conn id)]
          (documents/move! conn id (:basis before) "folder/new.md")
          (is (nil? (repo/read conn "old.md")))
          (is (= id (:id (documents/read-document conn id))))
          (is (= (:revision before) (:revision (documents/read-document conn id))))
          (is (= "folder/new.md" (:path (documents/read-document conn id))))
          (is (thrown? Exception (documents/move! conn id (:basis before) "stale.md"))))
        (repo/write! conn "occupied.md" (.getBytes "existing" "UTF-8"))
        (is (thrown? Exception
                     (documents/move! conn id (:basis (documents/read-document conn id)) "occupied.md")))
        (is (= "existing" (markdown/decode (repo/read conn "occupied.md"))))))))

(deftest out-of-band-writes-never-masquerade-as-current-knowledge
  (with-repository
    (fn [conn]
      (let [id (random-uuid)]
        (documents/create! conn {:id id :path "hello.md" :source "old"})
        (repo/write! conn "hello.md" (.getBytes "external edit" "UTF-8"))
        (is (thrown-with-msg? clojure.lang.ExceptionInfo #"projection does not match"
                             (documents/read-document conn id)))
        (is (thrown? Exception (documents/edit! conn id (markdown/revision "old") [])))))))

(deftest concurrent-writers-cannot-publish-mismatched-source-and-projection
  (with-repository
    (fn [conn]
      (let [id (random-uuid)
            ready (java.util.concurrent.CountDownLatch. 2)
            go (promise)]
        (documents/create! conn {:id id :path "race.md" :source "base"})
        (let [before (documents/read-document conn id)
              write (fn [text]
                      (future
                        (.countDown ready)
                        @go
                        (try
                          (documents/edit! conn id (:revision before) [{:from 0 :to 4 :text text}])
                          :success
                          (catch Exception _ :refused))))
              a (write "first") b (write "second")]
          (is (.await ready 10 java.util.concurrent.TimeUnit/SECONDS))
          (deliver go true)
          (is (= #{:success :refused} #{@a @b}))
          (let [after (documents/read-document conn id)]
            (is (#{"first" "second"} (:source after)))
            (is (= (:revision after) (markdown/revision (:source after))))))))))

(deftest paths-and-existing-files-are-not-silently-normalized-or-overwritten
  (with-repository
    (fn [conn]
      (doseq [path ["../outside.md" "/absolute.md" "a//b.md" "a/./b.md"
                    "a/../b.md" "a\\b.md" ".git/config" ".geschichte/repo.edn"]]
        (is (thrown? clojure.lang.ExceptionInfo
                     (documents/create! conn {:id (random-uuid) :path path :source "body"}))))
      (repo/write! conn "present.md" (.getBytes "existing" "UTF-8"))
      (is (thrown? Exception
                   (documents/create! conn {:id (random-uuid) :path "present.md" :source "replacement"})))
      (is (= "existing" (markdown/decode (repo/read conn "present.md")))))))

(deftest history-and-export-preserve-the-original-document
  (with-repository
    (fn [conn]
      (let [id (random-uuid)
            source "---\ncustom: true\n---\n# Original\n\n<!-- opaque -->\n"]
        (documents/create! conn {:id id :path "wiki/note.md" :source source})
        (repo/stage-all! conn)
        (let [commit (repo/commit! conn {:message "Initial knowledge"})
              before (documents/read-document conn id)]
          (let [start (.indexOf source "Original")]
            (documents/edit! conn id (:revision before) [{:from start :to (+ start 8) :text "Revised"}]))
          (is (= source (markdown/decode (repo/read-at conn commit "wiki/note.md")))))
        (with-repository
          (fn [destination]
            (let [exported (repo/read conn "wiki/note.md")]
              (documents/create! destination {:id id :path "wiki/note.md"
                                              :source (markdown/decode exported)})
              (is (= (seq exported) (seq (repo/read destination "wiki/note.md")))))
            (is (= id (:id (documents/read-document destination id))))))))))

(deftest raw-geschichte-checkout-is-not-yet-a-knowledge-publication-path
  ;; This is a design constraint, not a silently tolerated stale read. A raw
  ;; Geschichte checkout restores files, not arbitrary application projections.
  ;; Before registering this store, its ONE system owner must also atomically
  ;; rebuild/publish projections for checkout/merge/import. Separate adapters
  ;; over the same conn would not solve it.
  (with-repository
    (fn [conn]
      (let [id (random-uuid)]
        (documents/create! conn {:id id :path "note.md" :source "original"})
        (repo/stage-all! conn)
        (repo/commit! conn {:message "Original"})
        (repo/branch! conn "original")
        (documents/edit! conn id (markdown/revision "original") [{:from 0 :to 8 :text "changed"}])
        (repo/stage-all! conn)
        (repo/commit! conn {:message "Changed"})
        (repo/checkout! conn "original")
        (is (= "original" (markdown/decode (repo/read conn "note.md"))))
        (is (thrown-with-msg? clojure.lang.ExceptionInfo #"projection does not match"
                             (documents/read-document conn id)))))))
