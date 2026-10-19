(ns is.simm.documents.view-test
  (:require [clojure.test :refer [deftest is]]
            [datahike.api :as d]
            [geschichte.content :as content]
            [geschichte.repo :as repo]
            [is.simm.documents.markdown :as markdown]
            [is.simm.documents.repository :as documents]
            [is.simm.documents.view :as view]))

(def options {:root "wiki" :schema-revision :test-schema-v1})

(defn- with-repository [f]
  (let [cfg {:store {:backend :memory :id (random-uuid)}
             :schema-flexibility :write :keep-history? true :branch-history? true
             :max-string-length 0 :max-bytes-length 0 :max-tuple-string-length 0}]
    (d/create-database cfg)
    (let [conn (d/connect cfg)]
      (try (repo/init! conn {:name "Source selection tests"}) (f conn)
           (finally (d/release conn))))))

(defn- write! [conn path source]
  (repo/write! conn path (markdown/source-bytes source)))

(defn- paths [v]
  (set (d/q '[:find [?path ...] :where [_ :document/path ?path]] (:db v))))

(deftest immutable-projection-does-not-write-or-require-a-second-repository
  (with-repository
    (fn [conn]
      (write! conn "wiki/a.md" "# First\n")
      (write! conn "outside/ignored.md" "not selected")
      (let [tx (:max-tx @conn)
            v (view/current-view conn options)]
        (is (= :ready (:status v)))
        (is (= #{"wiki/a.md"} (paths v)))
        (is (= tx (:max-tx @conn)))
        (is (= :provisional (get-in v [:documents 0 :identity-status])))
        (is (= #{[:Heading] [:Text]} (d/q '[:find ?kind :where [_ :document.node/kind ?kind]] (:db v))))
        (is (view/current? @conn options v))
        (is (view/current? (:db v) options v) "Projection datoms do not self-invalidate")
        (write! conn "outside/ignored.md" "unrelated change")
        (is (view/current? @conn options v))
        (write! conn "wiki/a.md" "## Second\n")
        (is (not (view/current? @conn options v)))
        (is (= "# First\n" (get-in v [:documents 0 :source])))
        (is (= #{[1]} (d/q '[:find ?n :where [_ :document.node/heading-level ?n]] (:db v))))
        (let [next (view/current-view conn options)]
          (is (= "## Second\n" (get-in next [:documents 0 :source])))
          (is (= #{[2]} (d/q '[:find ?n :where [_ :document.node/heading-level ?n]] (:db next))))
          (is (not= (:id v) (:id next))))))))

(deftest checkout-and-raw-path-removal-select-the-real-files
  (with-repository
    (fn [conn]
      (write! conn "wiki/a.md" "original")
      (repo/stage-all! conn)
      (repo/commit! conn {:message "Original"})
      (repo/branch! conn "original")
      (write! conn "wiki/a.md" "changed")
      (write! conn "wiki/b.md" "new")
      (repo/stage-all! conn)
      (repo/commit! conn {:message "Changed"})
      (let [changed (view/current-view conn options)]
        (repo/checkout! conn "original")
        (is (not (view/current? @conn options changed)))
        (let [original (view/current-view conn options)]
          (is (= #{"wiki/a.md"} (paths original)))
          (is (= "original" (get-in original [:documents 0 :source])))
          ;; Raw worktree mutations need no document-specific callback.
          (d/transact conn [[:db/retractEntity [:geschichte.work/path "wiki/a.md"]]])
          (is (not (view/current? @conn options original)))
          (is (empty? (paths (view/current-view conn options)))))))))

(deftest source-change-during-preparation-is-pending-not-stale-current
  (with-repository
    (fn [conn]
      (write! conn "wiki/a.md" "before")
      (let [read-content content/read-by-id
            raced? (atom false)
            result (with-redefs [content/read-by-id
                                 (fn [& args]
                                   (when (compare-and-set! raced? false true)
                                     (write! conn "wiki/a.md" "after"))
                                   (apply read-content args))]
                     (view/current-view conn options))]
        (is @raced?)
        (is (= {:status :pending :reason :source-changed} result))
        (is (= "after" (get-in (view/current-view conn options) [:documents 0 :source])))))))

(deftest portable-identities-are-strict-and-duplicates-never-upsert-together
  (with-repository
    (fn [conn]
      (let [id (random-uuid)
            source (str "---\nsimmis-id: '" id "'\n---\nbody")]
        (write! conn "wiki/a.md" source)
        (let [v (view/current-view conn options)]
          (is (= id (get-in v [:documents 0 :id])))
          (is (= :explicit (get-in v [:documents 0 :identity-status]))))
        (write! conn "wiki/b.md" source)
        (let [v (view/current-view conn options)]
          (is (= :invalid (:status v)))
          (is (nil? (:db v)))
          (is (= #{:duplicate-document-id} (set (map :kind (:diagnostics v))))))
        (write! conn "wiki/b.md" "---\nsimmis-id: not-a-uuid\n---\nbody")
        (let [v (view/current-view conn options)]
          (is (= :invalid (:status v)))
          (is (= :invalid-document-id (-> v :diagnostics first :kind)))
          (is (nil? (:db v))))))))

(deftest codec-schema-and-root-are-part-of-the-selection
  (with-repository
    (fn [conn]
      (write! conn "wiki/a.md" "text")
      (let [v (view/current-view conn options)]
        (is (not (view/current? @conn (assoc options :schema-revision :v2) v)))
        (with-redefs [markdown/syntax-profile (assoc markdown/syntax-profile :version 2)]
          (is (not (view/current? @conn options v))))
        (doseq [root ["" "../wiki" "/wiki" "wiki/" "a\\b" ".git"]]
          (is (thrown? clojure.lang.ExceptionInfo (view/selection @conn (assoc options :root root)))))
        (is (thrown? clojure.lang.ExceptionInfo (view/selection @conn {:root "wiki"})))))))

(deftest malformed-metadata-never-becomes-an-empty-successful-kb
  (with-repository
    (fn [conn]
      (write! conn "wiki/a.md" "---\nvalue: [\n---\nbody")
      (let [v (view/current-view conn options)]
        (is (= :invalid (:status v)))
        (is (seq (:diagnostics v)))
        (is (nil? (:db v)))))))

(deftest colocated-prototype-rows-are-replaced-only-in-the-derived-db
  (with-repository
    (fn [conn]
      (documents/initialize! conn)
      (documents/create! conn {:id (random-uuid) :path "wiki/a.md" :source "# Old"})
      (write! conn "wiki/a.md" "## New")
      (let [v (view/current-view conn options)]
        (is (= #{[2]} (d/q '[:find ?n :where [_ :document.node/heading-level ?n]] (:db v))))
        (is (= #{[1]} (d/q '[:find ?n :where [_ :document.node/heading-level ?n]] @conn)))))))

(deftest explicit-file-identity-survives-a-raw-move
  (with-repository
    (fn [conn]
      (let [id (random-uuid)
            source (str "---\nsimmis-id: '" id "'\n---\n# Moved")]
        (write! conn "wiki/old.md" source)
        (let [before (view/current-view conn options)
              row (d/pull @conn '[*] [:geschichte.work/path "wiki/old.md"])]
          (d/transact conn [[:db/retractEntity (:db/id row)]
                            (-> row (dissoc :db/id) (assoc :geschichte.work/path "wiki/new.md"))])
          (is (not (view/current? @conn options before)))
          (let [after (view/current-view conn options)]
            (is (= #{"wiki/new.md"} (paths after)))
            (is (= id (get-in after [:documents 0 :id])))
            (is (= source (get-in after [:documents 0 :source])))))))))

(deftest resource-and-file-mode-refusals-occur-before-unbounded-content-work
  (with-repository
    (fn [conn]
      (write! conn "wiki/a.md" "# a")
      (let [read-content content/read-by-id
            reads (atom 0)]
        (with-redefs [content/read-by-id (fn [& args] (swap! reads inc) (apply read-content args))]
          (d/transact conn [{:geschichte.work/path "wiki/a.md" :geschichte.work/mode 40960}])
          (is (thrown? clojure.lang.ExceptionInfo (view/current-view conn options)))
          (is (zero? @reads))
          (d/transact conn [{:geschichte.work/path "wiki/a.md"
                             :geschichte.work/mode (long repo/default-mode)
                             :geschichte.work/size (inc markdown/max-source-bytes)}])
          (is (thrown? clojure.lang.ExceptionInfo (view/current-view conn options)))
          (is (zero? @reads)))))))

(deftest aggregate-node-refusal-stops-before-parsing-later-files
  (with-repository
    (fn [conn]
      (write! conn "wiki/a.md" "# a")
      (write! conn "wiki/b.md" "# b")
      (let [parse markdown/parse
            parses (atom 0)]
        (with-redefs-fn {(ns-resolve 'is.simm.documents.view 'max-total-nodes) 1
                        #'markdown/parse (fn [source] (swap! parses inc) (parse source))}
          #(is (thrown? clojure.lang.ExceptionInfo (view/current-view conn options))))
        (is (= 1 @parses))))))

(deftest authored-facts-remain-queryable-and-references-to-disposable-rows-are-refused
  (with-repository
    (fn [conn]
      (documents/initialize! conn)
      (d/transact conn [{:db/ident :test/property :db/valueType :db.type/string
                         :db/cardinality :db.cardinality/one}
                        {:db/ident :test/ref :db/valueType :db.type/ref
                         :db/cardinality :db.cardinality/one}])
      (let [id (random-uuid)]
        (documents/create! conn {:id id :path "wiki/a.md" :source "# Test"})
        (d/transact conn [{:db/id "authored" :test/property "Keep me"}])
        (let [before (:max-tx @conn)
              v (view/current-view conn options)]
          (is (= #{["Keep me"]} (d/q '[:find ?x :where [_ :test/property ?x]] (:db v))))
          (is (= before (:max-tx @conn))))
        (d/transact conn [{:test/property "Referencing" :test/ref [:document/id id]}])
        (let [before (:max-tx @conn)]
          (is (thrown? clojure.lang.ExceptionInfo (view/current-view conn options)))
          (is (= before (:max-tx @conn)))
          (is (= 1 (count (d/q '[:find ?e :where [?e :test/ref _]] @conn)))))))))

(deftest restore-and-hard-reset-reselect-without-an-application-callback
  (with-repository
    (fn [conn]
      (write! conn "wiki/a.md" "original")
      (repo/stage-all! conn)
      (let [commit (repo/commit! conn {:message "Original"})]
        (write! conn "wiki/a.md" "unstaged")
        (let [unstaged (view/current-view conn options)]
          (repo/restore-paths! conn ["wiki/a.md"] {:source :head})
          (is (not (view/current? @conn options unstaged)))
          (is (= "original" (get-in (view/current-view conn options) [:documents 0 :source]))))
        (write! conn "wiki/a.md" "committed change")
        (repo/stage-all! conn)
        (repo/commit! conn {:message "Changed"})
        (let [changed (view/current-view conn options)]
          (repo/reset! conn commit {:mode :hard})
          (is (not (view/current? @conn options changed)))
          (is (= "original" (get-in (view/current-view conn options) [:documents 0 :source]))))))))

(deftest orphan-nodes-and-unexpected-component-ownership-are-not-silently-reused
  (with-repository
    (fn [conn]
      (documents/initialize! conn)
      (d/transact conn [{:document.node/kind :Paragraph :document.node/from 0
                         :document.node/to 0 :document.node/ordinal 0}])
      (is (thrown? clojure.lang.ExceptionInfo (view/current-view conn options)))))
  (with-repository
    (fn [conn]
      (documents/initialize! conn)
      (d/transact conn [{:db/ident :document/source-content :db/isComponent true}])
      (is (thrown? clojure.lang.ExceptionInfo (view/current-view conn options))))))
