(ns is.simm.documents.block-reference-test
  (:require [clojure.test :refer [deftest is]]
            [datahike.api :as d]
            [geschichte.repo :as repo]
            [is.simm.documents.block-reference :as reference]
            [is.simm.documents.files :as files]
            [is.simm.documents.markdown :as markdown]))

(def block #uuid "bbbbbbbb-bbbb-4bbb-8bbb-bbbbbbbbbbbb")
(def document #uuid "aaaaaaaa-aaaa-4aaa-8aaa-aaaaaaaaaaaa")
(defn- source [text]
  (str "---\nsimmis-id: " document "\n---\n\n"
       "<!-- simmis:block " block " -->\n\n" text "\n\n<!-- /simmis:block -->\n"))
(defn- write! [conn path text] (repo/write! conn path (markdown/source-bytes text)))
(defn- with-repository [f]
  (let [config {:store {:backend :memory :id (random-uuid)} :schema-flexibility :write
                :keep-history? false :max-string-length 0 :max-bytes-length 0 :max-tuple-string-length 0}]
    (d/create-database config)
    (let [conn (d/connect config)]
      (try (repo/init! conn {:name "Source references"}) (f conn)
           (finally (d/release conn) (d/delete-database config))))))
(defn- request [db] {:repository-id (:repository-id (reference/selection db)) :block-id block})
(defn- reason [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e (:reason (ex-data e)))))

(deftest unique-source-blocks-resolve-without-projection-transactions
  (with-repository
    (fn [conn]
      (write! conn "wiki/original.md" (source "Hello"))
      (let [db @conn before (:max-tx db) result (reference/resolve-in db (request db))]
        (is (= :resolved (:status result)))
        (is (= "wiki/original.md" (:path result)))
        (is (= document (:document-id result)))
        (is (= block (:block-id result)))
        (is (= (files/entry db (:path result)) (:entry result)))
        (is (.startsWith (subs (:text (files/read-file-in db (:path result))) (:from result) (:to result))
                         "<!-- simmis:block"))
        (is (= before (:max-tx @conn)))
        (is (not (contains? (d/schema @conn) :document/id)))))))

(deftest location-hints-never-override-global-identity-or-ambiguity
  (with-repository
    (fn [conn]
      (write! conn "wiki/old.md" (source "Moving"))
      (let [request (assoc (request @conn) :path-hint "wiki/old.md")
            original (reference/resolve-in @conn request)]
        (write! conn "renamed/new.md" (source "Moving"))
        (is (= :ambiguous (:status (reference/resolve-in @conn request))))
        (repo/remove! conn "wiki/old.md")
        (let [moved (reference/resolve-in @conn request)]
          (is (= :resolved (:status moved)))
          (is (= "renamed/new.md" (:path moved)))
          (is (= (:block-id original) (:block-id moved)))
          (is (= (:entry original) (:entry moved))))
        ;; Moving a block to another document retains its explicit identity.
        (write! conn "renamed/new.md" "# The original document remains\n")
        (write! conn "research/note.md"
                (str "<!-- simmis:block " block " -->\n\nMoved across files.\n\n<!-- /simmis:block -->\n"))
        (let [moved (reference/resolve-in @conn request)]
          (is (= :resolved (:status moved)))
          (is (= "research/note.md" (:path moved)))
          (is (not= document (:document-id moved))))))))

(deftest snapshot-descriptors-detect-relevant-changes-only
  (with-repository
    (fn [conn]
      (write! conn "wiki/note.md" (source "Before"))
      (let [db @conn selected (reference/selection db) request (request db)
            original (reference/resolve-selected db selected request)]
        (write! conn "data/large.json" "{\"unrelated\":true}")
        (is (= selected (reference/selection @conn)))
        (is (= original (reference/resolve-selected @conn selected request)))
        (write! conn "wiki/note.md" (source "After"))
        (is (= :selection-changed (reason #(reference/resolve-selected @conn selected request))))
        (is (= original (reference/resolve-selected db selected request)))
        (is (not= (:entry original) (:entry (reference/resolve-in @conn request))))))))

(deftest malformed-sources-and-unbounded-scans-do-not-produce-guessed-targets
  (with-repository
    (fn [conn]
      (write! conn "wiki/note.md" (source "Target"))
      (let [request (request @conn)]
        (write! conn "wiki/invalid.md" "<!-- simmis:block invalid -->\n")
        (is (= :invalid-source (reason #(reference/resolve-in @conn request))))
        (repo/remove! conn "wiki/invalid.md")
        (with-redefs [reference/max-files 0]
          (is (= :file-limit (reason #(reference/selection @conn)))))
        (with-redefs [reference/max-total-bytes 1]
          (is (= :byte-limit (reason #(reference/selection @conn)))))
        (with-redefs [reference/max-total-nodes 0]
          (is (= :node-limit (reason #(reference/resolve-in @conn request)))))
        (with-redefs [files/max-index-entries 0]
          (is (= :metadata-limit (reason #(reference/selection @conn)))))
        (with-redefs [files/source-bytes (fn [& _] (throw (ex-info "Bodies must not be read" {})))]
          (is (= 1 (count (:files (reference/selection @conn))))))
        (is (= :repository-changed
               (reason #(reference/resolve-in @conn (assoc request :repository-id (random-uuid))))))
        (doseq [bad [(assoc request :world :parent) (assoc request :path-hint "../secret")
                     (assoc request :block-id (str block))]]
          (is (thrown? clojure.lang.ExceptionInfo (reference/resolve-in @conn bad))))))))

(deftest missing-and-example-markers-remain-inert
  (with-repository
    (fn [conn]
      (write! conn "wiki/example.md"
              (str "```markdown\n<!-- simmis:block " block " -->\n<!-- /simmis:block -->\n```\n"))
      (write! conn "data/not-markdown.json" (source "Not a document"))
      (is (= :missing (:status (reference/resolve-in @conn (request @conn)))))
      (write! conn "wiki/actual.md" (source "```clojure\n(throw (Exception. \"never evaluate\"))\n```"))
      (is (= "wiki/actual.md" (:path (reference/resolve-in @conn (request @conn))))))))

(deftest nested-targets-and-incomplete-valid-claims-are-not-confused
  (with-repository
    (fn [conn]
      (let [parent (random-uuid)
            text (str "<!-- simmis:block " parent " -->\n\nParent.\n\n"
                      "<!-- simmis:block " block " -->\n\nChild.\n\n<!-- /simmis:block -->\n\n<!-- /simmis:block -->\n")]
        (write! conn "wiki/nested.md" text)
        (let [r (request @conn) selected (reference/selection @conn)
              result (reference/resolve-in @conn r)]
          (is (= :resolved (:status result)))
          (is (.startsWith (subs text (:from result) (:to result)) (str "<!-- simmis:block " block " -->")))
          (write! conn "wiki/duplicate.md" (source "Another valid claimant"))
          (is (= :selection-changed (reason #(reference/resolve-selected @conn selected r))))
          (is (= :ambiguous (:status (reference/resolve-in @conn r))))
          (write! conn "wiki/duplicate.md" (str "<!-- simmis:block " block " -->\nUnclosed."))
          (is (= :invalid-source (reason #(reference/resolve-in @conn r)))))))))
