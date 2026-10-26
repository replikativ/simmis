(ns is.simm.ops.knowledge-changes-test
  (:require [clojure.set :as set]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [datahike.api :as d]
            [datahike.experimental.diff :as diff]
            [geschichte.content :as content]
            [geschichte.query :as query]
            [geschichte.repo :as repo]
            [is.simm.documents.markdown :as markdown]
            [is.simm.ops.knowledge-changes :as changes]
            [is.simm.ops.knowledge-inputs :as inputs]))

(defn- repository [refs? f]
  (let [config {:store {:backend :memory :id (random-uuid)} :schema-flexibility :write
                :keep-history? false :attribute-refs? refs? :commit-graph? true}]
    (d/create-database config)
    (let [conn (d/connect config)]
      (try (repo/init! conn {:name "Incremental knowledge tests"}) (f conn)
           (finally (d/release conn) (d/delete-database config))))))

(defn- write! [conn path text]
  (repo/write! conn path (markdown/source-bytes text)))

(defn- reason [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e (:reason (ex-data e)))))

(defn- selected-tree [db]
  ;; Independent full-tree metadata oracle. Production discovery must not call
  ;; this: comparing both whole inventories defeats structural delta traversal.
  (into (sorted-map)
        (keep (fn [[path metadata]]
                (let [role (cond (str/starts-with? path "inbox/") :trigger
                                 (str/starts-with? path "wiki/") :context)]
                  (when (and role (re-find #"(?i)\.(md|markdown)$" path))
                    [path (assoc metadata :role role)]))))
        (query/worktree db)))

(defn- oracle [before after]
  (let [old (selected-tree before) new (selected-tree after)]
    (into [] (keep (fn [path]
                    (let [a (get old path) b (get new path)]
                      (when (not= a b)
                        {:path path :kind (cond (nil? a) :added (nil? b) :removed :else :modified)
                         :before a :after b}))))
          (sort (set/union (set (keys old)) (set (keys new)))))))

(deftest net-deltas-match-full-tree-oracle-with-and-without-attribute-refs
  (doseq [refs? [false true]]
    (repository refs?
      (fn [conn]
        (write! conn "inbox/change.md" "Before")
        (write! conn "inbox/delete.md" "Remove")
        (write! conn "inbox/rename.md" "Move me")
        (write! conn "wiki/context.md" "Original context")
        (write! conn "inbox/binary.jpg" "Not Markdown")
        (let [before @conn]
          (write! conn "inbox/change.md" "After")
          (repo/remove! conn "inbox/delete.md")
          (write! conn "inbox/moved.md" "Move me")
          (repo/remove! conn "inbox/rename.md")
          (write! conn "inbox/new.MD" "New")
          (write! conn "wiki/context.md" "Changed context")
          (write! conn "inbox/binary.jpg" "Changed non-text asset")
          (write! conn "workflows/internal.md" "Not selected")
          (let [after @conn expected (oracle before after)
                actual (with-redefs [content/read-by-id (fn [& _] (throw (AssertionError. "No bodies")))
                                     query/worktree (fn [& _] (throw (AssertionError. "No whole-tree enumeration")))]
                         (changes/between before after inputs/default-policy))]
            (is (= expected (:changes actual)))
            (is (= :path (:order actual)))
            (is (= #{"inbox/change.md" "inbox/delete.md" "inbox/moved.md" "inbox/new.MD" "inbox/rename.md" "wiki/context.md"}
                   (set (map :path (:changes actual)))))
            (is (= [] (:changes (changes/between after after inputs/default-policy))))
            (is (= :cursor-reversed (reason #(changes/between after before inputs/default-policy))))))))))

(deftest bootstrap-inventory-is-independent-of-model-input-count-and-source-body-budget
  (repository false
    (fn [conn]
      (write! conn "seed.txt" "Same small source")
      (let [metadata (get (query/worktree @conn) "seed.txt")]
        (d/transact conn
          (mapv (fn [n] {:geschichte.work/path (format "inbox/%03d.md" n)
                         :geschichte.work/content [:geschichte.content/id (:content metadata)]
                         :geschichte.work/mode (:mode metadata) :geschichte.work/size (:size metadata)})
                (range 250)))
        (let [result (with-redefs [content/read-by-id (fn [& _] (throw (AssertionError. "No bodies")))]
                       (changes/inventory @conn inputs/default-policy))]
          (is (= 250 (count (:files result))))
          (is (= :trigger (get-in result [:files "inbox/000.md" :role])))
          (is (= :path (:order result))))
        (with-redefs [changes/limits (assoc changes/limits :max-inventory-entries 250)]
          (is (= :inventory-too-large (reason #(changes/inventory @conn inputs/default-policy)))))))))

(deftest exclusion-unrelated-metadata-and-context-changes-do-not-create-trigger-evidence
  (repository false
    (fn [conn]
      (write! conn "inbox/ignore/private.md" "Ignored")
      (write! conn "wiki/context.md" "Before")
      (let [before @conn policy (assoc inputs/default-policy :exclude-prefixes ["inbox/ignore"])]
        (write! conn "inbox/ignore/private.md" "Changed ignored")
        (d/transact conn [{:db/ident :test/cache :db/valueType :db.type/string :db/cardinality :db.cardinality/one}])
        (d/transact conn [{:test/cache "Projection-only churn"}])
        (is (= [] (:changes (changes/between before @conn policy))))
        (let [context-before @conn]
          (write! conn "wiki/context.md" "After")
          (let [result (changes/between context-before @conn policy)]
            (is (= ["wiki/context.md"] (mapv :path (:changes result))))
            (is (= [:context] (mapv #(get-in % [:after :role]) (:changes result))))))))))

(deftest bounds-and-views-refuse-before-materializing-a-diff
  (repository false
    (fn [conn]
      (write! conn "inbox/a.md" "A")
      (let [db @conn]
        (with-redefs [changes/limits (assoc changes/limits :max-snapshot-datoms 1)
                      diff/diff (fn [& _] (throw (AssertionError. "Must preflight cardinality")))]
          (is (= :snapshot-too-large (reason #(changes/between db db inputs/default-policy)))))
        (is (= :unsupported-snapshot (reason #(changes/inventory (d/filter db (fn [& _] true)) inputs/default-policy)))))
      (let [before @conn]
        (write! conn "inbox/b.md" "B")
        (with-redefs [changes/limits (assoc changes/limits :max-ancestor-commits 0)]
          (is (= :ancestry-limit (reason #(changes/between before @conn inputs/default-policy)))))))))

(deftest missing-ancestry-and-other-repositories-are-held
  (repository false
    (fn [conn]
      (write! conn "inbox/a.md" "A")
      (let [before @conn]
        (write! conn "inbox/a.md" "B")
        (write! conn "inbox/a.md" "C")
        (let [read d/commit-as-db ids #{(d/commit-id before) (d/commit-id @conn)}]
          (with-redefs [d/commit-as-db (fn [source id] (when (contains? ids id) (read source id)))]
            (is (= :ancestry-unavailable (reason #(changes/between before @conn inputs/default-policy))))))
        (repository false
          (fn [other]
            (is (= :source-changed (reason #(changes/between before @other inputs/default-policy))))))))))

(deftest unsupported-markdown-mode-is-not-silently-dropped
  (repository false
    (fn [conn]
      (write! conn "inbox/a.md" "A")
      (let [before @conn]
        (repo/write! conn "inbox/a.md" (markdown/source-bytes "A") {:mode 33261})
        (is (= :unsupported-file (reason #(changes/between before @conn inputs/default-policy))))))))

(deftest derived-uncommitted-values-cannot-be-used-as-durable-checkpoints
  (repository false
    (fn [conn]
      (write! conn "inbox/a.md" "A")
      (let [published @conn
            eid (:e (first (d/datoms published :avet :geschichte.work/path "inbox/a.md")))
            derived (:db-after (d/with published [[:db/add eid :geschichte.work/size 99]]))]
        (is (= (d/commit-id published) (d/commit-id derived)))
        (is (= :unpublished-snapshot (reason #(changes/basis derived))))
        (is (= :unpublished-snapshot (reason #(changes/between published derived inputs/default-policy))))))))

(deftest malformed-source-metadata-and-invalid-paths-hold-discovery
  (doseq [[path size expected] [["inbox/a.md" 99 :unsupported-file]
                               ["inbox/../a.md" 1 :invalid-path]]]
    (repository false
      (fn [conn]
        (write! conn "seed.txt" "A")
        (let [before @conn source (get (query/worktree before) "seed.txt")]
          (d/transact conn [{:geschichte.work/path path :geschichte.work/size size
                             :geschichte.work/mode 33188 :geschichte.work/content [:geschichte.content/id (:content source)]}])
          (let [failure (try (changes/between before @conn inputs/default-policy) nil
                             (catch clojure.lang.ExceptionInfo e e))]
            (if (= :invalid-path expected)
              (is (= :is.simm.documents.files/invalid (:type (ex-data failure))))
              (is (= expected (:reason (ex-data failure)))))))))))
