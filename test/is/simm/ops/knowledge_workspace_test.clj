(ns is.simm.ops.knowledge-workspace-test
  (:require [clojure.test :refer [deftest is]]
            [datahike.api :as d]
            [dvergr.catalog.workspace :as workspace]
            [dvergr.discourse :as discourse]
            [dvergr.room.store.memory :as memory]
            [dvergr.rooms.files :as files]
            [dvergr.substrate.geschichte :as geschichte]
            [geschichte.content :as content]
            [geschichte.query :as query]
            [is.simm.ops.knowledge-outputs :as outputs]
            [is.simm.ops.checkers :as checkers]
            [is.simm.ops.knowledge-workspace :as staging]
            [org.replikativ.spindel.engine.core :as ec]))

(defn- with-room [f]
  (let [room (discourse/make-room {:id (keyword (str "cleanup-" (random-uuid))) :store (memory/make)})]
    (try
      (workspace/ensure-workspace! room)
      (binding [ec/*execution-context* (:ctx room)]
        (f room (:conn (geschichte/current-workspace))))
      (finally (discourse/close-room! room)))))

(defn- tree-state [room]
  (binding [ec/*execution-context* (:ctx room)]
    (let [db @(:conn (geschichte/current-workspace))
          entries (query/worktree db) reader (atom db)]
      {:entries entries
       :bytes (into (sorted-map) (map (fn [[path entry]]
                                        [path (vec (content/read-by-id reader (:content entry)))])) entries)
       :directories (mapv :v (d/datoms db :avet :geschichte.work-dir/path))})))

(deftest real-fork-cleanup-preserves-outside-endpoint-files-directories-and-parent
  (doseq [kind [:files :directories]]
    (with-room
      (fn [parent conn]
        (let [siblings (case kind
                         :files {"/in/knowledge0" "Outside input λ" "/out/wiki0" "Outside output"}
                         :directories {"/in/knowledge0/keep.md" "Outside input λ"
                                       "/out/wiki0/keep.md" "Outside output"})]
          (workspace/seed! parent (merge siblings {"/in/knowledge/stale.md" "Stale input"
                                                   "/out/wiki/stale.md" "Stale output"}))
          (let [mode-path (if (= :files kind) "in/knowledge0" "in/knowledge0/keep.md")]
            (d/transact conn [[:db/add [:geschichte.work/path mode-path] :geschichte.work/mode 33261]]))
          (when (= :directories kind)
            (d/transact conn [{:geschichte.work-dir/path "in/knowledge0"}
                              {:geschichte.work-dir/path "out/wiki0"}]))
          (let [before (tree-state parent)
                child (discourse/fork-room parent {:isolation :ctx :clone-participants? false})]
            (try
              (binding [ec/*execution-context* (:ctx child)]
                (is (true? (staging/clear-owned! child)))
                (let [after (tree-state child)
                      expected (map #(subs % 1) (keys siblings))]
                  (is (= (set expected) (set (keys (:entries after)))))
                  (doseq [path expected]
                    (is (= (get-in before [:entries path]) (get-in after [:entries path])))
                    (is (= (get-in before [:bytes path]) (get-in after [:bytes path]))))
                  (when (= :directories kind)
                    (is (every? (set (:directories after)) ["in/knowledge0" "out/wiki0"])))))
              (is (= before (tree-state parent)))
              (finally (discourse/discard child)))
            (is (= before (tree-state parent)))))))))

(deftest cleanup-counts-real-overflow-across-roots-after-each-endpoint-is-excluded
  (with-room
    (fn [room conn]
      ;; Raw metadata rows deliberately avoid automatic parent directories:
      ;; this proves the FILE bound, independently of the directory bound.
      (d/transact conn [{:geschichte.work/path "in/knowledge/a.md"}
                        {:geschichte.work/path "in/knowledge0"}
                        {:geschichte.work/path "out/wiki/a.md"}
                        {:geschichte.work/path "out/wiki/b.md"}
                        {:geschichte.work/path "out/wiki0"}])
      (with-redefs [staging/cleanup-policy (assoc staging/cleanup-policy :max-paths 2)
                    files/write! (fn [& _] (throw (AssertionError. "Overflow must refuse before writing")))]
        ;; A raw GLOBAL take3 would spend a slot on in/knowledge0 and miss
        ;; out/wiki/b.md. Per-prefix filtering must retain all three real rows.
        (is (= "Inherited knowledge staging exceeds cleanup bounds"
               (try (staging/clear-owned! room)
                    (catch clojure.lang.ExceptionInfo e (ex-message e)))))))))

(deftest exact-owned-root-is-retained-and-each-native-prefix-is-bounded
  (let [seen (atom 0)
        rows (fn rows [i]
               (lazy-seq (swap! seen inc)
                         (cons {:v (str "in/knowledge/" i)} (rows (inc i)))))]
    (with-redefs [staging/cleanup-policy (assoc staging/cleanup-policy :max-paths 2)
                  d/datoms (fn [& _] [{:v "in/knowledge"}])
                  d/index-range (fn [& _] (rows 0))]
      (is (= ["in/knowledge" "in/knowledge/0" "in/knowledge/1" "in/knowledge/2"]
             (mapv :v (@#'staging/owned-rows nil :geschichte.work/path "in/knowledge" staging/cleanup-policy))))
      (is (= 3 @seen)))))

(deftest cleanup-removes-owned-staging-but-keeps-unrelated-deliverables
  (with-room
    (fn [room conn]
      (workspace/seed! room {"/out/wiki/stale.md" "Stale"
                             "/in/knowledge/stale.md" "Old input"
                             "/out/keep.edn" "{:keep true}"})
      (let [before @conn]
        (is (true? (staging/clear-owned! room)))
        (is (empty? (:files (outputs/capture-db @conn))))
        (is (= #{"/out/keep.edn"} (set (keys (workspace/read-tree room "/out")))))
        (is (= #{"wiki/stale.md"} (set (keys (:files (outputs/capture-db before))))))))))

(deftest skipped-effects-cannot-leave-stale-output-certified
  (with-room
    (fn [room conn]
      (workspace/seed! room {"/out/wiki/stale.md" "Stale"})
      (with-redefs [files/write! (fn [& _] {:applied? false})]
        (is (thrown? clojure.lang.ExceptionInfo (staging/clear-owned! room))))
      (is (= 1 (count (:files (outputs/capture-db @conn))))))))

(deftest unsupported-empty-directories-and-large-cleanups-refuse-before-editing
  (with-room
    (fn [room conn]
      (d/transact conn [{:geschichte.work-dir/path "out/wiki/empty.md"}])
      (with-redefs [files/write! (fn [& _] (throw (AssertionError. "Unexpected write")))]
        (is (thrown? clojure.lang.ExceptionInfo (staging/clear-owned! room))))))
  (with-room
    (fn [room _]
      (workspace/seed! room {"/out/wiki/a.md" "A" "/out/wiki/b.md" "B"})
      (with-redefs [staging/cleanup-policy (assoc staging/cleanup-policy :max-paths 1)
                    files/write! (fn [& _] (throw (AssertionError. "Unexpected write")))]
        (is (thrown? clojure.lang.ExceptionInfo (staging/clear-owned! room)))))))

(deftest input-files-cannot-pretend-to-be-new-knowledge-output
  (doseq [path ["/out" "/out/wiki" "/out/wiki/a.md" "/out/./wiki/a.md" "/out//wiki/a.md"]]
    (is (thrown? clojure.lang.ExceptionInfo (staging/validate-inputs! {path "Seeded"})))
    (is (thrown? clojure.lang.ExceptionInfo
                 (checkers/world-setup {:fixtures {path "Seeded"}} {:kind :knowledge})))
    (is (thrown? clojure.lang.ExceptionInfo
                 (checkers/world-setup {:cases {"one" {:fixtures {path "Seeded"}}}} {:kind :knowledge}))))
  (is (= {"/out/result.edn" "Allowed"}
         (staging/validate-inputs! {"/out/result.edn" "Allowed"}))))
