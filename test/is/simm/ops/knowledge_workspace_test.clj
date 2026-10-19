(ns is.simm.ops.knowledge-workspace-test
  (:require [clojure.test :refer [deftest is]]
            [datahike.api :as d]
            [dvergr.catalog.workspace :as workspace]
            [dvergr.discourse :as discourse]
            [dvergr.room.store.memory :as memory]
            [dvergr.rooms.files :as files]
            [dvergr.substrate.geschichte :as geschichte]
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
