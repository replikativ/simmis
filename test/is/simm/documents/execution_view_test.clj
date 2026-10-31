(ns is.simm.documents.execution-view-test
  (:require [clojure.test :refer [deftest is]]
            [dvergr.catalog.workspace :as workspace]
            [dvergr.discourse :as discourse]
            [dvergr.room.store.memory :as memory]
            [dvergr.substrate.geschichte :as geschichte]
            [geschichte.content :as content]
            [geschichte.query :as query]
            [geschichte.repo :as repo]
            [is.simm.documents.execution-view :as execution]
            [is.simm.documents.markdown :as markdown]
            [is.simm.documents.view :as view]
            [is.simm.ops.knowledge-workspace :as staging]
            [is.simm.ops.workflow-runs :as workflow-runs]
            [dvergr.agent.evaluation :as evaluation]
            [org.replikativ.spindel.engine.component :as component]
            [org.replikativ.spindel.engine.context :as context]
            [org.replikativ.spindel.engine.core :as ec]
            [org.replikativ.spindel.yggdrasil :as ygg]))

(def options {:schema-revision :execution-test-v1})

(defn- with-room [f]
  (let [room (discourse/make-room {:id (keyword (str "execution-view-" (random-uuid)))
                                   :store (memory/make)})]
    (try
      (workspace/ensure-workspace! room)
      (binding [ec/*execution-context* (:ctx room)] (f room))
      (finally (discourse/close-room! room)))))

(defn- selected-options []
  (let [{:keys [conn system]} (geschichte/current-workspace)]
    (execution/selection-options system conn options)))

(deftest only-explicit-execution-worlds-separate-staging-from-knowledge
  (with-room
    (fn [room]
      (let [source (str "---\nsimmis-id: '" (random-uuid) "'\n---\n# Company\n")
            parent (:conn (geschichte/current-workspace))]
        (repo/write! parent "wiki/Company.md" (markdown/source-bytes source))
        (repo/write! parent "workflows/notes.md" (markdown/source-bytes "Workflow notes"))
        (is (= options (selected-options)))
        (is (thrown? clojure.lang.ExceptionInfo (execution/install! room)))
        (let [fork (ygg/fork!)]
          (try
            (binding [ec/*execution-context* (:child-ctx fork)]
              (let [{:keys [conn]} (geschichte/current-workspace)]
                (doseq [path ["in/knowledge/sources/wiki/Company.md" "out/wiki/Company.md"]]
                  (repo/write! conn path (markdown/source-bytes source)))
                (repo/write! conn "in/knowledge0.md" (markdown/source-bytes "Not staging"))
                (repo/write! conn "out/wiki0.md" (markdown/source-bytes "Not output"))
                (is (= :invalid (:status (view/current-view conn options))))
                (execution/install! {:ctx (:child-ctx fork)})
                (let [selected (selected-options)
                      projected (view/current-view conn selected)]
                  (is (= :ready (:status projected)))
                  (is (= #{"wiki/Company.md" "workflows/notes.md" "in/knowledge0.md" "out/wiki0.md"}
                         (set (map :path (:documents projected)))))
                  (is (not= (view/selection @conn options) (:selection projected)))
                  (is (= source (String. ^bytes (content/read-by-id
                                                 conn (:content (get (query/worktree @conn)
                                                                     "out/wiki/Company.md"))) "UTF-8")))
                  (is (= execution/profile (execution/install! {:ctx (:child-ctx fork)}))))
                (let [child (ygg/fork!)]
                  (try
                    (binding [ec/*execution-context* (:child-ctx child)]
                      (is (= ["in/knowledge" "out/wiki"] (:exclude-prefixes (selected-options)))))
                    (finally (ygg/discard-fork! child))))))
            (is (= options (selected-options)) "Parent selection never changed")
            (finally (ygg/discard-fork! fork))))))))

(deftest immutable-role-follows-copies-but-does-not-merge-into-parent
  (let [root (context/create-execution-context)]
    (try
      (binding [ec/*execution-context* root]
        (let [fork (ygg/fork! {:systems :none})]
          (binding [ec/*execution-context* (:child-ctx fork)]
            (component/register! :execution-test (component/shared {:role execution/profile})))
          (let [[a b] (ygg/copy-fork! fork 2)]
            (try
              (binding [ec/*execution-context* (:child-ctx a)]
                (is (= {:role execution/profile} (get (component/registered) :execution-test))))
              (ygg/merge-fork! a)
              (is (not (contains? (component/registered) :execution-test)))
              (finally (ygg/discard-fork! b))))))
      (finally (context/stop-context! root)))))

(deftest actual-installed-source-role-survives-copy-and-not-parent-adoption
  (with-room
    (fn [_]
      ;; Geschichte settlement publishes commits, not an unborn main ref.
      (let [conn (:conn (geschichte/current-workspace))]
        (repo/write! conn "note.txt" (.getBytes "Committed source" "UTF-8"))
        (repo/stage-all! conn)
        (repo/commit! conn {:message "Execution-role fixture basis"}))
      (let [fork (ygg/fork!)]
        (binding [ec/*execution-context* (:child-ctx fork)]
          (execution/install! {:ctx (:child-ctx fork)}))
        (let [[a b] (ygg/copy-fork! fork 2)]
          (try
            (binding [ec/*execution-context* (:child-ctx a)]
              (is (= ["in/knowledge" "out/wiki"] (:exclude-prefixes (selected-options)))))
            (ygg/merge-fork! a)
            (is (= options (selected-options)) "Actual installed role is not adopted")
            (finally (ygg/discard-fork! b))))))))

(deftest both-knowledge-setup-paths-install-the-versioned-role-after-staging
  (doseq [kind [:benchmark :ordinary]]
    (with-room
      (fn [_]
        (let [fork (ygg/fork!)]
          (try
            (binding [ec/*execution-context* (:child-ctx fork)]
              (let [world {:ctx (:child-ctx fork)}
                    setup (case kind
                            :benchmark
                            (staging/with-frozen-staging
                              (evaluation/make-world-setup
                               {:id :execution-test/fixtures :version 1 :basis {}
                                :prepare (fn [_]
                                           (is (= options (selected-options)) "Not installed early")
                                           :seeded)}))
                            :ordinary
                            (@#'workflow-runs/setup "role-test" {:id (random-uuid)}
                                                    {"workflow.edn" "{}"} {"/in/knowledge/input.txt" "Evidence"} true nil))]
                (is (= execution/profile (get-in (evaluation/world-setup-ref setup) [:setup/basis :source-role])))
                ((:prepare setup) {:room world})
                (is (= ["in/knowledge" "out/wiki"] (:exclude-prefixes (selected-options))))))
            (finally (ygg/discard-fork! fork))))))))

(deftest mismatched-source-role-refuses-and-failed-setup-installs-nothing
  (with-room
    (fn [_]
      (let [fork (ygg/fork!)]
        (try
          (binding [ec/*execution-context* (:child-ctx fork)]
            (let [world {:ctx (:child-ctx fork)}
                  broken (staging/with-frozen-staging
                           (evaluation/make-world-setup
                            {:id :execution-test/broken :version 1 :basis {}
                             :prepare (fn [_] (throw (ex-info "Fixture failed" {})))}))]
              (is (thrown-with-msg? clojure.lang.ExceptionInfo #"Fixture failed"
                                    ((:prepare broken) {:room world})))
              (is (= options (selected-options)))
              (execution/install! world)
              (let [{:keys [system conn]} (geschichte/current-workspace)
                    role (get (component/registered) :is.simm.documents.execution-view/source-role)]
                (ec/swap-state! [:world/components :is.simm.documents.execution-view/source-role]
                                (constantly (component/shared (assoc-in role [:source :repository] (random-uuid)))))
                (is (thrown? clojure.lang.ExceptionInfo
                             (execution/selection-options system conn options))))))
          (finally (ygg/discard-fork! fork)))))))
