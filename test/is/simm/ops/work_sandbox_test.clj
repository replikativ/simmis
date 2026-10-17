(ns is.simm.ops.work-sandbox-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [dvergr.actors :as actors]
            [dvergr.agent.roster :as roster]
            [dvergr.agent.turn :as turn]
            [dvergr.catalog.workspace :as workspace]
            [dvergr.chat.context :as chat]
            [dvergr.orchestration.tasks :as tasks]
            [dvergr.sandbox.ns.intake :as intake]
            [is.simm.authority.world :as world]
            [is.simm.ops.work-sandbox :as worker]
            [is.simm.ops.work-test]
            [org.replikativ.spindel.engine.core :as ec]
            [sci.core :as sci]))

(deftest real-candidate-working-context-cannot-read-shared-registry-sentinels
  (world/with-world [conn w]
    (@#'is.simm.ops.work-test/with-room
     (fn [room]
       (workspace/ensure-workspace! room)
       (let [secret (str "private-ledger-" (random-uuid))
             task (tasks/create-task! conn {:actor-id :private-person :room-id :unrelated-room
                                            :content secret})
             _ (tasks/complete! conn (:id task) secret)
             _ (actors/spawn-human! conn {:id :private-person :name "Private Person"
                                          :external-refs {:email "private@example.test"}
                                          :config {:private-sentinel secret}})
             _ (worker/restrict-team (roster/make-roster {:id :fixture/install}))
             ctx (turn/new-working-ctx {:execution-ctx (:ctx room)
                                        :db-conn (:conn (:store room)) :budget-dollars 1.0
                                        :room-runtime-id (:id room) :agent-id :isolated-file-worker
                                        :sandbox-extensions #{worker/id}})
             sci-ctx (chat/sci-context-in ctx (:ctx room))
             probe (fn [source]
                     (try {:value (sci/eval-string* sci-ctx source)}
                          (catch Exception _ {:denied true})))]
         (try
           ;; Prove these are real private records, not empty fixtures.
           (is (= secret (:result (tasks/lookup conn (:id task)))))
           (is (= secret (get-in (actors/lookup conn :private-person) [:config :private-sentinel])))
           (sci/eval-string* sci-ctx "(require '[dvergr.tasks :as private-tasks :refer [lookup]])")
           (doseq [source ["(dvergr.tasks/list)"
                           "(private-tasks/list)"
                           (str "(lookup #uuid \"" (:id task) "\")")
                           "((deref (ns-resolve 'dvergr.tasks 'list)))"
                           "(require '[dvergr.tasks] :reload)"
                           (str "(dvergr.tasks/lookup #uuid \"" (:id task) "\")")
                           "(dvergr.actors/list)" "(dvergr.actors/lookup :private-person)"
                           "(dvergr.agents/list)" "(dvergr.skills/all)"
                           "(dvergr.room/list)" "(dvergr.room/forks)" "(dvergr.room/root)"
                           "(dvergr.room/get :unrelated-room)"
                           "(dvergr.room/messages :unrelated-room)"
                           "(env/get \"OPENAI_API_KEY\")"]]
             (let [result (probe source)]
               (is (:denied result) source)
               (is (not (str/includes? (pr-str result) secret)))))
           (is (false? (sci/eval-string* sci-ctx
                                         "(boolean (:backend org.replikativ.spindel.engine.core/*execution-context*))")))
           (is (= 6 (sci/eval-string* sci-ctx "(+ 1 2 3)")))
           (is (= "legitimate output"
                  (sci/eval-string* sci-ctx "(do (spit \"/out/probe.md\" \"legitimate output\") (slurp \"/out/probe.md\"))")))
           ;; Rebinding reinstalls the standard namespace surface, then must
           ;; reapply this exact AgentDef restriction before more agent code.
           (turn/rebind-working-ctx! ctx {:execution-ctx (:ctx room)
                                          :db-conn (:conn (:store room))
                                          :room-runtime-id (:id room) :agent-id :isolated-file-worker})
           (is (:denied (probe "(dvergr.tasks/list)")))
           (is (:denied (probe "(private-tasks/list)")))
           (is (false? (sci/eval-string* sci-ctx
                                         "(boolean (:backend org.replikativ.spindel.engine.core/*execution-context*))")))
           (finally (chat/release-sci-in! ctx (:ctx room)))))))))

(deftest already-open-host-mail-is-not-a-backdoor-around-offline-effects
  (world/with-world [conn w]
    (@#'is.simm.ops.work-test/with-room
     (fn [room]
       (workspace/ensure-workspace! room)
       (worker/restrict-team (roster/make-roster {:id :fixture/install}))
       (let [reads (atom 0)
             private-read (fn [& _] (swap! reads inc) "private-mail-sentinel")]
         (with-redefs-fn {#'intake/load-mail-bindings
                          (fn [] {'inbox private-read 'search private-read 'read private-read
                                  'sync! private-read 'open? (constantly true)})}
           (fn []
             (let [ctx (turn/new-working-ctx {:execution-ctx (:ctx room)
                                              :db-conn (:conn (:store room)) :budget-dollars 1.0
                                              :room-runtime-id (:id room) :agent-id :isolated-file-worker
                                              :sandbox-extensions #{worker/id}})
                   interpreter (chat/sci-context-in ctx (:ctx room))]
               (try
                 (doseq [source ["(intake.mail/inbox :account :private)"
                                 "(intake.mail/search \"secret\" :account :private)"
                                 "(intake.mail/read \"message\" :account :private)"]]
                   (is (thrown? Exception (sci/eval-string* interpreter source))))
                 (is (zero? @reads))
                 (finally (chat/release-sci-in! ctx (:ctx room))))))))))))

(deftest file-worker-policy-does-not-mutate-a-rich-sibling-interpreter
  (world/with-world [conn w]
    (@#'is.simm.ops.work-test/with-room
     (fn [room]
       (workspace/ensure-workspace! room)
       (worker/restrict-team (roster/make-roster {:id :fixture/install}))
       (let [options {:execution-ctx (:ctx room) :db-conn (:conn (:store room))
                      :budget-dollars 1.0 :room-runtime-id (:id room) :agent-id :fixture}
             rich (turn/new-working-ctx options)
             probe "(boolean (:backend org.replikativ.spindel.engine.core/*execution-context*))"]
         (try
           ;; The RESOLVED release already exposes CurrentWorld, not a raw
           ;; ExecutionContext record. Do not regress its opaque boundary.
           (is (false? (sci/eval-string* (chat/sci-context-in rich (:ctx room)) probe)))
           (is (= 3 (sci/eval-string* (chat/sci-context-in rich (:ctx room))
                                      "@(org.replikativ.spindel.spin.cps/spin (+ 1 2))")))
           (let [restricted (turn/new-working-ctx (assoc options :sandbox-extensions #{worker/id}))]
             (try
               (is (false? (sci/eval-string* (chat/sci-context-in restricted (:ctx room)) probe)))
               (is (= 3 (sci/eval-string* (chat/sci-context-in restricted (:ctx room))
                                          "@(org.replikativ.spindel.spin.cps/spin (+ 1 2))")))
               (is (false? (sci/eval-string* (chat/sci-context-in rich (:ctx room)) probe)))
               (is (= 3 (sci/eval-string* (chat/sci-context-in rich (:ctx room))
                                          "@(org.replikativ.spindel.spin.cps/spin (+ 1 2))")))
               (finally (chat/release-sci-in! restricted (:ctx room)))))
           (finally (chat/release-sci-in! rich (:ctx room)))))))))
