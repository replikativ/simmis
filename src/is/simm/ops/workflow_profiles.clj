(ns is.simm.ops.workflow-profiles
  "Closed HOST compilation of portable workflow profile requests. Source names
   request a policy; the caller's authenticated admission supplies authority."
  (:require [dvergr.agent.environment :as environment]
            [dvergr.agent.evaluation :as evaluation]
            [dvergr.agent.roster :as roster]
            [is.simm.ops.coordinator-sandbox :as coordinator]
            [is.simm.ops.work :as work]
            [is.simm.ops.work-sandbox :as worker]))

(def supported #{nil "developer" "coordinator"})

(def coordinator-prompt
  (str "Coordinate bounded work in your isolated workspace using dvergr.agent. "
       "Read agent/delegation-policy, agent/offered and agent/balance first. "
       "Children must select exactly #{:simmis/file-worker} or #{:simmis/coordinator}, "
       "permitted tools and an explicit permitted native provider. Split your existing "
       "money/dispatch balance; nothing funds itself. Child experiments use offered "
       "completion verification only, not a claim of task correctness. "
       "Put discard settlement in each Environment's :world {:isolation :ctx :settlement :discard}; "
       "Experiment metadata does not select settlement. "
       "If simmis.benchmarks/offered is nonempty, its exact admitted benchmark can be run "
       "with simmis.benchmarks/run! and its declared candidate profile plus an explicit "
       ":resources grant from your current balance. You may revise prompts/candidates, "
       "but cannot replace its checker, dataset or fixtures. "
       "Prefer small saved Clojure forms, parse them before load-string, and retain partial work. "
       "Use simmis.results/attempt-files and simmis.results/attempt-file for certified "
       "completion outputs (base64 byte chunks), or agent/attempt-output for explicitly "
       "published data. Put final deliverables under /out. Do not publish externally or merge trunk."))

(defn recursive? [definition] (= "coordinator" (:profile definition)))

(defn compile-team
  "Compile one known source declaration into an exact candidate profile and
   portable basis. Outer benchmark evaluator/setup capabilities never enter
   the explicit completion-only scope for child work."
  [team definition]
  (when-not (contains? supported (:profile definition))
    (throw (ex-info "Choose developer or coordinator as the workflow profile"
                    {:type ::unsupported})))
  (let [recursive? (recursive? definition)
        team (if recursive? (coordinator/restrict-team team) (worker/restrict-team team))
        scope {:evaluators {(evaluation/evaluator-ref work/completion) work/completion}
               :world-setups {}}
        basis {:version 1 :declared (or (:profile definition) "developer")
               :sandbox-policy (if recursive? coordinator/policy worker/policy)
               :effects (cond-> work/effects recursive? (update :admit conj :lifecycle))}]
    (cond-> {:team team :effects (:effects basis) :basis basis}
      recursive? (assoc :delegation-policy coordinator/delegation-policy
                        :experiment-capabilities scope
                        :basis (assoc basis :delegation-policy coordinator/delegation-policy
                                      :child-verifiers (set (keys (:evaluators scope))))))))

(defn prepare
  "The same profile compiler for an ordinary bounded work submission. The
   caller may replace its outer evaluator/setup afterward; child scope stays
   fixed and independent."
  [prepared definition]
  (when-not (contains? supported (:profile definition))
    (throw (ex-info "Choose developer or coordinator as the workflow profile" {:type ::unsupported})))
  (if-not (recursive? definition)
    prepared
    (let [{:keys [team effects basis] :as compiled} (compile-team (:team prepared) definition)
          team (reduce (fn [team agent]
                         (roster/revise-agent team (:agent/id agent) {:prompt coordinator-prompt}))
                       team (roster/agents team))
          original (:environment prepared)
          verifier (:environment/verifier original)
          env (environment/make-environment
               {:id :simmis/coordinated-work :task (:environment/task original)
                :verifier {:id (:verifier/id verifier) :version (:verifier/version verifier)
                           :basis (:verifier/basis verifier)}
                :limits (:environment/limits original)
                :world (assoc (:environment/world original) :effects effects)})]
      (-> prepared
          (merge (select-keys compiled [:delegation-policy :experiment-capabilities]))
          (assoc :team team :environment env)
          (update :basis merge
                  {:candidate (roster/agent team (:agent prepared)) :environment env
                   :sandbox-policy (:sandbox-policy basis) :execution-profile basis
                   :verifiers (:child-verifiers basis)})))))
