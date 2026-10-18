(ns is.simm.ops.coordinator
  "Explicit recursive work admission through the existing work journal and Jobs."
  (:require [dvergr.agent.environment :as environment]
            [dvergr.agent.evaluation :as evaluation]
            [dvergr.agent.roster :as roster]
            [is.simm.model.work-requests :as requests]
            [is.simm.ops.coordinator-sandbox :as sandbox]
            [is.simm.ops.work :as work]
            [malli.core :as m]
            [org.replikativ.spindel.engine.core :as ec]))

(defn prepare [input]
  (let [base (work/prepare input)
        team (sandbox/restrict-team (:team base))
        agent (:agent base)
        team (roster/revise-agent
              team agent
              {:prompt (str "Coordinate bounded work in your isolated workspace using dvergr.agent. "
                            "Read agent/delegation-policy, agent/offered and agent/balance first. "
                            "Children must select exactly #{:simmis/file-worker} or #{:simmis/coordinator}, "
                            "permitted tools and an explicit permitted native provider. Split your existing "
                            "money/dispatch balance; nothing funds itself. Child experiments use offered "
                            "completion verification only, not a claim of task correctness. "
                            "Use simmis.results/attempt-files and simmis.results/attempt-file for certified "
                            "completion outputs (base64 byte chunks), or agent/attempt-output for explicitly "
                            "published data. Put final deliverables under /out. Do not publish externally or merge trunk.")})
        original (:environment base)
        env (environment/make-environment
             {:id :simmis/coordinated-work :task (:environment/task original)
              :verifier {:id :simmis/work-completion :version 1 :basis "simmis-work-completion-v1"}
              :limits (:environment/limits original)
              :world (update (:environment/world original) :effects update :admit conj :lifecycle)})
        capabilities {:evaluators {(evaluation/evaluator-ref work/completion) work/completion}
                      :world-setups {}}]
    (assoc base :team team :environment env :request-kind :coordinator
           :execution-kind :workflow-coordinator
           :delegation-policy sandbox/delegation-policy :experiment-capabilities capabilities
           :basis {:submission input :candidate (roster/agent team agent) :environment env
                   :sandbox-policy sandbox/policy
                   :verifiers (set (keys (:evaluators capabilities)))})))

(defn submit! [conn host-ctx principal room-id room input]
  (when-not (m/validate work/task-schema input)
    (throw (ex-info "Invalid bounded task" {:type :is.simm.ops.work/invalid})))
  (binding [ec/*execution-context* host-ctx]
    (let [party (work/authorize! conn principal room-id)
          id (requests/request-id party room-id (:request-id input))]
      (if-let [row (requests/get-request conn id)]
        (do
          (when-not (and (= :coordinator (:work.request/kind row))
                         (= input (get-in (requests/payload conn row) [:input :submission])))
            (throw (ex-info "Request identity was used for different work"
                            {:type :is.simm.model.work-requests/conflict})))
          (work/status conn host-ctx principal room-id id))
        (work/submit-prepared! conn host-ctx principal room-id room input (prepare input))))))
