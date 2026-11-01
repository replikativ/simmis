(ns is.simm.ops.coordinator
  "Explicit recursive work admission through the existing work journal and Jobs."
  (:require [dvergr.agent.environment :as environment]
            [dvergr.agent.evaluation :as evaluation]
            [dvergr.agent.roster :as roster]
            [is.simm.model.work-requests :as requests]
            [is.simm.ops.coordinator-sandbox :as sandbox]
            [is.simm.ops.coordinator-benchmark :as benchmark]
            [is.simm.ops.owned-reads :as reads]
            [is.simm.ops.owned-room-operation :as room-operation]
            [is.simm.ops.benchmarks :as benchmarks]
            [is.simm.ops.work :as work]
            [malli.core :as m]
            [org.replikativ.spindel.engine.core :as ec]))

(def schema (conj work/task-schema [:benchmark {:optional true} benchmark/schema]))

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
                            "Put discard settlement in each Environment's :world {:isolation :ctx :settlement :discard}; "
                            "Experiment metadata does not select settlement. "
                            "If simmis.benchmarks/offered is nonempty, its exact admitted benchmark can be run "
                            "with simmis.benchmarks/run! and a file-worker candidate roster plus an explicit "
                            ":resources grant from your current balance. You may revise prompts/candidates, "
                            "but cannot replace its checker, dataset or fixtures. "
                            "Prefer small saved Clojure forms, parse them before load-string, and retain partial work. "
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

(defn- submit-owned-in!
  "Explicit inspected benchmark offer. Ordinary coordinators retain their
   existing endpoint; this path requires an owned host and operator admission."
  [host principal handle input]
  (when-not (and (m/validate schema input) (:benchmark input) (reads/owned-host? host))
    (throw (ex-info "Invalid owned coordinator benchmark" {:type ::invalid})))
  (let [{:keys [control-conn execution-ctx]} (::reads/owner host)
        original (reads/preflight-host-read! host principal handle)
        room-id (:room-id original)]
    (binding [ec/*execution-context* execution-ctx]
      (let [party (benchmarks/authorize-in! (:control-db original) principal room-id)
            id (requests/request-id party room-id (:request-id input))]
        (if-let [row (requests/get-request control-conn id)]
          (do
            (when-not (and (= :coordinator (:work.request/kind row))
                           (= input (get-in (requests/payload control-conn row) [:input :submission])))
              (throw (ex-info "Request identity was used for different work"
                              {:type :is.simm.model.work-requests/conflict})))
            (work/status control-conn execution-ctx principal room-id id))
          (let [room-binding (reads/resolve-host-room! host principal handle)
                offer (room-operation/with-current!
                        host principal handle room-binding
                        #(benchmark/compile! host principal handle room-binding input))
                ;; Provider preparation and native forks must not inherit a
                ;; source-operation lease, even when creation returns a handle.
                base (prepare (dissoc input :benchmark))
                _ (room-operation/with-current!
                    host principal handle room-binding
                    #(benchmark/current! host principal handle room-binding offer))
                prepared (benchmark/attach control-conn execution-ctx principal room-id
                                           (:room room-binding) host base input offer)]
            (work/submit-prepared! control-conn execution-ctx principal room-id
                                   (:room room-binding) input prepared)))))))

(defn submit-owned! [host principal handle input]
  (if-let [with-operation (get-in host [:config :host-lifecycle :with-operation])]
    (with-operation #(submit-owned-in! host principal handle input))
    (throw (ex-info "Coordinator benchmark requires its original App owner" {:type ::invalid}))))
