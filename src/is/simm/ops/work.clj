(ns is.simm.ops.work
  "Bounded work uses Dvergr Jobs, Runs, wallets and certified Attempts.
   This host admission journal is not copied into the speculative world."
  (:require [dvergr.agent.environment :as environment]
            [dvergr.agent.evaluation :as evaluation]
            [dvergr.agent.run :as run]
            [dvergr.agent.workflow :as workflow]
            [dvergr.catalog.workspace :as workspace]
            [dvergr.jobs :as jobs]
            [dvergr.resource :as resource]
            [is.simm.model.access :as access]
            [is.simm.model.work-requests :as requests]
            [is.simm.ops.work-sandbox :as work-sandbox]
            [malli.core :as m]
            [org.replikativ.spindel.engine.core :as ec]))

(def limits-schema
  [:map {:closed true}
   [:budget-microdollars [:int {:min 1 :max 5000000}]]
   [:max-dispatches [:int {:min 1 :max 32}]]
   [:max-model-steps [:int {:min 1 :max 32}]]
   [:timeout-ms [:int {:min 1000 :max 600000}]]
   [:settlement [:enum "review" "discard"]]])

(def task-schema
  [:map {:closed true}
   [:request-id :uuid]
   [:task [:string {:min 1 :max 16000}]]
   [:model [:string {:min 1 :max 200}]]
   [:limits limits-schema]])

(def allowance-schema
  [:map {:closed true}
   [:request-id :uuid]
   [:budget-microdollars [:int {:min 1 :max 5000000}]]
   [:max-dispatches [:int {:min 1 :max 32}]]])

(def effects
  "Work may change its fork's files, but cannot send messages, hire children,
   install host dependencies, run native processes or access arbitrary URLs."
  {:admit #{:read :write :network :spend}
   :speculative true :allow-hosts #{} :quota-bytes 10000000})

(def completion
  (evaluation/make-evaluator
   {:id :simmis/work-completion :version 1 :basis "simmis-work-completion-v1"
    ;; Capture before discard. Outputs outside /out remain in a retained
    ;; world's review, not in this deliberately bounded result bundle.
    :capture (fn [{world :world/room}]
               {:files (workspace/read-tree world "/out"
                                            {:strict? true :max-files 100
                                             :max-bytes 2000000})})
    :observe (fn [{:keys [default] evidence :execution/evidence}]
               (assoc default :files (:files evidence)))
    :verify (fn [_ evidence]
              (let [done? (= :completed (get-in evidence [:trace :runs 0 :run/status]))]
                {:checks {:completed? done?} :reward (if done? 1.0 0.0)}))}))

(defn- invalid! [message]
  (throw (ex-info message {:type ::invalid})))

(defn- owner! [conn principal room-id]
  (let [party (access/actor->party-uuid (:principal/id principal))
        db @conn]
    ;; Generic code is the same privileged boundary as the existing eval UI.
    ;; Approved named workflows can later admit narrower capabilities.
    (when-not (and party (access/can? db party :read {:room room-id})
                   (access/can? db party :merge {:room room-id}))
      (throw (ex-info "Room membership and merge authority are required for generic work"
                      {:type ::denied})))
    party))

(defn resources [limits]
  {resource/microdollars (bigdec (:budget-microdollars limits))
   resource/model-dispatches (bigdec (:max-dispatches limits))})

(defn prepare
  "Resolve and pin one generic native-API candidate before admission. CLI
   subscriptions are not represented as having verified dollar accounting."
  [{:keys [task model limits] :as input}]
  (when-not (m/validate task-schema input) (invalid! "Invalid bounded task"))
  (let [{:keys [team ids]} (workflow/candidates
                            {:models [model] :profile "developer"
                             :budget-dollars (/ (:budget-microdollars limits) 1000000.0)
                             :prompt "Complete the task in this isolated workspace. Put deliverables under /out. You cannot publish externally or merge into the parent room."})
        team (work-sandbox/restrict-team team)
        id (first ids)
        agent (get-in team [:roster/agents id])
        ;; AgentDefs carry an exact model policy, not the client alias.
        provider (get-in agent [:agent/model-policy :provider])
        env (environment/make-environment
             {:id :simmis/bounded-work :task task
              :verifier {:id :simmis/work-completion :version 1
                         :basis "simmis-work-completion-v1"}
              :world {:isolation :ctx :settlement (keyword (:settlement limits))
                      :effects effects :resources (resources limits)}
              :limits {:timeout-ms (:timeout-ms limits) :cancel-timeout-ms 30000
                       :on-timeout :verdict
                       :max-model-steps (:max-model-steps limits)
                       :budget-dollars (/ (:budget-microdollars limits) 1000000.0)}})]
    (when-not (contains? #{:openai :anthropic :fireworks} provider)
      (invalid! "This model does not support this endpoint's native dispatch accounting"))
    {:team team :agent id :environment env :evaluator completion
     :basis {:candidate agent :environment env :sandbox-policy work-sandbox/policy}}))

(defn- public-request [row]
  ;; Never expose the admission ownership token or internal store reference.
  (into {} (map (fn [[k v]] [(keyword (name k)) v]))
        (select-keys row [:work.request/id :work.request/client-id :work.request/room
                          :work.request/by :work.request/kind :work.request/created-at
                          :work.request/state :work.request/job :work.request/incident
                          :work.request/return-pending? :work.request/budget-microdollars
                          :work.request/max-dispatches :work.request/max-model-steps
                          :work.request/max-worker-admissions
                          :work.request/timeout-ms :work.request/settlement])))

(defn provision-prepared!
  "Trusted host seam for an already validated allowance vector and unit
   definitions. Public adapters must validate their closed schemas first.
   Explicit room-owner allowance, never automatic minting or agent authority."
  [conn host-ctx principal room-id room input {:keys [limits resources units]}]
  (let [journal (fn [f] (binding [ec/*execution-context* host-ctx] (f)))
        party (journal #(owner! conn principal room-id))
        row (:request (journal #(requests/claim!
                                 conn {:party party :room room-id :client-id (:request-id input)
                                       :kind :allowance :limits limits
                                       :intent {:resources resources}})))
        id (:work.request/id row)]
    (binding [ec/*execution-context* (:ctx room)]
      (doseq [unit units] (resource/install-unit! room unit))
      (resource/mint! room {:id id :resources resources
                            :effective-date (:work.request/created-at row)
                            :posted-at (:work.request/created-at row)
                            :actor (:principal/actor principal)}))
    (public-request (journal #(requests/record-provisioned! conn id (:work.request/claim row))))))

(defn provision!
  "Explicit owner provisioning of EXECUTION ALLOWANCE, not a payment or a
   promise of prepaid funds. No agent can mint via its sandbox. The immutable
   request pins its vector and the ledger's stable receipt makes replay safe."
  [conn host-ctx principal room-id room input]
  (when-not (m/validate allowance-schema input) (invalid! "Invalid work allowance"))
  (let [limits (select-keys input [:budget-microdollars :max-dispatches])]
    (provision-prepared! conn host-ctx principal room-id room input
                         {:limits limits :resources (resources limits)
                          :units [{:symbol resource/model-dispatches :name "Model dispatches" :precision 0}]})))

(defn- return-wallet! [room id parent]
  (when-let [balance (resource/run-wallet-balance room id)]
    (when-let [unused (not-empty (into {} (filter (comp pos? val)) balance))]
      ;; Provider reservation wallets are not remaining Run balances.
      (resource/return! room id parent unused))))

(defn- return-job! [conn host-ctx room row]
  (binding [ec/*execution-context* (:ctx room)]
    ;; A cancelled evaluation Spin can finish before its detached scorer and
    ;; child cleanup. Join this job only, never another operation's cleanup.
    (evaluation/await-cleanups-for! room (:work.request/job row) 30000)
    (return-wallet! room (:work.request/job row) nil))
  (binding [ec/*execution-context* host-ctx]
    (requests/mark-returned! conn (:work.request/id row))))

(defn submit-prepared!
  "Trusted adapter seam for an already validated/pinned candidate. Only the
   durable insert winner schedules; retries observe, including interruption.
   Jobs own status. The job wallet exists before the evaluation's child grant."
  [conn host-ctx principal room-id room input prepared]
  (let [journal (fn [f] (binding [ec/*execution-context* host-ctx] (f)))
        party (journal #(owner! conn principal room-id))
        settle! (fn [row]
                  (return-job! conn host-ctx room row)
                  ;; Host transport/physical-capacity cleanup is deliberately
                  ;; AFTER the group's real completion and resource return.
                  (when-let [settled! (:settled! prepared)]
                    (settled! (:work.request/job row))))
        {:keys [request claimed?]}
        (journal #(requests/claim!
                   conn {:party party :room room-id :client-id (:request-id input)
                         :kind (or (:request-kind prepared) :task) :intent (:basis prepared)
                         :limits (update (:limits input) :settlement keyword)}))
        id (:work.request/id request)]
    (when claimed?
      (try
        (let [job-ctx (if-let [context! (:job-context! prepared)]
                        (context! request) (:ctx room))]
          (jobs/start!
           room {:kind (if (= :benchmark (:request-kind prepared)) :experiment :workflow)
                 :ctx job-ctx :actor (:principal/actor principal)}
           (fn [job-id]
             (let [row (journal #(requests/record-job! conn id (:work.request/claim request) job-id))]
               (try
                 (binding [ec/*execution-context* (:ctx room)]
                   (when-let [admit! (:admit! prepared)] (admit! job-id))
                   (doseq [unit (or (:resource-units prepared)
                                    [{:symbol resource/model-dispatches :name "Model dispatches" :precision 0}])]
                     (resource/install-unit! room unit))
                   (resource/allocate-run! room job-id nil (or (:resources prepared) (resources (:limits input))))
                   (if-let [start! (:start! prepared)]
                     (start! job-id)
                     (evaluation/evaluate room (:team prepared) (:agent prepared)
                                          (:environment prepared) (:evaluator prepared)
                                          (merge (select-keys prepared [:protocol :world-setup])
                                                 {:parent-run job-id :cleanup-group job-id
                                                  :from (:principal/actor principal)
                                                  :metrics {:requester-party party
                                                            :submission-channel (or (:submission-channel prepared) :mcp)
                                                            :execution-kind (or (:execution-kind prepared) :workflow-candidate)
                                                            :request-id id}}))))
                 (catch Throwable e
                 ;; Construction may already have admitted a child. Fence the
                 ;; parent and cancel its own descendants before joining;
                 ;; never infer quiescence from not having returned a Spin.
                   (try (binding [ec/*execution-context* (:ctx room)]
                          (run/cancel-run! job-id)
                          (doseq [child (run/runs room {:root-run-id job-id :limit 1000})
                                  :when (not= job-id (:run/id child))]
                            (run/cancel-run! (:run/id child))))
                        (settle! row)
                        (catch Throwable cleanup (.addSuppressed e cleanup)))
                   (throw e)))))
           :settled (fn [_ _]
                      (settle! (journal #(requests/get-request conn id))))))
        (catch Throwable e
          ;; A failed host provisioning/run-pointer step has admitted no
          ;; execution. Its physical resources can be retired without joining
          ;; an unknown job. A published pointer retains the normal cleanup.
          (when-not (:work.request/job (journal #(requests/get-request conn id)))
            (when-let [abandon! (:abandon! prepared)]
              (try (abandon! request)
                   (catch Throwable cleanup (.addSuppressed e cleanup)))))
          (journal #(requests/interrupt! conn id (random-uuid)))
          (throw e))))
    (public-request (journal #(requests/get-request conn id)))))

(defn submit! [conn host-ctx principal room-id room input]
  ;; Validate identity and authority before resolving models/provider setup.
  (when-not (m/validate task-schema input) (invalid! "Invalid bounded task"))
  (binding [ec/*execution-context* host-ctx] (owner! conn principal room-id))
  (submit-prepared! conn host-ctx principal room-id room input (prepare input)))

(defn status [conn host-ctx principal room-id id]
  (binding [ec/*execution-context* host-ctx]
    (owner! conn principal room-id)
    (let [row (requests/get-request conn id)]
      (when-not (= room-id (:work.request/room row))
        (throw (ex-info "No such request in this room" {:type ::missing})))
      (public-request row))))

(defn recover!
  "Boot only, AFTER Dvergr reconciles orphaned Runs. Never restarts work or
   refunds a live/unresolved job. Room lookup must resolve the durable room."
  [conn host-ctx resolve-room]
  (binding [ec/*execution-context* host-ctx]
    (doseq [row (requests/requests conn)
            :when (contains? #{:task :benchmark} (:work.request/kind row))]
      (if-let [job (:work.request/job row)]
        (when (:work.request/return-pending? row)
          (when-let [room (resolve-room (:work.request/room row))]
            (let [s (jobs/status job)
                  active (set (map :run/id (run/active-runs)))
                  descendants (binding [ec/*execution-context* (:ctx room)]
                                (run/runs room {:root-run-id job :limit 100000}))
                  terminal? #(contains? #{:completed :failed :cancelled} (:run/status %))]
              (when (and (:settled? s) (seq descendants) (< (count descendants) 100000)
                         (every? terminal? descendants)
                         (not-any? #(contains? active (:run/id %)) descendants))
                ;; A dead process could not run its child cleanup. Reconcile
                ;; remaining wallets bottom-up, never reservation exposure.
                (let [children (group-by :run/parent descendants)]
                  (letfn [(settle [id seen]
                            (when (contains? seen id)
                              (throw (ex-info "Cyclic Run ancestry" {:type ::invalid-ancestry})))
                            (doseq [child (get children id)]
                              (settle (:run/id child) (conj seen id))
                              (binding [ec/*execution-context* (:ctx room)]
                                (return-wallet! room (:run/id child) id))))]
                    (settle job #{})))
                (return-job! conn host-ctx room row)))))
        (requests/interrupt! conn (:work.request/id row) (random-uuid))))))
