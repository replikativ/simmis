(ns is.simm.ops.work
  "Bounded work uses Dvergr Jobs, Runs, wallets and certified Attempts.
   This host admission journal is not copied into the speculative world."
  (:require [datahike.api :as d]
            [dvergr.agent.environment :as environment]
            [dvergr.agent.evaluation :as evaluation]
            [dvergr.agent.run :as run]
            [dvergr.agent.workflow :as workflow]
            [dvergr.artifact :as artifact]
            [dvergr.catalog.workspace :as workspace]
            [dvergr.jobs :as jobs]
            [dvergr.model.registry :as models]
            [dvergr.resource :as resource]
            [is.simm.model.access :as access]
            [is.simm.model.knowledge-batches :as batches]
            [is.simm.model.knowledge-schedule-batches :as scheduled]
            [is.simm.model.work-requests :as requests]
            [is.simm.model.work-request-checkpoints :as request-checkpoints]
            [is.simm.model.work-limits :as work-limits]
            [is.simm.ops.knowledge-batch-proof :as batch-proof]
            [is.simm.ops.knowledge-changes :as knowledge-changes]
            [is.simm.ops.work-request-checkpoints :as checkpoint-roots]
            [is.simm.ops.owned-reads :as reads]
            [is.simm.ops.owned-room-operation :as room-operation]
            [is.simm.ops.owned-workspace :as owned]
            [is.simm.ops.work-sandbox :as work-sandbox]
            [is.simm.ops.benchmark-notifications :as notifications]
            [is.simm.runtimes.operation-lifetime :as lifetime]
            [malli.core :as m]
            [taoensso.telemere :as log]
            [org.replikativ.spindel.engine.core :as ec]))

(def limits-schema work-limits/schema)

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

(def dispatch-accounted-providers
  "Native transports whose individual model HTTP attempts use Dvergr's
   admission gateway. Subscription dollars are zero incremental spend, not
   zero usage; dispatch, model-step and wall limits still apply. No CLI."
  #{:openai :anthropic :fireworks :codex-subscription})

(defn model-eligibility
  "Metadata-only native candidate eligibility, preserving ordinary aliases.
   Does not prepare a client, candidate, environment, reservation or dispatch."
  [model]
  (models/ensure-models-loaded!)
  (let [id (models/resolve-alias model) definition (models/get-model id)]
    (cond
      (nil? definition) {:status :unknown}
      (not (contains? dispatch-accounted-providers (:provider definition))) {:status :unsupported}
      :else {:status :eligible :policy {:provider (:provider definition) :model id}})))

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

(defn authorize-in!
  "HOST pure existing generic-work read+merge gate on one control snapshot."
  [db principal room-id]
  (let [party (access/actor->party-uuid (:principal/id principal))]
    (when-not (and party (access/can? db party :read {:room room-id})
                   (access/can? db party :merge {:room room-id}))
      (throw (ex-info "Room membership and merge authority are required for generic work"
                      {:type ::denied})))
    party))

(defn authorize!
  "Current room membership AND merge authority, including for named work.
   Adapters call this before reading source or resolving a model, and the
   canonical admission repeats it before publishing a request."
  [conn principal room-id]
  ;; Generic code retains the same privileged boundary as the eval UI.
  (authorize-in! @conn principal room-id))

(defn resources [limits]
  {resource/microdollars (bigdec (:budget-microdollars limits))
   resource/model-dispatches (bigdec (:max-dispatches limits))})

(defn prepare
  "Resolve and pin one generic native-API candidate before admission. Native
   subscriptions consume dispatch allowance without a fictional dollar charge;
   CLI subscriptions cannot supply this endpoint's dispatch accounting."
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
    (when-not (contains? dispatch-accounted-providers provider)
      (invalid! "This model does not support this endpoint's native dispatch accounting"))
    {:team team :agent id :environment env :evaluator completion
     :basis {:candidate agent :environment env :sandbox-policy work-sandbox/policy}}))

(def public-status-schema
  [:map {:closed true}
   [:id :uuid] [:client-id :uuid] [:room :uuid] [:by :uuid] [:kind :keyword]
   [:created-at inst?] [:state :keyword]
   [:job {:optional true} :uuid] [:incident {:optional true} :uuid]
   [:return-pending? {:optional true} :boolean]
   [:budget-microdollars {:optional true} :int]
   [:max-dispatches {:optional true} :int] [:max-model-steps {:optional true} :int]
   [:max-worker-admissions {:optional true} :int] [:max-cli-runs {:optional true} :int]
   [:timeout-ms {:optional true} :int] [:settlement {:optional true} :keyword]])

(defn- public-request [row]
  ;; Never expose the admission ownership token or internal store reference.
  (into {} (map (fn [[k v]] [(keyword (name k)) v]))
        (select-keys row (map #(keyword "work.request" (name (first %))) (drop 2 public-status-schema)))))

(defn provision-prepared!
  "Trusted host seam for an already validated allowance vector and unit
   definitions. Public adapters must validate their closed schemas first.
   Explicit room-owner allowance, never automatic minting or agent authority."
  [conn host-ctx principal room-id room input {:keys [limits resources units]}]
  (let [journal (fn [f] (binding [ec/*execution-context* host-ctx] (f)))
        party (journal #(authorize! conn principal room-id))
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

(defn- return-job-resources! [room row]
  (binding [ec/*execution-context* (:ctx room)]
    ;; Logical cancellation is not a physical cleanup acknowledgement.
    (evaluation/await-cleanups-for! room (:work.request/job row) 30000)
    (return-wallet! room (:work.request/job row) nil)))

(defn- return-job! [conn host-ctx room row]
  ;; Boot recovery has already established that no live Job/adapter remains.
  (return-job-resources! room row)
  (binding [ec/*execution-context* host-ctx]
    (requests/mark-returned! conn (:work.request/id row))))

(defn- batch-control-current!
  "Fixed native handoff observation, not a source lease across world fork.
   Writer arity uses only captured liveness and its own control snapshot."
  ([conn host-ctx principal room-id room prepared]
   (let [{:keys [host original ticket handle]} (:knowledge/owner prepared)]
     (when (.isInterrupted (Thread/currentThread)) (throw (InterruptedException. "Batch handoff interrupted")))
     (when-not (identical? ticket lifetime/*ticket*) (invalid! "Batch handoff has no original App owner"))
     (lifetime/check-current!)
     (batch-control-current! conn host-ctx principal room-id room prepared
                             (:control-db (reads/validate-host-room! host principal handle original)))))
  ([conn host-ctx principal room-id room prepared db]
   (let [{:keys [host original ticket handle]} (:knowledge/owner prepared)
         target (get-in prepared [:basis :source-origin :target])
         control (get-in prepared [:basis :knowledge/batch-control])]
     (when-not (and (map? host) (map? original) (identical? room (:room original))
                    (= room-id (:room-id original))
                    (identical? conn (get-in host [::reads/owner :control-conn]))
                    (identical? host-ctx (:execution-ctx host))
                    (lifetime/retained? ticket) (lifetime/ready? (:owner ticket))
                    (m/validate batch-proof/control-schema control))
       (invalid! "Batch handoff no longer owns its original App and Room"))
     (reads/validate-host-room-in! host principal handle original db)
     (let [party (authorize-in! db principal room-id)]
       (owned/validate-control-in! db principal room-id {:room-id room-id :party party :target target} (:mapping control))
       party))))

(defn- scheduled-handoff-current! [db prepared]
  (let [origin (get-in prepared [:basis :knowledge/batch])
        anchor (:knowledge/schedule-binding prepared)]
    (if (scheduled/bound-occurrence db (:batch origin))
      (scheduled/current! db anchor)
      (when (or anchor (contains? (:basis prepared) :knowledge/schedule)
                (contains? (:basis prepared) :schedule-origin))
        (invalid! "Batch handoff has no matching scheduled binding")))))

(defn- batch-admission-current!
  ([conn host-ctx principal room-id room prepared]
   (let [party (batch-control-current! conn host-ctx principal room-id room prepared)]
     (scheduled-handoff-current! @conn prepared)
     party))
  ([conn host-ctx principal room-id room prepared db]
   (let [party (batch-control-current! conn host-ctx principal room-id room prepared db)]
     (scheduled-handoff-current! db prepared)
     party)))

(defn- claim-batch-prepared!
  "Specific original-owner composition. Positive readback must prove BOTH
   request and batch linkage. No arbitrary claim callback or new runner."
  [conn host-ctx principal room-id room input prepared]
  (let [party (batch-admission-current! conn host-ctx principal room-id room prepared)
        intent (:basis prepared) origin (:knowledge/batch intent)
        _ (when-not (= input (:submission intent))
            (invalid! "Batch handoff submission differs from its retained intent"))
        p (requests/prepare-claim {:party party :room room-id :client-id (:request-id input)
                                   :kind (:request-kind prepared) :intent intent
                                   :limits (update (:limits input) :settlement keyword)})]
    (if-let [row (requests/inspect-claim @conn p)]
      (do (batch-proof/request! conn row) {:request row :claimed? false})
      (let [proof (batch-proof/ready! conn party room-id origin)
            _ (batch-proof/verify-input! proof intent)
            anchor (batch-proof/schedule-input! conn party room-id intent)
            _ (when-not (= anchor (:knowledge/schedule-binding prepared))
                (invalid! "Batch handoff scheduled proof changed"))
            {:keys [claim row]} (requests/claim-row p)]
        (batch-admission-current! conn host-ctx principal room-id room prepared)
        (try
          (artifact/publish-value! (artifact/datahike-store conn) (:intent p)
                                   (fn [ref]
                                     (batch-admission-current! conn host-ctx principal room-id room prepared)
                                     (d/transact conn
                                                 [[:db.fn/call (fn [db]
                                                                 (batch-admission-current! conn host-ctx principal room-id room prepared db)
                                                                 (let [row (assoc row :work.request/payload-ref ref)]
                                                                   (vec (concat (scheduled/request-tx db anchor row intent)
                                                                                (requests/claim-tx db row (:digest p))
                                                                                (batches/request-tx db origin row (:digest p))))))]])))
          (catch InterruptedException error
            (.interrupt (Thread/currentThread))
            (throw error))
          (catch Exception error
            (let [row (requests/inspect-claim @conn p)]
              (when-not row (throw error))
              (batch-proof/request! conn row))))
        (let [result (requests/claim-result @conn p claim)]
          (batch-proof/request! conn (:request result))
          result)))))

(defn- batch-handoff-current! [conn host-ctx principal room-id room prepared request]
  (let [party (batch-admission-current! conn host-ctx principal room-id room prepared)]
    (when-let [anchor (:knowledge/schedule-binding prepared)]
      ;; Unlike the pre-claim gate, native execution requires the complete
      ;; original request/batch/occurrence link, including its storage anchor.
      (scheduled/linked! @conn anchor request (:basis prepared)))
    party))

(defn- checkpoint-source-binding!
  [conn host-ctx principal room-id room prepared request]
  (batch-handoff-current! conn host-ctx principal room-id room prepared request)
  (let [selected (get-in prepared [:basis :knowledge/checkpoint])
        source-binding (binding [ec/*execution-context* host-ctx]
                         (owned/resolve! @conn principal :merge room-id room))
        physical [:repository :store :backend :branch :ref :configuration]]
    (when-not (and (= (:target selected) (:target source-binding))
                   (= (select-keys (:basis selected) physical)
                      (select-keys (knowledge-changes/source-identity (:db source-binding)) physical)))
      (invalid! "Checkpoint admission no longer has its original physical source"))
    source-binding))

(defn- retain-checkpoint!
  "Short synchronous native handoff only. Never retain its App/Room/source
   handles in a world setup, Job monitor or settlement closure."
  [conn host-ctx principal room-id room prepared request]
  (when (get-in prepared [:basis :knowledge/checkpoint])
    (when-not (and (= :workflow (:request-kind prepared))
                   (contains? (:basis prepared) :knowledge/batch))
      (invalid! "Checkpoint work requires native retained-batch admission"))
    (let [{:keys [host handle original]} (:knowledge/owner prepared)]
      (room-operation/with-current!
        host principal handle original
        (fn []
          (let [source-binding (checkpoint-source-binding! conn host-ctx principal room-id room prepared request)
                planned (binding [ec/*execution-context* host-ctx]
                          (request-checkpoints/prepare! conn request))]
            (when-not (true? (:new? planned))
              (invalid! "Checkpoint admission is not the original retention winner"))
            (checkpoint-source-binding! conn host-ctx principal room-id room prepared request)
      ;; A lost native ACK fails admission. Explicit HOST recovery may inspect
      ;; this root later, but never retries this side effect or resumes the Job.
            (checkpoint-roots/create! conn (:db source-binding) planned)
            (checkpoint-source-binding! conn host-ctx principal room-id room prepared request)
            (binding [ec/*execution-context* host-ctx]
              (checkpoint-roots/acknowledge! conn (:db source-binding) (:work.request/id request)))
            (checkpoint-source-binding! conn host-ctx principal room-id room prepared request)
            nil))))))

(defn submit-prepared!
  "Trusted adapter seam for an already validated/pinned candidate. Only the
   durable insert winner schedules; retries observe, including interruption.
   Jobs own status. The job wallet exists before the evaluation's child grant."
  [conn host-ctx principal room-id room input prepared]
  (let [batch? (contains? (:basis prepared) :knowledge/batch)
        ;; Authorize original control/Room BEFORE reading retained deployment.
        party-for-proof (when batch? (batch-control-current! conn host-ctx principal room-id room prepared))
        admission-prepared (if batch?
                             (assoc prepared :knowledge/schedule-binding
                                    (batch-proof/schedule-input! conn party-for-proof room-id (:basis prepared)))
                             prepared)
        ;; App/Room ownership is only a native admission handoff. Do not retain
        ;; that host/ticket graph in the Job's settlement closure or world data.
        prepared (dissoc admission-prepared :knowledge/owner :knowledge/schedule-binding)
        _ (when (contains? (:basis prepared) :knowledge/batch)
            (batch-admission-current! conn host-ctx principal room-id room admission-prepared))
        journal (fn [f] (binding [ec/*execution-context* host-ctx] (f)))
        party (journal #(authorize! conn principal room-id))
        construction-job (volatile! nil)
        ;; Retain only this native installation owner, never an App/lease or
        ;; the same-thread call-detached handoff. Completion may enter a NEW
        ;; short operation on this owner; a fenced/replaced App refuses it.
        notification-owner (when (contains? notifications/kinds (:request-kind prepared))
                             (:owner lifetime/*ticket*))
        {:keys [request claimed?]}
        (journal #(if (contains? (:basis prepared) :knowledge/batch)
                    (claim-batch-prepared! conn host-ctx principal room-id room input admission-prepared)
                    (requests/claim!
                     conn {:party party :room room-id :client-id (:request-id input)
                           :kind (or (:request-kind prepared) :task) :intent (:basis prepared)
                           :limits (update (:limits input) :settlement keyword)})))
        id (:work.request/id request)
        finish-row! (fn [job-id]
                      (let [row (journal #(requests/get-request conn id))]
                        (when-not (and (uuid? job-id) (= job-id (:id @construction-job))
                                       row (= (:work.request/claim request) (:work.request/claim row))
                                       (= (:work.request/intent-id request) (:work.request/intent-id row)))
                          (invalid! "Job cleanup no longer has its canonical Request"))
                        (cond
                          (= job-id (:work.request/job row)) row
                          (and (nil? (:work.request/job row))
                               (not (:linked? @construction-job))
                               (contains? #{:admitting :interrupted} (:work.request/state row))) nil
                          :else (invalid! "Job cleanup has a different Request pointer"))))]
    (when claimed?
      (try
        (when (contains? (:basis prepared) :knowledge/batch)
          (batch-handoff-current! conn host-ctx principal room-id room admission-prepared request))
        (let [job-ctx (if-let [context! (:job-context! prepared)]
                        (context! request) (:ctx room))]
          (lifetime/call-detached
           (fn [within-operation!]
             (jobs/start!
              room {:kind (if (#{:benchmark :cli-benchmark :local-worker-benchmark} (:request-kind prepared)) :experiment :workflow)
                    :ctx job-ctx :actor (:principal/actor principal)}
              (fn [job-id]
                (vreset! construction-job {:id job-id :linked? false})
                (let [row (within-operation! #(journal (fn [] (requests/record-job! conn id (:work.request/claim request) job-id))))]
                  (vswap! construction-job assoc :linked? true)
                  (try
                    (when (contains? (:basis prepared) :knowledge/batch)
                      (within-operation! #(batch-handoff-current! conn host-ctx principal room-id room admission-prepared request)))
                    (when (get-in prepared [:basis :knowledge/checkpoint])
                      (within-operation! #(retain-checkpoint! conn host-ctx principal room-id room admission-prepared row)))
                    (binding [ec/*execution-context* (:ctx room)]
                      (when-let [admit! (:admit! prepared)] (within-operation! #(admit! job-id)))
                      (doseq [unit (or (:resource-units prepared)
                                       [{:symbol resource/model-dispatches :name "Model dispatches" :precision 0}])]
                        (resource/install-unit! room unit))
                      (resource/allocate-run! room job-id nil (or (:resources prepared) (resources (:limits input))))
                      (if-let [start! (:start! prepared)]
                        (start! job-id)
                        (evaluation/evaluate room (:team prepared) (:agent prepared)
                                             (:environment prepared) (:evaluator prepared)
                                             (merge (select-keys prepared [:protocol :world-setup :experiment-capabilities :delegation-policy])
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
                           (catch Throwable cleanup (.addSuppressed e cleanup)))
                      (throw e)))))
              :before-finish (fn [job-id _ _]
                               (when-let [row (finish-row! job-id)]
                                 (return-job-resources! room row)))
              :settled (fn [status value]
                         (let [job-id (:id @construction-job)
                               row (finish-row! job-id)
                               original (assoc request :work.request/job job-id)]
                           ;; Preserve exact result IDs under the Job's native
                           ;; owner before App admission or optional formatting.
                           (when (and row (contains? notifications/kinds (:request-kind prepared)))
                             (journal #(notifications/record-result!
                                        conn original room status value)))
                           ;; Adapter retirement may close job-ctx. Dvergr
                           ;; retains the Job lease and uses control ctx here.
                           (when-let [settled! (:settled! prepared)] (settled! job-id))
                           (when row
                             (journal #(requests/mark-returned! conn id)))
                           (when (and row notification-owner)
                             ;; Reporting cannot hold Job cleanup or change a
                             ;; completed outcome. A reserved failed post can
                             ;; be explicitly retried without running work.
                             (try
                               (lifetime/with-operation!
                                notification-owner
                                #(journal (fn []
                                            (notifications/complete! conn principal room-id room original))))
                               (catch Throwable error
                                 (when (or (instance? InterruptedException error)
                                           (.isInterrupted (Thread/currentThread)))
                                   (.interrupt (Thread/currentThread)))
                                 (try
                                   (log/log! {:level :warn :id ::benchmark-notification-pending
                                              :data {:request id :job job-id}})
                                   (catch Throwable _ nil)))))))))))
        (catch Throwable e
          ;; Before the Jobs callback begins, this scope owns abandonment.
          ;; Once entered, Jobs owns retirement even if pointer publication
          ;; failed; never duplicate an uncertain adapter cleanup here.
          (when-not @construction-job
            (when-let [abandon! (:abandon! prepared)]
              (try (abandon! request)
                   (catch Throwable cleanup (.addSuppressed e cleanup)))))
          (journal #(requests/interrupt! conn id (random-uuid)))
          (throw e))))
    (public-request (journal #(requests/get-request conn id)))))

(defn submit! [conn host-ctx principal room-id room input]
  ;; Validate identity and authority before resolving models/provider setup.
  (when-not (m/validate task-schema input) (invalid! "Invalid bounded task"))
  (binding [ec/*execution-context* host-ctx] (authorize! conn principal room-id))
  (submit-prepared! conn host-ctx principal room-id room input (prepare input)))

(defn status [conn host-ctx principal room-id id]
  (binding [ec/*execution-context* host-ctx]
    (authorize! conn principal room-id)
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
            :when (contains? #{:task :workflow :benchmark :coordinator} (:work.request/kind row))]
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
