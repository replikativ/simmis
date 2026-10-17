(ns is.simm.ops.worker-work
  "Local workers run through existing Dvergr Jobs/Runs and certified Attempts.
   The transport uses explicit human admission and independent recovery."
  (:require [datahike.api :as d]
            [dvergr.agent.environment :as environment]
            [dvergr.agent.evaluation :as evaluation]
            [dvergr.agent.roster :as roster]
            [dvergr.agent.run :as run]
            [dvergr.agent.spend :as spend]
            [dvergr.room.registry :as registry]
            [dvergr.room.store :as store]
            [dvergr.resource :as resource]
            [dvergr.substrate.geschichte :as workspace]
            [geschichte.repo :as repo]
            [hasch.core :as hasch]
            [is.simm.model.access :as access]
            [is.simm.model.work-requests :as requests]
            [is.simm.model.workers :as workers]
            [is.simm.ops.work :as work]
            [is.simm.ops.worker-credentials :as credentials]
            [is.simm.ops.worker-files :as files]
            [is.simm.ops.worker-output :as output]
            [is.simm.ops.worker-wait :as wait]
            [is.simm.ops.workflow-runs :as workflow-runs]
            [is.simm.ops.workflows :as workflows]
            [malli.core :as m]
            [org.replikativ.spindel.engine.core :as ec])
  (:import [java.util Date]))

(def admission-unit "local-worker-admission")
(def request-kinds #{:local-worker :local-worker-workflow :local-worker-benchmark})
(def units [{:symbol admission-unit :name "Local worker admissions" :precision 0}])
(def allowance-schema [:map {:closed true} [:request-id :uuid]
                       [:max-worker-admissions [:int {:min 1 :max 32}]]])
(def task-schema
  [:map {:closed true} [:request-id :uuid] [:worker :uuid]
   [:task [:string {:min 1 :max 16000}]]
   [:paths [:vector {:max 100} [:string {:min 1 :max 512}]]]
   [:limits files/limits-schema]])
(def workflow-schema
  (into workflow-runs/source-schema
        [[:request-id :uuid] [:worker :uuid] [:limits files/limits-schema]]))

(defn- refuse! [type message] (throw (ex-info message {:type type})))
(defn- checked! [schema input]
  (when-not (m/validate schema input) (refuse! ::invalid "Invalid local worker request")))
(defn- human! [conn principal room]
  (let [by (access/actor->party-uuid (:principal/id principal))]
    (when-not (and (= :human (:principal/kind principal))
                   (credentials/permitted? @conn {:simmis.worker/by by :simmis.worker/room room}))
      (refuse! ::denied "Current human room read and merge authority is required"))
    by))

(defn provision! [conn host-ctx principal room-id room input]
  (checked! allowance-schema input)
  (binding [ec/*execution-context* host-ctx] (human! conn principal room-id))
  (work/provision-prepared! conn host-ctx principal room-id room input
                            {:limits (select-keys input [:max-worker-admissions])
                             :resources {admission-unit (bigdec (:max-worker-admissions input))}
                             :units units}))

(defn- marker [id] [:simmis/local-worker-output id])

(defn- current-enrollment? [conn pinned]
  (let [current (workers/enrollment conn (:simmis.worker/id pinned))]
    (and (= (select-keys pinned [:simmis.worker/id :simmis.worker/by :simmis.worker/room
                                 :simmis.worker/generation :simmis.worker/config-id])
            (select-keys current [:simmis.worker/id :simmis.worker/by :simmis.worker/room
                                  :simmis.worker/generation :simmis.worker/config-id]))
         (credentials/permitted? @conn current))))

(defn- metadata-files [descriptors]
  (update-vals descriptors #(select-keys % [:bytes :sha256 :mode])))

(defn capture [world id config limits]
  (binding [ec/*execution-context* (:ctx world)]
    (if-let [imported (ec/get-state (marker id))]
      (let [captured (:files (files/freeze world "Capture local worker output" config
                                           (vec (keys (:files imported))) limits))
            tree (repo/worktree (:conn (workspace/current-workspace)))]
        (when-not (and (= (:files imported) (metadata-files captured))
                       (every? #(nil? (get tree %)) (:removed imported)))
          (refuse! ::inconsistent "Run output changed before capture"))
        {:files (into {} (map (fn [[path file]] [(str "/" path) file])) captured)
         :worker (assoc (dissoc imported :files) :world (:id world))})
      {:files {} :worker {:imported? false :world (:id world)}})))

(defn- make-evaluator [config limits basis]
  (evaluation/make-evaluator
   {:id :simmis/local-worker-completion :version 1 :basis basis
    :capture (fn [{world :world/room id :run-id}] (capture world id config limits))
    :observe (fn [{:keys [default] evidence :execution/evidence}]
               (merge default evidence
                      ;; Unknown remote subscription usage is not zero-priced
                      ;; native usage. The ledger records admissions separately.
                      {:spend (assoc spend/zero :priced? false)}))
    :verify (fn [_ evidence]
              (let [completed? (= :completed (get-in evidence [:trace :runs 0 :run/status]))
                    imported? (true? (get-in evidence [:worker :imported?]))
                    exited? (= 0 (get-in evidence [:worker :exit-code]))
                    ok? (and completed? imported? exited?)]
                {:checks {:completed? completed? :imported? imported? :exit-zero? exited?}
                 :reward (if ok? 1.0 0.0)
                 :metrics {:usage-measurement :unreported-local-subscription}}))}))

(defn assignment-input
  "Trusted host binding to the exact durable child/root before publication."
  [conn host-ctx control room-id request-id pinned frozen cell id]
  (let [durable (binding [ec/*execution-context* (:ctx control)]
                  (store/-load-run (:store control) (store/conversation-id control) id))
        job (:run/parent durable)]
    (when-not (and (uuid? job)
                   (binding [ec/*execution-context* host-ctx]
                     (= job (:work.request/job (requests/get-request conn request-id)))))
      (refuse! ::inconsistent "Local worker Run is not owned by its submitted Job"))
    (cond-> {:id id :worker (:simmis.worker/id pinned) :generation (:simmis.worker/generation pinned)
             :room room-id :request request-id :job job :input frozen :permitted? credentials/permitted?}
      cell (assoc :cell cell))))

(defn make-protocol [conn host-ctx control room-id request-id pinned frozen basis & [cell world-parent]]
  (evaluation/make-protocol
   {:id :simmis/local-worker :version 1 :basis basis
    :inputs (fn [_] {:input-id (:input-id frozen) :config (:config frozen)
                     :accounting :local-worker-admission-only})
    :run
    (fn [{world :room id :run/id :keys [cancelled?]}]
      (let [journal (fn [f] (binding [ec/*execution-context* host-ctx] (f)))
            assignment (assignment-input conn host-ctx control room-id request-id pinned frozen cell id)
            job (:job assignment)
            failed? (volatile! false)
            stopped? (fn [] (or @failed? (cancelled?)
                                (not (journal (fn [] (current-enrollment? conn pinned))))))
            watch (journal #(wait/watch! conn id {:cancelled? stopped?
                                                  :cancel! (fn [] (workers/withdraw! conn assignment (fn [] (Date.))))}))]
        (try
          (when cell (journal #(workers/prepare! conn assignment)))
          (when-not (and (uuid? job)
                         (journal #(= job (:work.request/job (requests/get-request conn request-id)))))
            (refuse! ::inconsistent "Local worker Run is not owned by its submitted Job"))
          ;; One idempotent admission charge BEFORE any remote publication.
          ;; Even a failed publication consumes this admission attempt; it is
          ;; not an attestation of provider calls, USD or actual CLI startup.
          (binding [ec/*execution-context* (:ctx control)]
            (resource/consume! control id {:id (hasch/uuid [:simmis/local-worker-admission id])
                                           :resources {admission-unit 1M}}))
          (when (stopped?) (vreset! failed? true))
          (when-not @failed?
            (try (journal #(workers/enqueue! conn assignment))
                 (catch Throwable _ (vreset! failed? true))))
          ;; Ambiguous publication MUST resolve via the same-intent closing
          ;; tombstone and actual shutdown, not an absence read/finally-close.
          (let [{:keys [assignment cancelled?]} (journal #(wait/await-stop! watch))
                receipt assignment
                allowed? (fn []
                           (and (not @failed?) (not (stopped?))
                                (binding [ec/*execution-context* (:ctx (or world-parent control))]
                                  (identical? world (registry/lookup (:id world))))
                                (binding [ec/*execution-context* (:ctx control)]
                                  (some #(and (= id (:run/id %)) (= job (:run/parent %))
                                                   (= :running (:run/status %)))
                                             (run/active-runs (:id control))))
                                (journal
                                 #(let [current (workers/assignment conn id)]
                                    (and (= (:simmis.worker.assignment/intent-id receipt)
                                            (:simmis.worker.assignment/intent-id current))
                                         (= (:simmis.worker.assignment/output-id receipt)
                                            (:simmis.worker.assignment/output-id current))
                                         (= :completed (:simmis.worker.assignment/state current))
                                         (:simmis.worker.assignment/stop-ack-at current)
                                         (nil? (:simmis.worker.assignment/cancel-at current))
                                         (current-enrollment? conn pinned))))))]
            (when (or cancelled? (not (allowed?)))
              (refuse! ::cancelled "Local worker stopped without import authority"))
            (let [original (journal #(workers/payload conn receipt :simmis.worker.assignment/input-ref))
                  result (journal #(workers/payload conn receipt :simmis.worker.assignment/output-ref))]
              (when-not (= frozen original) (refuse! ::inconsistent "Assignment input identity changed"))
              (let [imported (output/import! world id original result allowed?)
                    metadata {:imported? true :worker (:simmis.worker/id pinned)
                              :output-id (:simmis.worker.assignment/output-id receipt)
                              :exit-code (:exit-code result)
                              :files (into {} (map (fn [[path file]] [(subs path 1) (select-keys file [:bytes :sha256 :mode])]))
                                           (:files imported))
                              :removed (mapv #(subs % 1) (:removed imported))}]
                (binding [ec/*execution-context* (:ctx world)]
                  (ec/swap-state! (marker id) (constantly metadata)))
                (when-not (allowed?) (refuse! ::cancelled "Worker import authority was revoked"))
                (when-not (zero? (:exit-code result)) (refuse! ::worker-failed "Local worker exited unsuccessfully"))
                (assoc (select-keys imported [:summary :exit-code :removed :import])
                       :worker (:simmis.worker/id pinned) :output-id (:simmis.worker.assignment/output-id receipt)
                       :usage-measurement :unreported-local-subscription))))
          (finally (journal #(wait/close! watch))))))}))

(defn enrollment! [conn host-ctx room-id by input]
  (binding [ec/*execution-context* host-ctx]
    (let [row (workers/enrollment conn (:worker input))]
      (when-not (and (= by (:simmis.worker/by row)) (= room-id (:simmis.worker/room row))
                     (credentials/permitted? @conn row))
        (refuse! ::denied "Worker enrollment is not delegated by this human in this room"))
      row)))

(defn workflow-input
  "Pure reconstruction from the admitted source snapshot. Does not read the
   current room, resolve a provider, execute source or run authored checkers."
  [snapshot input config]
  (checked! workflow-schema input)
  (let [{:keys [source inputs task] :as prepared}
        (workflow-runs/prepare-source snapshot (select-keys input workflow-runs/source-fields))
        modes (into {} (map (juxt :path :mode)) (:manifest snapshot))
        prefix (str "workflows/" (:name input) "/")
        entries (merge (into {} (map (fn [[p text]] [(str prefix p) [text (get modes p)]])) source)
                       (into {} (map (fn [[p text]] [(subs p 1) [text 0x81A4]])) inputs))
        descriptors
        (into (sorted-map)
              (map (fn [[p [text mode]]] [p (files/text-file text mode)])) entries)]
    (assoc prepared :frozen (files/freeze-files task config descriptors (:limits input)))))

(defn worker-team [config]
  (roster/make-agent (roster/make-roster {:id :simmis/local-worker})
                     {:id :local-worker :program {:kind :echo}
                      :model-policy {:provider :local-worker
                                     :model (str "local-worker/" (:adapter config) "/" (:model config))}
                      :metadata {:adapter-config config :usage-measurement :unreported-local-subscription}}))

(defn- prepare-frozen [conn host-ctx room-id room request-id input pinned frozen workflow]
  (let [config (:config frozen)
        paths (vec (keys (:files frozen)))
        task (:task frozen)
        basis {:policy :simmis/local-worker-v1 :worker (:worker input)
               :generation (:simmis.worker/generation pinned) :config-id (:simmis.worker/config-id pinned)
               :input-id (:input-id frozen)}
        setup (evaluation/make-world-setup
               {:id :simmis/local-worker-inputs :version 1 :basis basis
                :prepare (fn [{world :room run-id :run/id :as context}]
                           (when workflow
                             ((:prepare (:world-setup workflow)) context)
                             ;; The shared text setup pins the subtree. Import the
                             ;; same frozen bytes once more to preserve source modes.
                             (output/import! world run-id frozen
                                             {:files (:files frozen) :summary "Pin workflow inputs" :exit-code 0}
                                             (constantly true)))
                           (when-not (= frozen (files/freeze world task config paths (:limits input)))
                             (refuse! ::conflict "Selected room files changed before Run admission"))
                           {:input-id (:input-id frozen)})})
        protocol (make-protocol conn host-ctx room room-id request-id pinned frozen basis)
        evaluator (make-evaluator config (:limits input) basis)
        grant {admission-unit 1M}
        env (environment/make-environment
             (cond-> {:id :simmis/local-worker :task task
                      :verifier {:id :simmis/local-worker-completion :version 1 :basis basis}
                      :limits {:timeout-ms (get-in input [:limits :timeout-ms]) :cancel-timeout-ms 30000 :on-timeout :verdict}
                      :world {:isolation :ctx :settlement (keyword (get-in input [:limits :settlement]))
                              :effects work/effects :resources grant :setup (evaluation/world-setup-ref setup)
                              :protocol (evaluation/protocol-ref protocol)}}
               workflow (assoc :metadata {:workflow (:workflow workflow)})))
        team (worker-team config)]
    {:request-kind (if workflow :local-worker-workflow :local-worker) :team team :agent :local-worker
     :resources grant :resource-units units :environment env :world-setup setup :protocol protocol :evaluator evaluator
     :basis (cond-> {:submission input :frozen frozen :environment env :team team :policy basis}
              workflow (assoc :snapshot (:snapshot workflow) :workflow (:workflow workflow)))}))

(defn- prepare [conn host-ctx room-id room by request-id input]
  (let [pinned (enrollment! conn host-ctx room-id by input)
        config (binding [ec/*execution-context* host-ctx] (workers/payload conn pinned :simmis.worker/config-ref))]
    (prepare-frozen conn host-ctx room-id room request-id input pinned
                    (files/freeze room (:task input) config (:paths input) (:limits input)) nil)))

(defn- prepare-workflow [conn host-ctx room-id room by request-id input]
  (let [pinned (enrollment! conn host-ctx room-id by input)
        config (binding [ec/*execution-context* host-ctx] (workers/payload conn pinned :simmis.worker/config-ref))
        snapshot (workflows/snapshot room (:name input))
        workflow (assoc (workflow-input snapshot input config) :snapshot snapshot)]
    (prepare-frozen conn host-ctx room-id room request-id input pinned (:frozen workflow) workflow)))

(defn- submit-prepared! [conn host-ctx principal room-id room input kind prepare]
  (binding [ec/*execution-context* host-ctx]
    (let [by (human! conn principal room-id) id (requests/request-id by room-id (:request-id input))]
      (if-let [row (requests/get-request conn id)]
        (do
          (when-not (and (= kind (:work.request/kind row))
                         (= input (get-in (requests/payload conn row) [:input :submission])))
            (refuse! ::conflict "Request identity was used for different work"))
          (work/status conn host-ctx principal room-id id))
        (work/submit-prepared! conn host-ctx principal room-id room
                              (assoc-in input [:limits :max-worker-admissions] 1)
                              (assoc (prepare conn host-ctx room-id room by id input)
                                     :execution-kind kind
                                     :submission-channel (if (:principal/mcp? principal) :mcp :host)))))))

(defn submit! [conn host-ctx principal room-id room input]
  (checked! task-schema input)
  (submit-prepared! conn host-ctx principal room-id room input :local-worker prepare))

(defn submit-workflow! [conn host-ctx principal room-id room input]
  (checked! workflow-schema input)
  (submit-prepared! conn host-ctx principal room-id room input :local-worker-workflow prepare-workflow))
