(ns is.simm.ops.benchmarks
  "Pinned room workflow experiments using Dvergr's existing runner and receipts."
  (:require [datahike.api :as d]
            [dvergr.agent.environment :as environment]
            [dvergr.agent.evaluation :as evaluation]
            [dvergr.agent.experiment.runner :as runner]
            [dvergr.agent.roster :as roster]
            [dvergr.catalog.room :as catalog]
            [dvergr.jobs :as jobs]
            [dvergr.model.api.claude-code :as claude-code]
            [dvergr.resource :as resource]
            [dvergr.room.registry :as registry]
            [dvergr.rooms :as rooms]
            [dvergr.system.db :as system-db]
            [is.simm.model.access :as access]
            [is.simm.model.work-requests :as requests]
            [is.simm.ops.checkers :as checkers]
            [is.simm.ops.cli-owner :as cli-owner]
            [is.simm.ops.benchmark-dataset :as dataset]
            [is.simm.ops.benchmark-source :as benchmark-source]
            [is.simm.ops.knowledge-inputs :as knowledge-inputs]
            [is.simm.ops.knowledge-execution-origin :as execution-origin]
            [is.simm.ops.work :as work]
            [is.simm.ops.work-sandbox :as work-sandbox]
            [is.simm.ops.workflows :as workflows]
            [malli.core :as m]
            [org.replikativ.spindel.engine.core :as ec])
  (:import [java.util.concurrent Semaphore]
           [java.util.concurrent.atomic AtomicBoolean AtomicReference]))

;; Physical installation capacity, like a process pool, NOT application state
;; inside a fork. An unresolved scoring worker keeps this permit until restart
;; or actual completion; no timeout pretends that the worker has disappeared.
(defonce ^:private checker-slot (Semaphore. 1))
(defonce ^:private pending-retirements
  ;; Host I/O capabilities retaining a physical permit after a failed close.
  ;; Durable requests remain authoritative; these callbacks never run work.
  (atom {}))

(def schema
  [:map {:closed true}
   [:request-id :uuid]
   [:name workflows/name-schema]
   [:expected-bundle :uuid]
   [:models [:vector {:min 1 :max 3} [:string {:min 1 :max 200}]]]
   [:repetitions [:int {:min 1 :max 2}]]
   [:split {:optional true} dataset/split-schema]
   [:cases {:optional true} [:vector {:min 1 :max 4} [:string {:min 1 :max 100}]]]
   [:limits work/limits-schema]])

(defn- invalid! [message]
  (throw (ex-info message {:type ::invalid})))

(def cli-limits-schema
  [:map {:closed true}
   [:max-cli-runs [:int {:min 1 :max 8}]]
   [:max-model-steps [:int {:min 1 :max 32}]]
   [:timeout-ms [:int {:min 1000 :max 600000}]]
   [:settlement [:= "discard"]]])
(def cli-schema (assoc schema (dec (count schema)) [:limits cli-limits-schema]))
(def cli-allowance-schema
  [:map {:closed true} [:request-id :uuid] [:max-cli-runs [:int {:min 1 :max 8}]]])
(def cli-units [{:symbol "cli-run-admission" :name "CLI Run admissions" :precision 0}])
(declare authorize!)

(defn provision-cli! [conn host-ctx principal room-id room input]
  (when-not (m/validate cli-allowance-schema input) (invalid! "Invalid CLI allowance"))
  (binding [ec/*execution-context* host-ctx] (authorize! conn principal room-id))
  (cli-owner/ownership!)
  (work/provision-prepared! conn host-ctx principal room-id room input
                            {:limits (select-keys input [:max-cli-runs])
                             :resources {"cli-run-admission" (bigdec (:max-cli-runs input))}
                             :units cli-units}))

(defn authorize-in!
  "Pure operator AND room read/merge gate over ONE supplied control snapshot."
  [db principal room-id]
  (let [party (access/actor->party-uuid (:principal/id principal))]
    (when-not (and party
                   (= :admin (d/q '[:find ?role . :in $ ?p
                                    :where [?e :party/id ?p] [?e :party/role ?role]] db party))
                   (access/can? db party :read {:room room-id})
                   (access/can? db party :merge {:room room-id}))
      (throw (ex-info "An installation operator with room merge authority must admit this checker revision"
                      {:type ::denied})))
    party))

(defn authorize!
  "Operator status alone never grants access to someone else's room."
  [conn principal room-id]
  (authorize-in! @conn principal room-id))

(defn- prepare*
  "Pure construction after one pinned file read. No checker/provider execution
   or room creation. All candidate/case/repetition cells share a finite parent
   allowance; each gets at most its integer share."
  [execution snapshot {:keys [name expected-bundle models repetitions cases limits] :as input}]
  (when-not (m/validate (if (= execution :cli-text) cli-schema schema) input) (invalid! "Invalid benchmark request"))
  (if (= :checkpoint-fixture execution)
    (when-not (execution-origin/validate-execution! (:files snapshot) limits)
      (invalid! "Checkpoint fixture preparation requires the explicit execution-origin policy"))
    (execution-origin/require-copied-context! (:files snapshot)))
  (let [inspection (workflows/inspect-snapshot name snapshot)]
    (when-not (:ok inspection) (invalid! "Workflow bundle is not structurally valid"))
    (when-not (= (str expected-bundle) (:bundle inspection))
      (throw (ex-info "Workflow revision changed; inspect and explicitly admit the new bundle"
                      {:type ::conflict}))))
  (let [bundle (catalog/bundle name (:files snapshot))
        definition (:definition bundle)
        selection-policy (knowledge-inputs/declaration (:files snapshot))
        all-cases (set (keys (:cases bundle)))
        selection (dataset/select-cases (:files snapshot) all-cases input)
        selected (:cases selection)
        cell-count (* (count models) repetitions (max 1 (count selected)))
        cli? (= execution :cli-text)
        dollars (if cli? 1000000 (quot (:budget-microdollars limits) cell-count))
        dispatches (if cli? 1 (quot (:max-dispatches limits) cell-count))]
    ;; A benchmark never resolves live room knowledge. Every selected case
    ;; supplies the same retained input contract as ordinary execution.
    (when selection-policy
      (doseq [case (if (seq selected) selected [nil])]
        (knowledge-inputs/from-inputs selection-policy
                                      (merge (:fixtures bundle) (get-in bundle [:cases case :fixtures])))))
    (when-not (= (count models) (count (set models))) (invalid! "Duplicate models"))
    (when-not (and (= (count selected) (count (set selected)))
                   (every? all-cases selected)
                   (or (seq all-cases) (nil? cases)))
      (invalid! "Unknown or duplicate workflow cases"))
    (when-not (and (<= 1 cell-count 8) (pos? dollars) (pos? dispatches))
      (invalid! "At most eight cells, each with positive money and dispatch allowance, are required"))
    (when (and cli? (< (:max-cli-runs limits) cell-count))
      (invalid! "Each benchmark cell needs one explicit CLI Run admission"))
    (when-not (and (= "discard" (:settlement limits))
                   (<= 1 (count (catalog/task bundle {})) 16000)
                   (contains? #{nil "developer"} (:profile definition))
                   (contains? #{nil ["/out"]} (:capture definition))
                   (not-any? #(contains? definition %)
                             [:judge :web :fetched :allowed-sources :blocked-sources]))
      (invalid! "Initial benchmarks require offline developer workflows, /out capture and discard settlement; no judge or web effects"))
    (let [setup (checkers/world-setup bundle selection-policy)
          evaluator (checkers/evaluator bundle selection-policy)
          verifier (evaluation/evaluator-ref evaluator)
          cell-limits (assoc limits :budget-microdollars dollars :max-dispatches dispatches)
          plan (catalog/experiment-plan bundle
                                        {:models models :cases selected :tier :ad-hoc
                                         :budget-dollars (/ dollars 1000000.0)
                                         :timeout-ms (:timeout-ms limits)})
          plan (update plan :team work-sandbox/restrict-team)
          _ (when-not (every? #(contains? (if cli? #{:claude-code} work/dispatch-accounted-providers)
                                          (get-in % [:agent/model-policy :provider]))
                              (roster/agents (:team plan)))
              (invalid! "Benchmark models must use accounted native providers"))
          owner (when cli? (cli-owner/ownership!))
          provider-policy (when cli? (claude-code/execution-policy))
          _ (when (and cli? (nil? (:cli-version provider-policy)))
              (invalid! "Claude Code CLI is unavailable"))
          plan (if cli?
                 (update plan :team
                         (fn [team]
                           (reduce (fn [team agent]
                                     (roster/revise-agent team (:agent/id agent)
                                                          {:program (merge (:agent/program agent)
                                                                           {:execution-mode :text :single-use? true
                                                                            :provider-policy provider-policy
                                                                            :auto-compact? false
                                                                            :max-model-steps (:max-model-steps limits)})}))
                                   team (roster/agents team))))
                 plan)
          environments
          (mapv (fn [env]
                  (environment/make-environment
                   {:id (:environment/id env) :version (:environment/version env)
                    :task (:environment/task env)
                    :verifier {:id (:verifier/id verifier) :version (:verifier/version verifier)
                               :basis (:verifier/basis verifier)}
                    :limits (merge (:environment/limits env)
                                   {:max-model-steps (:max-model-steps limits)
                                    :budget-dollars (/ dollars 1000000.0)})
                    :world (assoc (:environment/world env)
                                  :effects work/effects :resources (if cli? {"cli-run-admission" 1M}
                                                                       (work/resources cell-limits))
                                  :setup (evaluation/world-setup-ref setup))
                    :metadata (:environment/metadata env)}))
                (:environments plan))
          plan (-> plan (assoc :environments environments :parallelism 1 :resume? (not cli?))
                   (assoc-in [:capabilities :evaluator] evaluator)
                   (assoc-in [:capabilities :world-setup] setup))
          experiment (runner/experiment-def
                      (assoc plan :id (keyword (str "request-" (:request-id input)))
                             :repetitions repetitions
                             :metadata (merge (runner/runtime-identity models)
                                              (when-let [data (:metadata selection)]
                                                {:simmis/evaluation data})
                                              {:checker-policy checkers/policy
                                               :simmis/execution (if cli?
                                                                   {:adapter :claude-cli :program :llm :mode :text
                                                                    :accounting :cli-run-admission :provider-policy provider-policy :owner owner
                                                                    :limits limits}
                                                                   {:adapter :native-provider :program :llm})
                                               :worker-sandbox work-sandbox/policy
                                               :simmis/workflow {:name name
                                                                 :title (some-> (:title definition) (subs 0 (min 256 (count (:title definition)))))
                                                                 :bundle (str expected-bundle)}
                                               :operator-admitted-bundle (str expected-bundle)})))]
      (cond-> {:bundle bundle :plan (assoc plan :experiment experiment)
               :cells cell-count :cell-limits cell-limits
       ;; Canonical definitions only; no functions, room handles or checker
       ;; runtime state in the durable admission intent.
               :basis {:submission input :experiment experiment :team (:team plan)
                       :limits limits :policy checkers/policy}
               :execution-owner owner}
        cli? (assoc :request-kind :cli-benchmark :resources {"cli-run-admission" (bigdec cell-count)}
                    :resource-units cli-units)))))

(defn prepare [snapshot input] (prepare* :native snapshot input))
(defn prepare-cli [snapshot input] (prepare* :cli-text snapshot input))

(defn prepare-checkpoint-fixture
  "HOST-only pure source reconstruction route. Reuses every ordinary native
   preparation check, retaining exact source before Request claim. This result
   is not admitted by the existing adapter; no fixture, pin or Run is created."
  [snapshot input]
  (let [retained (benchmark-source/retain (:name input) (:files snapshot))]
    (assoc-in (prepare* :checkpoint-fixture snapshot input) [:basis :benchmark/source] retained)))

(defn- fixture-slug [request-id] (str "simmis-benchmark-" request-id))
(defn fixture-room-id [request-id] (keyword (fixture-slug request-id)))

(defn- fixture-room! [host-ctx request-id]
  (binding [ec/*execution-context* host-ctx]
    ;; Provision from the host, NEVER fork the author's control room (which
    ;; contains gold/checker/calibration). No user-chosen repo/URL is accepted.
    (let [id (rooms/create-room! {:slug (fixture-slug request-id)
                                  :title "Benchmark fixture" :parent-id false :ctx host-ctx})]
      (or (registry/lookup id)
          (throw (ex-info "Benchmark fixture was not provisioned" {:type ::unavailable}))))))

(defn retire-fixture! [host-ctx request-id]
  (binding [ec/*execution-context* host-ctx]
    (let [slug (fixture-slug request-id)]
      (if-let [room (registry/lookup (keyword slug))]
        (when-not (:ok? (rooms/archive-room! room))
          (throw (ex-info "Benchmark fixture retirement needs recovery" {:type ::unavailable})))
        ;; Provisioning may have persisted a row before a crash, with no live
        ;; Room to hydrate/close. The durable locator still allows retirement.
        (when-let [row (system-db/room-by-slug slug)]
          (system-db/archive-room! (:room/id row)))))))

(defn retire-request! [host-ctx id]
  (if-let [close! (get @pending-retirements id)]
    (close!)
    (retire-fixture! host-ctx id)))

(defn retirement-pending?
  "Bounded host-resource/locator check, not an execution or refund decision."
  [host-ctx id]
  (binding [ec/*execution-context* host-ctx]
    (boolean (or (contains? @pending-retirements id)
                 (registry/lookup (fixture-room-id id))
                 (when-let [row (system-db/room-by-slug (fixture-slug id))]
                   (nil? (:room/archived-at row)))))))

(defn fixture-transport
  "Fixed HOST fixture lifecycle and checker capacity for one submission.
   The world-parent accessor only observes the original live fixture; it
   never recreates one or conveys source authority. Callers retain their own
   native lifetime. No execution, accounting or configurable hooks live here."
  [host-ctx]
  (let [slot checker-slot
        held (AtomicBoolean. false)
        fixture (AtomicReference.)
        locator (AtomicReference.)
        retire! (fn retire! []
                  (locking held
                    (let [id (.get locator)]
                      (try
                        (when id (retire-fixture! host-ctx id))
                        (.set fixture nil)
                        (when (.compareAndSet held true false) (.release ^Semaphore slot))
                        (when id (swap! pending-retirements dissoc id))
                        (catch Throwable error
                          (when id (swap! pending-retirements assoc id retire!))
                          (throw error))))))]
    {:job-context! (fn [request]
                     (let [id (:work.request/id request)]
                       (.set locator id)
                       (.set fixture (fixture-room! host-ctx id))
                       (:ctx (.get fixture))))
     :world-parent (fn []
                     (let [room (.get fixture)]
                       (when-not (and room (binding [ec/*execution-context* (:ctx room)]
                                            (registry/admitted-incarnation? room)))
                         (throw (ex-info "Benchmark fixture is no longer available" {:type ::unavailable})))
                       room))
     :admit! (fn [_]
               (when-not (.tryAcquire ^Semaphore slot)
                 (throw (ex-info "Another trusted checker benchmark still owns physical capacity"
                                 {:type ::busy})))
               (.set held true))
     :settled! (fn [_] (retire!))
     :abandon! (fn [_] (retire!))}))

(defn adapter
  "One host-owned transport/capacity adapter per submission. A losing retry
   never acquires its permit. Dvergr remains the only experiment executor."
  [host-ctx control prepared principal]
  (when (contains? (:basis prepared) :benchmark/source)
    (invalid! "Checkpoint benchmark fixtures require resolved admission before dispatch"))
  (let [transport (fixture-transport host-ctx)
        context! (:job-context! transport)
        parent (:world-parent transport)]
    (merge prepared (dissoc transport :world-parent)
           {:request-kind (or (:request-kind prepared) :benchmark)
            :job-context! (fn [request]
                            (let [ctx (context! request)]
                              (when-let [ready! (:world-parent-ready! prepared)] (ready! (parent)))
                              ctx))
            :start! (fn [job-id]
                      (runner/run-in control
                                     (assoc (:plan prepared) :parent-run job-id :cleanup-group job-id
                                            :world-parent (parent) :from (:principal/actor principal))))})))

(defn retry-request
  "Metadata-only NORMAL exact-submission decision for the two fixed benchmark
   kinds. No source/probe/admission work and no authority is granted here."
  [kind conn party room-id input]
  (when-not (#{:benchmark :cli-benchmark} kind) (invalid! "Invalid benchmark kind"))
  (let [id (requests/request-id party room-id (:request-id input))]
    (if-let [row (requests/get-request conn id)]
      {:status (if (and (= kind (:work.request/kind row))
                        (= input (get-in (requests/payload conn row) [:input :submission])))
                 :retry :conflict)
       :id id}
      {:status :absent})))

(defn- submit-kind! [kind prepare-fn input-schema conn host-ctx principal room-id room input & [snapshot]]
  (when-not (m/validate input-schema input) (invalid! "Invalid benchmark request"))
  (binding [ec/*execution-context* host-ctx]
    (let [party (authorize! conn principal room-id)
          {:keys [status id]} (retry-request kind conn party room-id input)]
      (when (= :conflict status)
        (throw (ex-info "Request identity was used for different work" {:type ::conflict})))
      (if (= :retry status)
        ;; A client can observe its exact old submission even after the room's
        ;; authoring files change. It cannot silently schedule the new revision.
        (work/status conn host-ctx principal room-id id)
        (let [prepared (prepare-fn (or snapshot (workflows/snapshot room (:name input))) input)]
          (work/submit-prepared! conn host-ctx principal room-id room input
                                 (adapter host-ctx room prepared principal)))))))

(defn submit! [& args] (apply submit-kind! :benchmark prepare schema args))
(defn submit-snapshot!
  "Host-owned scheduled benchmarks reuse the same checker admission, fixture
   isolation, exact retry and cleanup, with retained rather than current source."
  [conn host-ctx principal room-id room input snapshot]
  (submit-kind! :benchmark prepare schema conn host-ctx principal room-id room input snapshot))
(defn submit-cli! [& args] (apply submit-kind! :cli-benchmark prepare-cli cli-schema args))

(defn recover-request!
  "Retry only retirement of an already physically/resource-settled request.
   An in-process failed close retains its exact permit-owning callback."
  [host-ctx row]
  (let [job (:work.request/job row)
        status (when job (jobs/status job))]
    (when (and (#{:benchmark :cli-benchmark :local-worker-benchmark} (:work.request/kind row))
               (if job
                 (and (false? (:work.request/return-pending? row)) (:settled? status)
                      (contains? #{:completed :failed :cancelled} (:status status)))
                 (= :interrupted (:work.request/state row))))
      (retire-request! host-ctx (:work.request/id row)))))

(defn recover!
  "After work/recover!, retire request-addressed fixtures even if a crash fell
   between the wallet-return write and adapter cleanup. Never retire a live
   job's fixture or one whose physical/resource cleanup is unresolved."
  [conn host-ctx]
  (binding [ec/*execution-context* host-ctx]
    (doseq [row (requests/requests conn)] (recover-request! host-ctx row))))
