(ns is.simm.ops.benchmarks
  "Pinned room workflow experiments using Dvergr's existing runner and receipts."
  (:require [datahike.api :as d]
            [dvergr.agent.environment :as environment]
            [dvergr.agent.evaluation :as evaluation]
            [dvergr.agent.experiment.runner :as runner]
            [dvergr.agent.roster :as roster]
            [dvergr.catalog.room :as catalog]
            [dvergr.jobs :as jobs]
            [dvergr.room.registry :as registry]
            [dvergr.rooms :as rooms]
            [dvergr.system.db :as system-db]
            [is.simm.model.access :as access]
            [is.simm.model.work-requests :as requests]
            [is.simm.ops.checkers :as checkers]
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

(def schema
  [:map {:closed true}
   [:request-id :uuid]
   [:name workflows/name-schema]
   [:expected-bundle :uuid]
   [:models [:vector {:min 1 :max 3} [:string {:min 1 :max 200}]]]
   [:repetitions [:int {:min 1 :max 2}]]
   [:cases {:optional true} [:vector {:min 1 :max 4} [:string {:min 1 :max 100}]]]
   [:limits work/limits-schema]])

(defn- invalid! [message]
  (throw (ex-info message {:type ::invalid})))

(defn authorize!
  "Operator status alone never grants access to someone else's room."
  [conn principal room-id]
  (let [db @conn
        party (access/actor->party-uuid (:principal/id principal))]
    (when-not (and party
                   (= :admin (d/q '[:find ?role . :in $ ?p
                                    :where [?e :party/id ?p] [?e :party/role ?role]] db party))
                   (access/can? db party :read {:room room-id})
                   (access/can? db party :merge {:room room-id}))
      (throw (ex-info "An installation operator with room merge authority must admit this checker revision"
                      {:type ::denied})))
    party))

(defn prepare
  "Pure construction after one pinned file read. No checker/provider execution
   or room creation. All candidate/case/repetition cells share a finite parent
   allowance; each gets at most its integer share."
  [snapshot {:keys [name expected-bundle models repetitions cases limits] :as input}]
  (when-not (m/validate schema input) (invalid! "Invalid benchmark request"))
  (let [inspection (workflows/inspect-snapshot name snapshot)]
    (when-not (:ok inspection) (invalid! "Workflow bundle is not structurally valid"))
    (when-not (= (str expected-bundle) (:bundle inspection))
      (throw (ex-info "Workflow revision changed; inspect and explicitly admit the new bundle"
                      {:type ::conflict}))))
  (let [bundle (catalog/bundle name (:files snapshot))
        definition (:definition bundle)
        all-cases (set (keys (:cases bundle)))
        selected (or cases (vec (sort all-cases)))
        cell-count (* (count models) repetitions (max 1 (count selected)))
        dollars (quot (:budget-microdollars limits) cell-count)
        dispatches (quot (:max-dispatches limits) cell-count)]
    (when-not (= (count models) (count (set models))) (invalid! "Duplicate models"))
    (when-not (and (= (count selected) (count (set selected)))
                   (every? all-cases selected)
                   (or (seq all-cases) (nil? cases)))
      (invalid! "Unknown or duplicate workflow cases"))
    (when-not (and (<= 1 cell-count 8) (pos? dollars) (pos? dispatches))
      (invalid! "At most eight cells, each with positive money and dispatch allowance, are required"))
    (when-not (and (= "discard" (:settlement limits))
                   (<= 1 (count (catalog/task bundle {})) 16000)
                   (contains? #{nil "developer"} (:profile definition))
                   (contains? #{nil ["/out"]} (:capture definition))
                   (not-any? #(contains? definition %)
                             [:judge :web :fetched :allowed-sources :blocked-sources]))
      (invalid! "Initial benchmarks require offline developer workflows, /out capture and discard settlement; no judge or web effects"))
    (let [evaluator (checkers/evaluator bundle)
          verifier (evaluation/evaluator-ref evaluator)
          cell-limits (assoc limits :budget-microdollars dollars :max-dispatches dispatches)
          plan (catalog/experiment-plan bundle
                                        {:models models :cases selected :tier :ad-hoc
                                         :budget-dollars (/ dollars 1000000.0)
                                         :timeout-ms (:timeout-ms limits)})
          plan (update plan :team work-sandbox/restrict-team)
          _ (when-not (every? #(contains? #{:openai :anthropic :fireworks}
                                          (get-in % [:agent/model-policy :provider]))
                              (roster/agents (:team plan)))
              (invalid! "Benchmark models must use accounted native providers"))
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
                                  :effects work/effects :resources (work/resources cell-limits))
                    :metadata (:environment/metadata env)}))
                (:environments plan))
          plan (-> plan (assoc :environments environments :parallelism 1)
                   (assoc-in [:capabilities :evaluator] evaluator))
          experiment (runner/experiment-def
                      (assoc plan :id (keyword (str "request-" (:request-id input)))
                             :repetitions repetitions
                             :metadata (merge (runner/runtime-identity models)
                                              {:checker-policy checkers/policy
                                               :worker-sandbox work-sandbox/policy
                                               :operator-admitted-bundle (str expected-bundle)})))]
      {:bundle bundle :plan (assoc plan :experiment experiment)
       :cells cell-count :cell-limits cell-limits
       ;; Canonical definitions only; no functions, room handles or checker
       ;; runtime state in the durable admission intent.
       :basis {:submission input :experiment experiment :team (:team plan)
               :limits limits :policy checkers/policy}})))

(defn- fixture-slug [request-id] (str "simmis-benchmark-" request-id))

(defn- fixture-room! [host-ctx request-id]
  (binding [ec/*execution-context* host-ctx]
    ;; Provision from the host, NEVER fork the author's control room (which
    ;; contains gold/checker/calibration). No user-chosen repo/URL is accepted.
    (let [id (rooms/create-room! {:slug (fixture-slug request-id)
                                  :title "Benchmark fixture" :parent-id false :ctx host-ctx})]
      (or (registry/lookup id)
          (throw (ex-info "Benchmark fixture was not provisioned" {:type ::unavailable}))))))

(defn- retire-fixture! [host-ctx request-id]
  (binding [ec/*execution-context* host-ctx]
    (let [slug (fixture-slug request-id)]
      (if-let [room (registry/lookup (keyword slug))]
        (when-not (:ok? (rooms/archive-room! room))
          (throw (ex-info "Benchmark fixture retirement needs recovery" {:type ::unavailable})))
        ;; Provisioning may have persisted a row before a crash, with no live
        ;; Room to hydrate/close. The durable locator still allows retirement.
        (when-let [row (system-db/room-by-slug slug)]
          (system-db/archive-room! (:room/id row)))))))

(defn adapter
  "One host-owned transport/capacity adapter per submission. A losing retry
   never acquires its permit. Dvergr remains the only experiment executor."
  [host-ctx control prepared principal]
  (let [slot checker-slot
        held (AtomicBoolean. false)
        fixture (AtomicReference.)
        locator (AtomicReference.)
        retire! (fn []
                  (when-let [id (.get locator)] (retire-fixture! host-ctx id))
                  (when (.compareAndSet held true false) (.release ^Semaphore slot)))]
    (assoc prepared
           :request-kind :benchmark
           :job-context! (fn [request]
                           (let [id (:work.request/id request)]
                             (.set locator id)
                             (.set fixture (fixture-room! host-ctx id))
                             (:ctx (.get fixture))))
           :admit! (fn [_]
                     (when-not (.tryAcquire ^Semaphore slot)
                       (throw (ex-info "Another trusted checker benchmark still owns physical capacity"
                                       {:type ::busy})))
                     (.set held true))
           :start! (fn [job-id]
                     (runner/run-in control
                                    (assoc (:plan prepared) :parent-run job-id :cleanup-group job-id
                                           :world-parent (.get fixture) :from (:principal/actor principal))))
           :settled! (fn [_] (retire!))
           :abandon! (fn [_] (retire!)))))

(defn submit! [conn host-ctx principal room-id room input]
  (when-not (m/validate schema input) (invalid! "Invalid benchmark request"))
  (binding [ec/*execution-context* host-ctx]
    (let [party (authorize! conn principal room-id)
          id (requests/request-id party room-id (:request-id input))]
      (if-let [row (requests/get-request conn id)]
        ;; A client can observe its exact old submission even after the room's
        ;; authoring files change. It cannot silently schedule the new revision.
        (do (when-not (and (= :benchmark (:work.request/kind row))
                           (= input (get-in (requests/payload conn row) [:input :submission])))
              (throw (ex-info "Request identity was used for different work" {:type ::conflict})))
            (work/status conn host-ctx principal room-id id))
        (let [prepared (prepare (workflows/snapshot room (:name input)) input)]
          (work/submit-prepared! conn host-ctx principal room-id room input
                                 (adapter host-ctx room prepared principal)))))))

(defn recover!
  "After work/recover!, retire request-addressed fixtures even if a crash fell
   between the wallet-return write and adapter cleanup. Never retire a live
   job's fixture or one whose physical/resource cleanup is unresolved."
  [conn host-ctx]
  (binding [ec/*execution-context* host-ctx]
    (doseq [row (requests/requests conn)
            :when (= :benchmark (:work.request/kind row))
            :let [job (:work.request/job row)
                  status (when job (jobs/status job))]
            :when (if job
                    (and (false? (:work.request/return-pending? row)) (:settled? status)
                         (contains? #{:completed :failed :cancelled} (:status status)))
                    (= :interrupted (:work.request/state row)))]
      (retire-fixture! host-ctx (:work.request/id row)))))
