(ns is.simm.ops.worker-benchmarks
  "Local SCI candidates in the existing room experiment/checker runner. Gold,
   checker and calibration remain server-side; only fixtures reach the guest."
  (:require [dvergr.agent.environment :as environment]
            [dvergr.agent.evaluation :as evaluation]
            [dvergr.agent.experiment.runner :as runner]
            [dvergr.agent.spend :as spend]
            [hasch.core :as hasch]
            [is.simm.model.work-requests :as requests]
            [is.simm.model.workers :as workers]
            [is.simm.ops.benchmarks :as benchmarks]
            [is.simm.ops.checkers :as checkers]
            [is.simm.ops.work :as work]
            [is.simm.ops.worker-files :as files]
            [is.simm.ops.worker-work :as local]
            [is.simm.ops.workflow-runs :as workflow-runs]
            [is.simm.ops.workflows :as workflows]
            [malli.core :as m]
            [org.replikativ.spindel.engine.core :as ec])
  (:import [java.util.concurrent.atomic AtomicReference]))

(def schema
  [:map {:closed true} [:request-id :uuid] [:worker :uuid]
   [:name workflows/name-schema] [:expected-bundle :uuid]
   [:repetitions [:int {:min 1 :max 2}]]
   [:cases {:optional true} [:vector {:min 1 :max 4} [:string {:min 1 :max 100}]]]
   [:limits files/limits-schema]])

(def policy {:version 1 :max-cases 4 :max-cells 8 :parallelism 1 :retry :explicit-new-request
             :source :clean-fixtures :usage :unreported-local-subscription})

(defn- refuse! [message] (throw (ex-info message {:type ::invalid})))

(defn- evaluator [bundle config limits basis]
  (let [checker (checkers/evaluator bundle)]
    (evaluation/make-evaluator
     {:id :simmis/local-worker-benchmark :version 1 :tier (evaluation/evaluator-tier checker)
      :basis {:worker basis :checker (evaluation/evaluator-ref checker)}
      :capture (fn [{world :world/room id :run-id :as context}]
                 (let [worker (local/capture world id config limits)]
                   (assoc ((:capture checker) context) :worker (:worker worker)
                          :worker-files (:files worker))))
      :observe (fn [{captured :execution/evidence :as context}]
                 (assoc ((:observe checker) context)
                        :worker (:worker captured) :worker-files (:worker-files captured)
                        :spend (assoc spend/zero :priced? false)))
      :verify (fn [environment evidence]
                (let [imported? (true? (get-in evidence [:worker :imported?]))
                      exited? (= 0 (get-in evidence [:worker :exit-code]))
                      ready? (and imported? exited? (= :completed (:run-status evidence)))
                      verdict (if ready? ((:verify checker) environment evidence)
                                  {:checks {:completed? false} :reward 0.0})]
                  (-> verdict
                      (update :checks assoc :imported? imported? :exit-zero? exited?)
                      (assoc :metrics {:usage-measurement :unreported-local-subscription}))))})))

(defn prepare
  "Pure pinned definitions and capabilities. No checker/provider execution,
   room creation or publication. Runtime binds each cell before its setup."
  [snapshot {:keys [name expected-bundle repetitions cases limits] :as input} config pinned]
  (when-not (and (m/validate schema input) (= "discard" (:settlement limits)))
    (refuse! "Local benchmarks require finite discard-only limits"))
  (let [inspection (workflows/inspect-snapshot name snapshot)
        available (set (:cases inspection))
        selected (if (seq available) (or cases (vec (sort available))) [nil])]
    (when-not (and (:ok inspection) (= (str expected-bundle) (:bundle inspection))
                   (<= (count selected) (:max-cases policy))
                   (= (count selected) (count (set selected)))
                   (if (seq available) (every? available selected) (nil? cases))
                   (<= 1 (* repetitions (count selected)) (:max-cells policy)))
      (refuse! "Inspect the exact valid workflow revision and select at most eight benchmark cells"))
    (let [selections
          (mapv (fn [case]
                  (workflow-runs/prepare-source
                   snapshot (cond-> {:name name :expected-bundle expected-bundle} case (assoc :case case)))) selected)
          bundle (:bundle (first selections))
          frozen (into (sorted-map)
                       (map (fn [case source]
                              (let [frozen (files/freeze-files
                                            (:task source) config
                                            (into {} (map (fn [[path text]] [(subs path 1) (files/text-file text 0x81A4)]))
                                                  (:inputs source)) limits)
                                    cell (hasch/uuid [:simmis/local-benchmark-cell-v1 case (:input-id frozen)])]
                                [cell {:case case :frozen frozen}])) selected selections))
          basis {:policy policy :worker (:worker input) :generation (:simmis.worker/generation pinned)
                 :config-id (:simmis.worker/config-id pinned)
                 :inputs (update-vals frozen #(get-in % [:frozen :input-id]))}
          checker-setup (checkers/world-setup bundle)
          setup (evaluation/make-world-setup
                 {:id :simmis/local-worker-benchmark-inputs :version 1
                  :basis {:worker basis :fixtures (evaluation/world-setup-ref checker-setup)}
                  :prepare (fn [{world :room env :environment :as context}]
                             (let [frozen (get-in frozen [(get-in env [:environment/metadata :worker-cell]) :frozen])
                                   result ((:prepare checker-setup) context)]
                               (when-not (= frozen (files/freeze world (:task frozen) config
                                                               (vec (keys (:files frozen))) limits))
                                 (refuse! "Benchmark fixtures differ from the pinned worker input"))
                               result))})
          evaluator (evaluator bundle config limits basis)
          verifier (evaluation/evaluator-ref evaluator)
          protocol-ref {:protocol/id :simmis/local-worker-benchmark :protocol/version 1 :protocol/basis basis}
          environments
          (mapv (fn [[cell {:keys [case frozen]}]]
                  (environment/make-environment
                   {:id (keyword "simmis.local-benchmark" name) :task (:task frozen)
                    :verifier {:id (:verifier/id verifier) :version (:verifier/version verifier) :basis (:verifier/basis verifier)}
                    :limits {:timeout-ms (:timeout-ms limits) :cancel-timeout-ms 30000 :on-timeout :verdict}
                    :world {:isolation :ctx :settlement :discard :effects work/effects
                            :resources {local/admission-unit 1M} :setup (evaluation/world-setup-ref setup)
                            :protocol protocol-ref}
                    :metadata (cond-> {:bundle (str expected-bundle) :worker-cell cell} case (assoc :case case))})) frozen)
          team (local/worker-team config)
          plan {:benchmark :simmis.local-benchmark :id (keyword (str "request-" (:request-id input)))
                :team team :environments environments :repetitions repetitions :parallelism 1
                :dataset {:id (keyword "simmis.local-benchmark" name)}
                :capabilities {:world-setup setup :evaluator evaluator}
                :metadata {:worker-policy policy :usage-measurement :unreported-local-subscription
                           :operator-admitted-bundle (str expected-bundle)}}
          experiment (runner/experiment-def plan)]
      {:request-kind :local-worker-benchmark :bundle bundle :plan (assoc plan :experiment experiment)
       :resources {local/admission-unit (bigdec (* repetitions (count selected)))} :resource-units local/units
       :basis {:submission input :snapshot snapshot :frozen frozen :policy basis :team team :experiment experiment}})))

(defn adapter [conn host-ctx room-id room request-id pinned prepared principal]
  (let [frozen (get-in prepared [:basis :frozen])
        ;; Process-local capability handoff from the canonical fixture adapter;
        ;; not forkable application state or an alternative room registry.
        parent (AtomicReference.)
        basis (get-in prepared [:basis :policy])
        cell! (fn [context]
                (let [cell (get-in context [:environment :environment/metadata :worker-cell])]
                  (when-not (contains? frozen cell) (refuse! "Unknown pinned benchmark cell"))
                  cell))
        ;; Bind case identity durably BEFORE calibration or guest publication.
        setup (get-in prepared [:plan :capabilities :world-setup])
        setup (update setup :prepare
                      (fn [prepare]
                        (fn [context]
                          (let [cell (cell! context)
                                assignment (local/assignment-input conn host-ctx room room-id request-id pinned
                                                                    (get-in frozen [cell :frozen]) cell (:run/id context))]
                            (try
                              (binding [ec/*execution-context* host-ctx] (workers/prepare! conn assignment))
                              (prepare context)
                              (catch Throwable error
                                (try
                                  (binding [ec/*execution-context* host-ctx]
                                    (workers/withdraw! conn assignment #(java.util.Date.)))
                                  (catch Throwable cleanup (.addSuppressed error cleanup)))
                                (throw error)))))))
        protocol (evaluation/make-protocol
                  {:id :simmis/local-worker-benchmark :version 1 :basis basis
                   :run (fn [context]
                          (let [cell (cell! context)
                                world-parent (or (.get parent) (refuse! "Canonical benchmark fixture is unavailable"))
                                capability (local/make-protocol conn host-ctx room room-id request-id pinned
                                                                (get-in frozen [cell :frozen]) basis cell world-parent)]
                            ((:run capability) context)))})
        prepared (-> prepared
                     (assoc :world-parent-ready! #(.set parent %))
                     (assoc-in [:plan :capabilities :world-setup] setup)
                     (assoc-in [:plan :capabilities :protocol] protocol))]
    (benchmarks/adapter host-ctx room prepared principal)))

(defn submit! [conn host-ctx principal room-id room input]
  (when-not (m/validate schema input) (refuse! "Invalid local benchmark request"))
  (binding [ec/*execution-context* host-ctx]
    (let [by (benchmarks/authorize! conn principal room-id)
          _ (when-not (= :human (:principal/kind principal)) (refuse! "Human benchmark admission required"))
          id (requests/request-id by room-id (:request-id input))]
      (if-let [row (requests/get-request conn id)]
        (do
          (when-not (and (= :local-worker-benchmark (:work.request/kind row))
                         (= input (get-in (requests/payload conn row) [:input :submission])))
            (refuse! "Request identity belongs to different work"))
          (work/status conn host-ctx principal room-id id))
        (let [pinned (local/enrollment! conn host-ctx room-id by input)
              config (workers/payload conn pinned :simmis.worker/config-ref)
              prepared (prepare (workflows/snapshot room (:name input)) input config pinned)
              cells (long (get (:resources prepared) local/admission-unit))]
          (work/submit-prepared! conn host-ctx principal room-id room
                                (assoc-in input [:limits :max-worker-admissions] cells)
                                (adapter conn host-ctx room-id room id pinned prepared principal)))))))
