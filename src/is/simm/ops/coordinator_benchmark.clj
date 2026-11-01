(ns is.simm.ops.coordinator-benchmark
  "One inspected benchmark offer for a recursive coordinator. No agent-built
   checker, dataset, setup, fixture owner or second funding authority."
  (:require [dvergr.agent.authority :as authority]
            [dvergr.agent.evaluation :as evaluation]
            [dvergr.agent.experiment :as experiment]
            [dvergr.agent.roster :as roster]
            [dvergr.agent.subexperiment :as subexperiment]
            [hasch.core :as hasch]
            [is.simm.ops.benchmarks :as benchmarks]
            [is.simm.ops.benchmark-source :as retained]
            [is.simm.ops.benchmark-dataset :as dataset]
            [is.simm.ops.owned-reads :as reads]
            [is.simm.ops.owned-workflows :as sources]
            [is.simm.ops.owned-workspace :as owned]
            [is.simm.ops.work :as work]
            [is.simm.ops.work-sandbox :as worker]
            [is.simm.ops.workflows :as workflows]
            [is.simm.runtimes.operation-lifetime :as lifetime]
            [malli.core :as m]
            [org.replikativ.spindel.engine.core :as ec])
  (:import [java.util.concurrent Semaphore]
           [java.util.concurrent.atomic AtomicBoolean]))

(def schema
  [:map {:closed true} [:name workflows/name-schema]
   [:expected-bundle :uuid] [:expected-owned-source :uuid]
   [:split {:optional true} dataset/split-schema]
   [:cases {:optional true} [:vector {:min 1 :max 4} [:string {:min 1 :max 100}]]]
   [:limits work/limits-schema]])

(def invocation-id :simmis/admitted-benchmark)
(def policy {:version 1 :source :owned-inspected-snapshot :origin :clean-fixture
             :candidates :file-worker-only :dataset :fixed :funding :parent-run
             :capture :exact-admitted-verifier :max-candidates 1 :max-parallelism 1
             :max-active-invocations 1})

(defn- refuse! [reason]
  (throw (ex-info "Coordinator benchmark is not admitted"
                  {:type (if (= :source-changed reason) ::conflict ::refused) :reason reason})))

(defn compile!
  "Read exactly one guarded owned source snapshot. Called under the captured
   original Room operation before any request claim or provider dispatch."
  [host principal handle original input]
  (when-not (m/validate schema (:benchmark input)) (refuse! :invalid-admission))
  (let [binding (reads/resolve-host-read! host principal handle)
        admission (:benchmark input)
        room-id (:room-id original)
        _ (when-not (and (= room-id (:room-id binding))
                         (identical? (:room original) (:room binding)))
            (refuse! :source-owner-changed))
        _ (benchmarks/authorize-in! (:control-db binding) principal room-id)
        snapshot (workflows/snapshot-in (:db binding) (:name admission))
        manifest (:manifest snapshot)
        revision (sources/source-id binding (:name admission) manifest)
        _ (when-not (= revision (:expected-owned-source admission)) (refuse! :source-changed))
        source (retained/retain (:name admission) (:files snapshot))
        _ (retained/validate-manifest! source manifest)
        prepared (benchmarks/prepare snapshot
                                     (-> admission (dissoc :expected-owned-source)
                                         (assoc :request-id (:request-id input) :models [(:model input)] :repetitions 1)))
        origin {:revision revision :target-stamp (:target-stamp binding)
                :target (:target binding) :manifest manifest}
        _ (owned/validate! (:control-db (reads/preflight-host-read! host principal handle))
                           principal :merge room-id (:room original) binding)
        evaluator (get-in prepared [:plan :capabilities :evaluator])
        setup (get-in prepared [:plan :capabilities :world-setup])
        basis {:source source :source-origin origin :experiment (get-in prepared [:plan :experiment])
               :limits (:limits admission) :policy policy}
        ref {:invocation/id invocation-id :invocation/version 1
             :invocation/basis {:admission-id (hasch/uuid basis)
                                :file-verifier (evaluation/evaluator-ref evaluator)}}]
    {:ref ref :basis basis :prepared prepared :evaluator evaluator :setup setup}))

(defn current!
  "Recheck source identity and authority after provider preparation, under a
   short original Room operation which ends BEFORE native fork admission."
  [host principal handle original offer]
  (let [binding (reads/resolve-host-read! host principal handle)
        source (get-in offer [:basis :source])
        manifest (workflows/manifest-in (:db binding) (:name source))]
    (when-not (and (= (:room-id original) (:room-id binding))
                   (identical? (:room original) (:room binding))
                   (= (get-in offer [:basis :source-origin :revision])
                      (sources/source-id binding (:name source) manifest)))
      (refuse! :source-changed))
    (benchmarks/authorize-in! (:control-db binding) principal (:room-id original)))
  nil)

(defn candidate-team
  "Normalize candidate values through ordinary constructors and reject any
   broader sandbox, interpreter, tools or transport before native admission."
  [team parent-authority]
  (when-not (and (map? team) (roster/data-value? team)
                 (<= 1 (count (roster/agents team)) (:max-candidates policy)))
    (refuse! :invalid-candidates))
  (let [team (reduce roster/make-agent (roster/make-roster) (roster/agents team))
        ceiling {:tools #{:clojure_eval :read_file :write_file :edit_file}
                 :sandboxes #{#{worker/id}} :providers work/dispatch-accounted-providers}]
    (doseq [candidate (roster/agents team)]
      (authority/check-agent! candidate {:delegation-policy ceiling})
      (authority/check-agent! candidate parent-authority))
    ;; Register the existing installer, preserving exactly this sandbox.
    (worker/restrict-team team)))

(defn invoke!
  "Trusted fixed-plan handler. The generic native bridge never validates these
   domain options for us. Candidates cannot substitute task/case/setup/origin."
  [offer original-room fixture-parent authorize! {:keys [control-room work-room authority]} team opts]
  (when-not (and (map? opts) (every? #{:resources :parallelism} (keys opts))
                 (map? (:resources opts))
                 (or (not (contains? opts :parallelism))
                     (= 1 (:parallelism opts))))
    (refuse! :invalid-invocation-options))
  (let [ceiling (work/resources (get-in offer [:basis :limits]))]
    (when-not (and (seq (:resources opts))
                   (every? (fn [[unit amount]]
                             (and (contains? ceiling unit) (number? amount) (pos? amount)
                                  (<= amount (get ceiling unit)))) (:resources opts)))
      (refuse! :grant-exceeds-admission)))
  (when-not (identical? original-room control-room) (refuse! :control-owner-changed))
  (authorize!)
  (let [team (candidate-team team authority)
        original (get-in offer [:prepared :plan :experiment])
        definition (experiment/make-experiment
                    {:id :simmis/coordinator-benchmark
                     :dataset (:experiment/dataset original)
                     :candidates (roster/agents team) :repetitions 1
                     :metadata {:simmis/benchmark-invocation (:ref offer)
                                :simmis/workflow (get-in original [:experiment/metadata :simmis/workflow])
                                :operator-admitted-bundle (get-in original [:experiment/metadata :operator-admitted-bundle])}})
        scope {:evaluators {(evaluation/evaluator-ref (:evaluator offer)) (:evaluator offer)}
               :world-setups {(evaluation/world-setup-ref (:setup offer)) (:setup offer)}}
        slot (:invocation-slot offer)
        _ (when-not (instance? Semaphore slot) (refuse! :missing-physical-owner))
        held (AtomicBoolean. false)]
    ;; This scope is never offered to arbitrary agent-built EnvironmentDefs.
    ;; Candidates have the file-worker sandbox and cannot hire coordinators.
    (subexperiment/start! control-room work-room team definition (assoc opts :parallelism 1)
                          (assoc authority :experiment-capabilities scope)
                          {:world-parent (fixture-parent)
                           :admit! (fn [_]
                                     (when-not (.tryAcquire ^Semaphore slot)
                                       (throw (ex-info "Another benchmark invocation is still physically active" {:type ::busy})))
                                     (.set held true))
                           :settled! (fn [_ _]
                                       (when (.compareAndSet held true false) (.release ^Semaphore slot)))})))

(defn attach
  "One exact fixture/capacity owner for the outer coordinator's physical
   lifetime. Child grants still come solely from its current Run wallet."
  [conn host-ctx principal room-id room host base input offer]
  (let [transport (benchmarks/fixture-transport conn host-ctx principal room-id)
        create! (:job-context! transport)
        offered (assoc offer :invocation-slot (Semaphore. (:max-active-invocations policy)))
        with-operation (get-in host [:config :host-lifecycle :with-operation])
        _ (when-not (fn? with-operation) (refuse! :missing-app-owner))
        handler (fn [current team opts]
                  (with-operation
                    #(lifetime/call-detached
                      (fn [within-operation!]
                        (invoke! offered room (:world-parent transport)
                                 (fn [] (within-operation!
                                         (fn [] (binding [ec/*execution-context* host-ctx]
                                                  (benchmarks/authorize! conn principal room-id)))))
                                 current team opts)))))
        capabilities (assoc (:experiment-capabilities base) :invocations {(:ref offer) handler})]
    (-> base
        (merge (select-keys transport [:admit! :settled! :abandon!]))
        (assoc :admit! (fn [id]
                         (binding [ec/*execution-context* host-ctx]
                           (benchmarks/authorize! conn principal room-id))
                         ((:admit! transport) id))
               :experiment-capabilities capabilities
               :job-context! (fn [request] (create! request) (:ctx room)))
        (assoc-in [:basis :submission] input)
        (assoc-in [:basis :benchmark-offer] (assoc (:basis offer) :ref (:ref offer))))))
