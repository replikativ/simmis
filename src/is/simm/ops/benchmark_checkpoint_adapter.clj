(ns is.simm.ops.benchmark-checkpoint-adapter
  "Fixed native checkpoint admission. The shared transport owns the private
   fixture and capacity; Dvergr owns temporary worlds, execution and accounting.
   Source ownership is consumed during synchronous admission, never retained in
   runner capabilities. The owned native facade selects it from saved policy."
  (:require [datahike.api :as d]
            [dvergr.agent.evaluation :as evaluation]
            [dvergr.agent.experiment.runner :as runner]
            [dvergr.agent.run :as run]
            [dvergr.catalog.workspace :as seed]
            [dvergr.substrate.geschichte :as workspace]
            [is.simm.model.work-request-checkpoints :as checkpoints]
            [is.simm.model.work-requests :as requests]
            [is.simm.ops.benchmark-case-evidence :as evidence]
            [is.simm.ops.benchmark-case-manifest :as manifests]
            [is.simm.ops.benchmark-fixture :as fixture]
            [is.simm.ops.benchmark-fixture-snapshot :as fixture-snapshot]
            [is.simm.ops.benchmark-resolution :as resolution]
            [is.simm.ops.benchmark-source :as source]
            [is.simm.ops.benchmarks :as benchmarks]
            [is.simm.ops.checkpoint-roots :as pins]
            [is.simm.ops.knowledge-changes :as changes]
            [is.simm.ops.owned-room-operation :as room-operation]
            [is.simm.ops.owned-workspace :as owned]
            [is.simm.runtimes.operation-lifetime :as lifetime]
            [org.replikativ.spindel.engine.core :as ec])
  (:import [java.util.concurrent.atomic AtomicReference]))

(defn- refuse! [reason]
  (throw (ex-info "Checkpoint benchmark admission is held" {:type ::held :reason reason})))

(defn- original-app! [^AtomicReference owner-cell]
  (let [owner (.get owner-cell)]
    (when-not (and owner (::ticket owner) (identical? (::ticket owner) lifetime/*ticket*))
      (refuse! :original-app-unavailable))
    (lifetime/check-current!)
    owner))

(defn- job-current! [control job-id]
  (when (.isInterrupted (Thread/currentThread))
    (throw (InterruptedException. "Checkpoint benchmark admission interrupted")))
  (binding [ec/*execution-context* (:ctx control)]
    (let [job (run/run control job-id)]
      (when-not (and (= job-id (:run/id job))
                     (= :experiment (:run/kind job)) (= :running (:run/status job))
                     (not (run/cancel-requested? job-id)))
        (refuse! :job-no-longer-running)))))

(defn- source-handoff!
  "Only invoked through the admitting call's expiring App-binding restorer.
   The original source's current mapping/permissions remain required; its
   exact retained program bytes, rather than new authoring bytes, are executed."
  [^AtomicReference owner-cell conn host-ctx principal room-id control request f]
  (let [{:keys [host handle original binding] :as owner} (original-app! owner-cell)]
    (when-not (and owner (identical? control (:room original))
                   (= room-id (:room-id original)))
      (refuse! :original-source-unavailable))
    (binding [ec/*execution-context* host-ctx]
      (room-operation/with-current!
        host principal handle original
        (fn []
          (let [current! #(do
                            (lifetime/check-current!)
                            (benchmarks/authorize! conn principal room-id)
                            (owned/validate! @conn principal :merge room-id control binding)
                            (when-not (= request (requests/get-request conn (:work.request/id request)))
                              (refuse! :request-changed))
                            (job-current! control (:work.request/job request)))]
            (current!)
            (let [result (f)] (current!) result)))))))

(defn- selection [c0 snapshot]
  (let [basis (changes/basis snapshot)]
    (checkpoints/validate-selection!
     {:version 1 :policy :request-evidence-v1 :basis basis
      :origin {:version 1 :kind :datahike :commit (:commit basis)
               :source (select-keys basis [:store :repository])}
      :target (merge (get-in c0 [:selection :target])
                     (select-keys basis [:repository :store :backend :branch :ref]))})))

(defn- build-case!
  [conn principal room-id control parent request preparation retained c0 fixture-db ordinal handoff!]
  (job-current! control (:work.request/job request))
  (let [case-plan (nth (get-in preparation [:plan :cases]) ordinal)
        files (source/case-files retained (:case case-plan))
        expected (manifests/after-seeding (:manifest c0) files seed/fixture-seed-limits)
        checkpoints {(get-in c0 [:selection :target :runtime-system]) (get-in c0 [:selection :origin])}]
    ;; This call returns ONLY after its native temporary world physically
    ;; disposed. A retained journal row by itself never establishes that fact.
    (evaluation/with-checkpoint-fixture!
      control parent (:work.request/job request) checkpoints
      (fn []
        (seed/seed-in-context! files seed/fixture-seed-limits)
        (let [workspace-conn (:conn (workspace/current-workspace))
              live @workspace-conn
              snapshot (d/commit-as-db (:store live) (d/commit-id live))
              _ (when-not snapshot (refuse! :stored-case-unavailable))
              selected (selection c0 snapshot)]
          (when-not (= expected (manifests/inventory snapshot)) (refuse! :visible-case-mismatch))
          (handoff!
           (fn []
             (let [record (resolution/publish-case! conn principal room-id request preparation ordinal c0 selected)
                   body (:body record)]
              ;; The durable pinning intent precedes its sole physical attempt.
              ;; No replay on a lost native acknowledgement; uncertainty holds.
               (job-current! control (:work.request/job request))
               (pins/create! snapshot (:basis selected) (get-in body [:case :root]) (evidence/root-owner body))
               (resolution/acknowledge-case! conn principal room-id request preparation ordinal c0 fixture-db)))))))))

(defn adapter
  "Internal fixed native adapter. OWNER contains the original App HOST,
   handle, Room descriptor and captured owned source binding. The higher facade
   must hold that original App operation through submit-prepared!. No serialized
   map can establish this ownership, and no source/authority loader is accepted."
  [conn host-ctx control prepared principal room-id owner]
  (lifetime/check-current!)
  (when-not (and (= :benchmark (:request-kind prepared))
                 (get-in prepared [:basis :benchmark/source])
                 (identical? control (get-in owner [:original :room])))
    (refuse! :invalid-admission))
  (let [owner-cell (AtomicReference. (assoc owner ::ticket lifetime/*ticket*))
        plan-cell (AtomicReference.)
        program (dissoc prepared :knowledge/owner :execution-owner)
        transport (benchmarks/fixture-transport conn host-ctx principal room-id)
        parent (:world-parent transport)
        retire! (fn [callback id]
                  (try (callback id)
                       (finally (.set owner-cell nil) (.set plan-cell nil))))]
    (merge program (dissoc transport :world-parent)
           {:job-context! (fn [request]
                            (original-app! owner-cell)
                            ((:job-context! transport) request))
            :admit!
            (fn [job-id]
              ;; A different operation must not consume this admission's
              ;; ownership cell or acquire its native capacity.
              (original-app! owner-cell)
              (try
                ((:admit! transport) job-id)
                (let [request (requests/get-request conn
                                                    (requests/request-id
                                                     (:party (:original (.get owner-cell))) room-id
                                                     (get-in program [:basis :submission :request-id])))
                      _ (when-not (= job-id (:work.request/job request)) (refuse! :job-pointer-changed))
                      preparation (source-handoff! owner-cell conn host-ctx principal room-id control request
                                                   #(resolution/publish-preparation! conn principal room-id request))]
                  (when-not (true? (:new? preparation)) (refuse! :not-fresh-family))
                  (lifetime/call-detached
                   (fn [with-original-app!]
                     (let [receipt {:room (parent) :persistent-id (:work.request/fixture-room request)}
                           _ (fixture/validate-receipt! host-ctx (:work.request/id request) receipt)
                           {:keys [db c0]} (fixture-snapshot/capture! host-ctx (:work.request/id request) receipt)
                           handoff! (fn [f]
                                      (with-original-app!
                                        #(source-handoff! owner-cell conn host-ctx principal room-id control request f)))
                           records (mapv #(build-case! conn principal room-id control (:room receipt) request preparation
                                                       (get-in program [:basis :benchmark/source]) c0 db % handoff!)
                                         (range (count (:cases preparation))))]
                       (when-not (= (count records) (count (:cases preparation))) (refuse! :incomplete-builders))
                       (job-current! control job-id)
                       (when-not (= c0 (:c0 (fixture-snapshot/capture! host-ctx (:work.request/id request) receipt)))
                         (refuse! :pristine-fixture-changed))
                       ;; Every builder above has physically returned; only
                       ;; now may the fixed journal publish executable READY.
                       (let [ready (handoff! #(resolution/publish-ready! conn principal room-id request preparation c0 db))]
                         (.set plan-cell {:job-id job-id :plan (:plan ready)}))))))
                (finally (.set owner-cell nil))))
            :start!
            (fn [job-id]
              (let [{expected :job-id plan :plan :as entry} (.get plan-cell)]
                (when-not (and plan (= expected job-id)) (refuse! :no-admitted-plan))
                (when-not (.compareAndSet plan-cell entry nil) (refuse! :plan-already-consumed))
                (job-current! control job-id)
                (runner/run-in control (assoc plan :parent-run job-id :cleanup-group job-id
                                              :world-parent (parent) :from (:principal/actor principal)))))
            :settled! (fn [id] (retire! (:settled! transport) id))
            :abandon! (fn [id] (retire! (:abandon! transport) id))})))
