(ns is.simm.ops.owned-worker-admission
  "Inactive fixed human HOST workflow/benchmark local-worker admission.
   Existing kernel, worker credentials, protocol and physical owners execute;
   no transport, controller, enrollment, runner or activation is added here."
  (:require [is.simm.model.work-requests :as requests]
            [is.simm.model.workers :as workers]
            [is.simm.ops.benchmarks :as benchmarks]
            [is.simm.ops.owned-reads :as reads]
            [is.simm.ops.owned-workflows :as sources]
            [is.simm.ops.owned-workspace :as owned]
            [is.simm.ops.work :as work]
            [is.simm.ops.worker-benchmarks :as local-benchmarks]
            [is.simm.ops.worker-work :as local]
            [is.simm.ops.workflow-runs :as workflow-runs]
            [is.simm.ops.workflows :as workflows]
            [malli.core :as m]
            [org.replikativ.spindel.engine.core :as ec]))

;; Independent LOCAL DTOs. Legacy/exported schemas and other routes unchanged.
(def ^:private workflow-schema (conj local/workflow-schema [:expected-owned-source :uuid]))
(def ^:private benchmark-schema (conj local-benchmarks/schema [:expected-owned-source :uuid]))

(defn- fail! [type message] (throw (ex-info message {:type type})))
(defn- unavailable! []
  (fail! ::unavailable "Worker admission unavailable; inspect request status before retrying"))
(defn- interrupted? [error]
  (loop [cause error seen #{}]
    (cond (nil? cause) false
          (instance? InterruptedException cause) true
          (contains? seen cause) false
          :else (recur (.getCause ^Throwable cause) (conj seen cause)))))
(defn- storage! [f]
  (try (f)
       (catch Throwable error
         (when (interrupted? error) (.interrupt (Thread/currentThread)))
         (unavailable!))))

(defn- authorize! [kind db principal room-id]
  (local/authorize-in! db principal room-id)
  (when (= :local-worker-benchmark kind)
    (benchmarks/authorize-in! db principal room-id)))

(defn- preflight! [kind host principal handle]
  (storage! #(let [{:keys [control-db room-id] :as current}
                   (reads/preflight-host-read! host principal handle)]
               (authorize! kind control-db principal room-id)
               current)))

(defn- current! [kind host principal handle original binding pinned]
  (let [current (preflight! kind host principal handle)]
    (when-not (= (select-keys original [:room-id :party])
                 (select-keys current [:room-id :party])) (unavailable!))
    (when binding
      (storage! #(owned/validate! (:control-db current) principal :merge
                                  (:room-id original) (:room binding) binding)))
    ;; SAME final control snapshot as original-owner and both action gates.
    (when (and pinned
               (not (true? (storage! #(local/current-enrollment-in? (:control-db current) pinned)))))
      (unavailable!))
    current))

(defn- retry! [kind conn original input]
  (let [result (storage! #(local/retry-request kind conn (:party original) (:room-id original) input))
        id (storage! #(requests/request-id (:party original) (:room-id original) (:request-id input)))]
    (when-not (and (uuid? id)
                   (or (= {:status :absent} result)
                       (and (map? result) (= #{:status :id} (set (keys result)))
                            (contains? #{:retry :conflict} (:status result)) (= id (:id result)))))
      (unavailable!))
    {:result result :id id}))

(defn- validate-input! [kind input]
  (when-not (and (m/validate (case kind :local-worker-workflow workflow-schema
                                   :local-worker-benchmark benchmark-schema) input)
                 (re-matches #"[a-z0-9][a-z0-9-]{0,63}" (:name input)))
    (fail! ::invalid "Invalid owned local worker request")))

(defn- observe-retry! [kind host principal handle input]
  (validate-input! kind input)
  (let [original (preflight! kind host principal handle)
        {:keys [control-conn execution-ctx]} (::reads/owner host)]
    (binding [ec/*execution-context* execution-ctx]
      (let [{:keys [result id]} (retry! kind control-conn original input)]
        (when (= :conflict (:status result))
          (fail! ::conflict "Request identity was used for different work"))
        (let [observed (if (= :retry (:status result))
                         (let [request (storage! #(work/status control-conn execution-ctx principal (:room-id original) id))]
                           (when-not (and (storage! #(m/validate work/public-status-schema request))
                                          (= {:id id :room (:room-id original) :by (:party original)
                                              :client-id (:request-id input) :kind kind}
                                             (select-keys request [:id :room :by :client-id :kind])))
                             (unavailable!))
                           {:status :retry :request request})
                         {:status :absent})]
          (current! kind host principal handle original nil nil)
          observed)))))

(defn retry-workflow!
  "Fixed HOST metadata-only retry observation before fresh-admission gates.
   Normal absence is not an authorization lease. No source/enrollment access."
  [host principal handle input]
  (observe-retry! :local-worker-workflow host principal handle input))

(defn retry-benchmark!
  "Fixed HOST OFFLINE benchmark retry observation; current operator and room
   authority still apply. Returns only normal absence or public request status."
  [host principal handle input]
  (observe-retry! :local-worker-benchmark host principal handle input))

(defn- submit-kind! [kind host principal handle input]
  (validate-input! kind input)
  (let [original (preflight! kind host principal handle)
        {:keys [control-conn execution-ctx]} (::reads/owner host)]
    (binding [ec/*execution-context* execution-ctx]
      (let [{:keys [result id]} (retry! kind control-conn original input)]
        (when (= :conflict (:status result))
          (fail! ::conflict "Request identity was used for different work"))
        (if (= :retry (:status result))
          (do (current! kind host principal handle original nil nil)
              (storage! #(work/status control-conn execution-ctx principal (:room-id original) id)))
          (let [binding (storage! #(reads/resolve-host-read! host principal handle))
                _ (when-not (= (select-keys original [:room-id :party])
                               (select-keys binding [:room-id :party])) (unavailable!))
                _ (storage! #(authorize! kind (:control-db binding) principal (:room-id binding)))
                manifest (storage! #(workflows/manifest-in (:db binding) (:name input)))
                revision (storage! #(sources/source-id binding (:name input) manifest))]
            (when-not (uuid? revision) (unavailable!))
            (when-not (= revision (:expected-owned-source input))
              (fail! ::conflict "Workflow source changed; reload metadata"))
            (let [snapshot (storage! #(workflows/snapshot-in (:db binding) (:name input)))
                  _ (when-not (= manifest (:manifest snapshot)) (unavailable!))
                  legacy (dissoc input :expected-owned-source)
                  ;; Benchmark case sources are always OFFLINE. Workflow LIVE
                  ;; input/publication planning uses the very same immutable DB.
                  resolution (when (= :local-worker-workflow kind)
                               (storage! #(workflow-runs/resolve-admission-in
                                           (:db binding) (select-keys (:target binding) [:registry-system :runtime-system])
                                           snapshot (select-keys legacy workflow-runs/source-fields))))
                  _ (when (and (:publication-basis resolution)
                               (not= (:target binding) (get-in resolution [:publication-basis :target])))
                      (unavailable!))
                  _ (current! kind host principal handle original binding nil)
                  pinned (storage! #(local/enrollment! control-conn execution-ctx (:room-id original) (:party original) legacy))
                  config (storage! #(workers/payload control-conn pinned :simmis.worker/config-ref))
                  prepared (storage! #(case kind
                                        :local-worker-workflow
                                        (local/prepare-workflow-in control-conn execution-ctx (:room-id original) (:room binding)
                                                                   id legacy pinned config snapshot resolution)
                                        :local-worker-benchmark
                                        (local-benchmarks/prepare snapshot legacy config pinned)))
                  channel (if (:principal/mcp? principal) :mcp :host)
                  prepared (-> prepared
                               (assoc :request-kind kind :execution-kind kind :submission-channel channel)
                               (assoc-in [:basis :submission] input)
                               (assoc-in [:basis :admission-channel] channel)
                               (assoc-in [:basis :source-origin]
                                         {:revision revision :target-stamp (:target-stamp binding)
                                          :target (:target binding) :manifest manifest}))
                  admissions (case kind :local-worker-workflow 1
                                   :local-worker-benchmark (get (:resources prepared) local/admission-unit))]
              (when-not (true? (storage! #(and (number? admissions) (<= 1 admissions 8)
                                               (= (bigdec (long admissions)) (bigdec admissions)))))
                (unavailable!))
              (current! kind host principal handle original binding pinned)
              (storage! #(work/submit-prepared!
                          control-conn execution-ctx principal (:room-id original) (:room binding)
                           ;; Canonical intent retains ORIGINAL submission above;
                           ;; the admission allowance is a kernel-only limit.
                          (assoc-in input [:limits :max-worker-admissions] (long admissions))
                          (case kind
                            :local-worker-workflow prepared
                            :local-worker-benchmark
                            (local-benchmarks/adapter control-conn execution-ctx (:room-id original) (:room binding)
                                                      id pinned prepared principal)))))))))))

(defn submit-workflow!
  "Inactive fixed owned workflow worker admission; no ad-hoc TASK entry."
  [host principal original-handle input]
  (submit-kind! :local-worker-workflow host principal original-handle input))

(defn submit-benchmark!
  "Inactive fixed owned OFFLINE benchmark worker admission."
  [host principal original-handle input]
  (submit-kind! :local-worker-benchmark host principal original-handle input))
