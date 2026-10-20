(ns is.simm.ops.owned-workflow-admission
  "Inactive fixed human HOST saved-workflow admission. No transport, alternate
   runner, selected fallback, journal or configurable authority/source loader."
  (:require [is.simm.model.work-requests :as requests]
            [is.simm.ops.owned-reads :as reads]
            [is.simm.ops.owned-workflows :as sources]
            [is.simm.ops.owned-workspace :as owned]
            [is.simm.ops.work :as work]
            [is.simm.ops.workflow-runs :as workflows-run]
            [is.simm.ops.workflows :as workflows]
            [malli.core :as m]
            [org.replikativ.spindel.engine.core :as ec]))

(def ^:private schema
  ;; LOCAL typed domain DTO only. Other transports and legacy schemas do not
  ;; silently accept this expectation before their own coherent migration.
  (conj workflows-run/schema [:expected-owned-source :uuid]))

(defn- fail! [type message] (throw (ex-info message {:type type})))
(defn- unavailable! []
  (fail! ::unavailable "Workflow admission unavailable; inspect request status before retrying"))

(defn- interrupted? [failure]
  (loop [cause failure seen #{}]
    (cond (nil? cause) false
          (instance? InterruptedException cause) true
          (contains? seen cause) false
          :else (recur (.getCause ^Throwable cause) (conj seen cause)))))

(defn- storage! [f]
  (try (f)
       (catch Throwable error
         (when (interrupted? error) (.interrupt (Thread/currentThread)))
         ;; Native/helper messages, tags, statuses and causes never manufacture
         ;; normal revision/request conflicts or a retryable admission outcome.
         (unavailable!))))

(defn- preflight! [host principal handle]
  (storage!
   #(let [{:keys [control-db room-id] :as current} (reads/preflight-host-read! host principal handle)]
      (work/authorize-in! control-db principal room-id)
      current)))

(defn- current! [host principal handle original binding]
  (let [current (preflight! host principal handle)]
    (when-not (= (select-keys original [:room-id :party])
                 (select-keys current [:room-id :party]))
      (unavailable!))
    (when binding
      (storage! #(owned/validate! (:control-db current) principal :merge
                                  (:room-id original) (:room binding) binding)))
    current))

(defn- retry! [conn original input]
  (let [result (storage! #(workflows-run/retry-request conn (:party original) (:room-id original) input))
        id (storage! #(requests/request-id (:party original) (:room-id original) (:request-id input)))]
    (when-not (or (= {:status :absent} result)
                  (and (map? result) (= #{:status :id} (set (keys result)))
                       (contains? #{:retry :conflict} (:status result)) (= id (:id result))))
      (unavailable!))
    result))

(defn submit!
  "Fixed inactive HOST admission. A REQUIRED owned-source UUID is retained in
   ORIGINAL submission intent, distinct from the portable expected-bundle.
   Exact retries are metadata/journal only; uncertainty never schedules again."
  [host principal original-handle input]
  (when-not (and (m/validate schema input)
                 (re-matches #"[a-z0-9][a-z0-9-]{0,63}" (:name input)))
    (fail! ::invalid "Invalid owned workflow execution"))
  (let [original (preflight! host principal original-handle)
        {:keys [control-conn execution-ctx]} (::reads/owner host)]
    (binding [ec/*execution-context* execution-ctx]
      (let [{:keys [status id]} (retry! control-conn original input)]
        (when (= :conflict status)
          (fail! ::conflict "Request identity was used for different work"))
        (if (= :retry status)
          (do (current! host principal original-handle original nil)
              (storage! #(work/status control-conn execution-ctx principal (:room-id original) id)))
          (let [binding (storage! #(reads/resolve-host-read! host principal original-handle))
                _ (when-not (= (select-keys original [:room-id :party])
                               (select-keys binding [:room-id :party]))
                    (unavailable!))
                _ (storage! #(work/authorize-in! (:control-db binding) principal (:room-id binding)))
                manifest (storage! #(workflows/manifest-in (:db binding) (:name input)))
                revision (storage! #(sources/source-id binding (:name input) manifest))]
            (when-not (uuid? revision) (unavailable!))
            (when-not (= revision (:expected-owned-source input))
              (fail! ::conflict "Workflow source changed; reload metadata"))
            (let [snapshot (storage! #(workflows/snapshot-in (:db binding) (:name input)))
                  _ (when-not (= manifest (:manifest snapshot)) (unavailable!))
                  legacy-input (dissoc input :expected-owned-source)
                  resolution (storage! #(workflows-run/resolve-admission-in
                                         (:db binding) (select-keys (:target binding) [:registry-system :runtime-system])
                                         snapshot (select-keys legacy-input workflows-run/source-fields)))
                  _ (when (and (:publication-basis resolution)
                               (not= (:target binding) (get-in resolution [:publication-basis :target])))
                      (unavailable!))
                  ;; This gate is deliberately BEFORE provider preparation.
                  _ (current! host principal original-handle original binding)
                  prepared (storage! #(workflows-run/bind-publication
                                       (if-let [captured (:captured resolution)]
                                         (workflows-run/prepare snapshot legacy-input captured)
                                         (workflows-run/prepare snapshot legacy-input))
                                       resolution))
                  prepared (-> prepared
                               (assoc :submission-channel (if (:principal/mcp? principal) :mcp :host))
                               (assoc-in [:basis :submission] input)
                               (assoc-in [:basis :source-origin]
                                         {:revision revision :target-stamp (:target-stamp binding)
                                          :target (:target binding) :manifest manifest}))]
              ;; Current checks are boundaries, not a cross-store revocation
              ;; lease. The canonical kernel repeats authority and owns claim,
              ;; Job start, budgets, settlement and interrupted recovery.
              (current! host principal original-handle original binding)
              (storage! #(work/submit-prepared! control-conn execution-ctx principal
                                                (:room-id original) (:room binding) input prepared)))))))))
