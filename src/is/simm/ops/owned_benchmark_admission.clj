(ns is.simm.ops.owned-benchmark-admission
  "Inactive fixed human HOST native/CLI benchmark admission. Existing kernel,
   experiment adapter and offline checker own all execution/resource lifetimes."
  (:require [is.simm.model.work-requests :as requests]
            [is.simm.ops.benchmarks :as benchmarks]
            [is.simm.ops.owned-reads :as reads]
            [is.simm.ops.owned-workflows :as sources]
            [is.simm.ops.owned-workspace :as owned]
            [is.simm.ops.work :as work]
            [is.simm.ops.workflows :as workflows]
            [malli.core :as m]
            [org.replikativ.spindel.engine.core :as ec]))

;; LOCAL schemas: the CLI limits replacement has already happened upstream.
;; Do not append fields to its base schema or expose these through old routes.
(def ^:private native-schema (conj benchmarks/schema [:expected-owned-source :uuid]))
(def ^:private cli-schema (conj benchmarks/cli-schema [:expected-owned-source :uuid]))

(defn- fail! [type message] (throw (ex-info message {:type type})))
(defn- unavailable! []
  (fail! ::unavailable "Benchmark admission unavailable; inspect request status before retrying"))
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

(defn- preflight! [host principal handle]
  (storage!
   #(let [{:keys [control-db room-id] :as current} (reads/preflight-host-read! host principal handle)]
      (benchmarks/authorize-in! control-db principal room-id)
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

(defn- retry! [kind conn original input]
  (let [result (storage! #(benchmarks/retry-request kind conn (:party original) (:room-id original) input))
        id (storage! #(requests/request-id (:party original) (:room-id original) (:request-id input)))]
    (when-not (or (= {:status :absent} result)
                  (and (map? result) (= #{:status :id} (set (keys result)))
                       (contains? #{:retry :conflict} (:status result)) (= id (:id result))))
      (unavailable!))
    result))

(defn- submit-kind! [kind host principal original-handle input]
  (when-not (and (m/validate (case kind :benchmark native-schema :cli-benchmark cli-schema) input)
                 (re-matches #"[a-z0-9][a-z0-9-]{0,63}" (:name input)))
    (fail! ::invalid "Invalid owned benchmark request"))
  (let [original (preflight! host principal original-handle)
        {:keys [control-conn execution-ctx]} (::reads/owner host)]
    (binding [ec/*execution-context* execution-ctx]
      (let [{:keys [status id]} (retry! kind control-conn original input)]
        (when (= :conflict status)
          (fail! ::conflict "Request identity was used for different work"))
        (if (= :retry status)
          (do (current! host principal original-handle original nil)
              (storage! #(work/status control-conn execution-ctx principal (:room-id original) id)))
          (let [binding (storage! #(reads/resolve-host-read! host principal original-handle))
                _ (when-not (= (select-keys original [:room-id :party])
                               (select-keys binding [:room-id :party])) (unavailable!))
                _ (storage! #(benchmarks/authorize-in! (:control-db binding) principal (:room-id binding)))
                manifest (storage! #(workflows/manifest-in (:db binding) (:name input)))
                revision (storage! #(sources/source-id binding (:name input) manifest))]
            (when-not (uuid? revision) (unavailable!))
            (when-not (= revision (:expected-owned-source input))
              (fail! ::conflict "Workflow source changed; reload metadata"))
            (let [snapshot (storage! #(workflows/snapshot-in (:db binding) (:name input)))
                  _ (when-not (= manifest (:manifest snapshot)) (unavailable!))
                  _ (current! host principal original-handle original binding)
                  ;; Both fixed preparations are OFFLINE. They verify frozen
                  ;; per-case inputs, never resolve live knowledge/publication.
                  prepared (storage! #(case kind
                                        :benchmark (benchmarks/prepare snapshot (dissoc input :expected-owned-source))
                                        :cli-benchmark (benchmarks/prepare-cli snapshot (dissoc input :expected-owned-source))))
                  channel (if (:principal/mcp? principal) :mcp :host)
                  prepared (-> prepared
                               (assoc :request-kind kind :submission-channel channel)
                               (assoc-in [:basis :submission] input)
                               ;; The experiment runner does NOT forward the
                               ;; kernel channel to Attempt metrics. This is
                               ;; truthful admission provenance, not that claim.
                               (assoc-in [:basis :admission-channel] channel)
                               (assoc-in [:basis :source-origin]
                                         {:revision revision :target-stamp (:target-stamp binding)
                                          :target (:target binding) :manifest manifest}))]
              (current! host principal original-handle original binding)
              (storage! #(work/submit-prepared! control-conn execution-ctx principal
                                                (:room-id original) (:room binding) input
                                                (benchmarks/adapter execution-ctx (:room binding) prepared principal))))))))))

(defn submit-native!
  "Fixed inactive ordinary native benchmark HOST entry. No legacy route flip."
  [host principal original-handle input]
  (submit-kind! :benchmark host principal original-handle input))

(defn submit-cli!
  "Fixed inactive CLI-text benchmark HOST entry; no CLI policy/probe on retry."
  [host principal original-handle input]
  (submit-kind! :cli-benchmark host principal original-handle input))
