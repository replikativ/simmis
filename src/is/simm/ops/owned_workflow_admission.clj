(ns is.simm.ops.owned-workflow-admission
  "Fixed human HOST saved-workflow admission. No transport, alternate
   runner, selected fallback, journal or configurable authority/source loader."
  (:require [dvergr.resource :as resource]
            [is.simm.model.work-requests :as requests]
            [is.simm.ops.owned-room-operation :as room-operation]
            [is.simm.ops.knowledge-batch-inputs :as batch-inputs]
            [is.simm.ops.knowledge-batch-proof :as batch-proof]
            [is.simm.ops.owned-knowledge-discovery :as discovery]
            [is.simm.ops.owned-reads :as reads]
            [is.simm.ops.owned-workflows :as sources]
            [is.simm.ops.owned-workspace :as owned]
            [is.simm.ops.work :as work]
            [is.simm.ops.workflow-runs :as workflows-run]
            [is.simm.ops.workflows :as workflows]
            [is.simm.runtimes.operation-lifetime :as lifetime]
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

(defn- with-source! [host principal handle original-room f]
  (if original-room
    (room-operation/with-current! host principal handle original-room f)
    (f)))

(defn- current!
  ([host principal handle original binding]
   (current! host principal handle original binding nil))
  ([host principal handle original binding original-room]
   (let [current (preflight! host principal handle)]
     (when-not (= (select-keys original [:room-id :party])
                  (select-keys current [:room-id :party]))
       (unavailable!))
     (when binding
       (storage! #(with-source! host principal handle original-room
                    (fn [] (owned/validate! (:control-db current) principal :merge
                                           (:room-id original) (:room binding) binding)))))
     current)))

(defn- retry! [conn original input]
  (let [result (storage! #(workflows-run/retry-request conn (:party original) (:room-id original) input))
        id (storage! #(requests/request-id (:party original) (:room-id original) (:request-id input)))]
    (when-not (or (= {:status :absent} result)
                  (and (map? result) (= #{:status :id} (set (keys result)))
                       (contains? #{:retry :conflict} (:status result)) (= id (:id result))))
      (unavailable!))
    result))

(defn- batch-readiness!
  "Predictable refusal before claiming source versions, not a reservation.
   Every original-Room extent ends before discovery/provider/native execution."
  [host principal handle original original-room input]
  (lifetime/check-current!)
  (current! host principal handle original nil)
  (let [eligibility (storage! #(work/model-eligibility (:model input)))]
    (case (:status eligibility)
      :eligible
      (when-not (and (= #{:status :policy} (set (keys eligibility)))
                     (= #{:provider :model} (set (keys (:policy eligibility))))
                     (contains? work/dispatch-accounted-providers (get-in eligibility [:policy :provider]))
                     (string? (get-in eligibility [:policy :model])))
        (unavailable!))
      (:unknown :unsupported) (fail! ::model "Select an available model with native dispatch accounting")
      (unavailable!)))
  (let [enough?
        (storage!
         #(room-operation/with-current!
           host principal handle original-room
           (fn []
             (binding [ec/*execution-context* (:ctx (:room original-room))]
               (let [balance (resource/balance (:room original-room))]
                 (every? (fn [[unit amount]] (>= (get balance unit 0M) amount))
                         (work/resources (:limits input))))))))]
    (current! host principal handle original nil)
    (lifetime/check-current!)
    (when-not enough? (fail! ::allowance "Insufficient room execution allowance for this workflow"))))

(defn- submit-batch!
  [host principal handle original original-room binding input snapshot declaration]
  ;; Overrides need their own retained invocation identity before they can be
  ;; admitted. Reject them before discovery, a claim, or provider preparation.
  (when (or (:case input) (seq (:params input)) (seq (:inputs input)))
    (fail! ::invalid "Incremental knowledge workflows currently use their default inputs"))
  (lifetime/check-current!)
  (when-not original-room (unavailable!))
  (batch-readiness! host principal handle original original-room input)
  (let [{:keys [control-conn execution-ctx]} (::reads/owner host)
        ready (storage! #(discovery/prepare-batch!
                          host principal handle
                          {:name (:name input) :expected-source (:expected-owned-source input)
                           :expected-bundle (:expected-bundle input) :epoch (:epoch declaration)}))]
    (if (not= :ready (:status ready))
      ready
      (let [_ (when-not (= snapshot (:snapshot ready)) (unavailable!))
            proof (storage! #(batch-proof/ready! control-conn (:party original) (:room-id original)
                                                (:batch-origin ready)))
            frozen (:frozen proof)
            source (:source-origin proof)
            _ (when-not (and (= source (:source-origin ready))
                             (= (:captured frozen) (:captured ready))
                             (= (:publication-basis frozen) (:publication-basis ready)))
                (unavailable!))
            _ (current! host principal handle original binding original-room)
            _ (lifetime/check-current!)
            _ (batch-readiness! host principal handle original original-room input)
            prepared (storage! #(workflows-run/prepare-frozen-batch
                                 snapshot input proof (:control-revision ready)
                                 {:host host :original original-room :handle handle :ticket lifetime/*ticket*}))
            prepared (assoc prepared :submission-channel (if (:principal/mcp? principal) :mcp :host))]
        (current! host principal handle original binding original-room)
        (storage! #(work/submit-prepared! control-conn execution-ctx principal
                                          (:room-id original) (:room original-room) input prepared))))))

(defn- capture-source!
  [host principal handle original input original-room]
  (with-source!
   host principal handle original-room
   (fn []
     (let [binding (storage! #(reads/resolve-host-read! host principal handle))
           _ (when-not (and (= (select-keys original [:room-id :party])
                              (select-keys binding [:room-id :party]))
                            (or (nil? original-room) (identical? (:room original-room) (:room binding))))
               (unavailable!))
           _ (storage! #(work/authorize-in! (:control-db binding) principal (:room-id binding)))
           manifest (storage! #(workflows/manifest-in (:db binding) (:name input)))
           revision (storage! #(sources/source-id binding (:name input) manifest))]
       (when-not (uuid? revision) (unavailable!))
       (when-not (= revision (:expected-owned-source input))
         (fail! ::conflict "Workflow source changed; reload metadata"))
       (let [snapshot (storage! #(workflows/snapshot-in (:db binding) (:name input)))
             _ (when-not (= manifest (:manifest snapshot)) (unavailable!))
             declaration (storage! #(batch-inputs/declaration (:files snapshot)))
             resolution (when-not declaration
                          (storage! #(workflows-run/resolve-admission-in
                                      (:db binding) (select-keys (:target binding) [:registry-system :runtime-system])
                                      snapshot (select-keys input workflows-run/source-fields))))]
         (when (and (:publication-basis resolution)
                    (not= (:target binding) (get-in resolution [:publication-basis :target])))
           (unavailable!))
         {:binding binding :snapshot snapshot :revision revision
          :declaration declaration :resolution resolution})))))

(defn- submit-owned!
  "Fixed HOST admission. Exact retries are source/provider-free;
   fresh source IO uses short original-Room extents when lifetime is installed."
  [host principal handle input]
  (when-not (and (m/validate schema input)
                 (re-matches #"[a-z0-9][a-z0-9-]{0,63}" (:name input)))
    (fail! ::invalid "Invalid owned workflow execution"))
  (let [original (preflight! host principal handle)
        {:keys [control-conn execution-ctx]} (::reads/owner host)]
    (binding [ec/*execution-context* execution-ctx]
      (let [{:keys [status id]} (retry! control-conn original input)]
        (when (= :conflict status)
          (fail! ::conflict "Request identity was used for different work"))
        (if (= :retry status)
          (do (current! host principal handle original nil)
              (storage! #(work/status control-conn execution-ctx principal (:room-id original) id)))
          (let [original-room (when (get-in host [:config :host-lifecycle :with-operation])
                                (storage! #(reads/resolve-host-room! host principal handle)))
                {:keys [binding snapshot revision declaration resolution]}
                (capture-source! host principal handle original input original-room)]
            (if declaration
              (submit-batch! host principal handle original original-room binding input snapshot declaration)
              (let [legacy-input (dissoc input :expected-owned-source)
                    _ (current! host principal handle original binding original-room)
                    _ (when original-room (lifetime/check-current!))
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
                                            :target (:target binding) :manifest (:manifest snapshot)}))]
                (current! host principal handle original binding original-room)
                (storage! #(work/submit-prepared! control-conn execution-ctx principal
                                                  (:room-id original) (:room binding) input prepared))))))))))

(defn submit!
  "Owned admission uses the original App extent when installed. Incremental
   batches require it; ordinary adapters retain their existing contract.
   No source lease extends into provider preparation or native world execution."
  [host principal handle input]
  (if-let [with-operation (get-in host [:config :host-lifecycle :with-operation])]
    (with-operation #(submit-owned! host principal handle input))
    (submit-owned! host principal handle input)))
