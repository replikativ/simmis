(ns is.simm.model.work-requests
  "Durable at-most-once admission intents, not a second execution engine.
   Only the atomic insert winner may schedule. Jobs/Runs own execution status."
  (:require [datahike.api :as d]
            [dvergr.artifact :as artifact]
            [hasch.core :as hasch]))

(def schema
  (mapv (fn [[ident type]]
          (cond-> {:db/ident ident :db/valueType type :db/cardinality :db.cardinality/one}
            (= ident :work.request/id) (assoc :db/unique :db.unique/value)))
        [[:work.request/id :db.type/uuid]
         [:work.request/client-id :db.type/uuid]
         [:work.request/by :db.type/uuid]
         [:work.request/room :db.type/uuid]
         [:work.request/kind :db.type/keyword]
         [:work.request/intent-id :db.type/uuid]
         [:work.request/payload-ref :db.type/store-ref]
         [:work.request/claim :db.type/uuid]
         [:work.request/created-at :db.type/instant]
         [:work.request/state :db.type/keyword]
         [:work.request/job :db.type/uuid]
         [:work.request/return-pending? :db.type/boolean]
         [:work.request/budget-microdollars :db.type/long]
         [:work.request/max-dispatches :db.type/long]
         [:work.request/max-model-steps :db.type/long]
         [:work.request/max-worker-admissions :db.type/long]
         [:work.request/max-cli-runs :db.type/long]
         [:work.request/timeout-ms :db.type/long]
         [:work.request/settlement :db.type/keyword]
         [:work.request/incident :db.type/uuid]]))

(defn request-id [party room client-id]
  (hasch/uuid [:simmis/work-request-v1 party room client-id]))

(defn get-request [conn id]
  (let [db @conn]
    (when-let [eid (d/q '[:find ?e . :in $ ?id :where [?e :work.request/id ?id]] db id)]
      (d/pull db '[*] eid))))

(defn- same-intent! [row intent-id]
  (when-not (= intent-id (:work.request/intent-id row))
    (throw (ex-info "Request identity was already used for different work"
                    {:type ::conflict :request (:work.request/id row)})))
  row)

(defn- claim-tx [db row digest]
  ;; Conditional insert in the writer's serialized database. A no-op loser
  ;; still receives a COMMITTED report, unlike a uniqueness exception from a
  ;; batched writer which may precede publication of the winner's snapshot.
  (if-let [eid (d/q '[:find ?e . :in $ ?id :where [?e :work.request/id ?id]]
                    db (:work.request/id row))]
    (do (same-intent! (d/pull db '[*] eid) digest) [])
    [row]))

(defn claim!
  "Persist one exact intent before work. New entity + unique VALUE, never
   an upserting identity. Ambiguous transaction responses are resolved by the
   durable random claim token; a retry never blindly schedules another job."
  [conn {:keys [party room client-id kind intent limits]}]
  (when-not (and (every? uuid? [party room client-id]) (keyword? kind) (map? intent))
    (throw (ex-info "Invalid work-request identity" {:type ::invalid})))
  (let [id (request-id party room client-id)
        intent {:kind kind :limits (or limits {}) :input intent}
        digest (hasch/uuid intent)]
    (if-let [row (get-request conn id)]
      {:request (same-intent! row digest) :claimed? false}
      (let [claim (random-uuid)
            base (merge {:db/id -1
                         :work.request/id id :work.request/client-id client-id
                         :work.request/by party :work.request/room room :work.request/kind kind
                         :work.request/intent-id digest :work.request/claim claim
                         :work.request/created-at (java.util.Date.) :work.request/state :admitting}
                        (into {} (for [k [:budget-microdollars :max-dispatches :max-model-steps :max-worker-admissions :max-cli-runs
                                          :timeout-ms :settlement]
                                       :when (contains? limits k)]
                                   ;; JSON decoders may produce Integer while
                                   ;; Datahike's :db.type/long requires Long.
                                   ;; Preserve the exact intent payload; only
                                   ;; normalize these typed indexed columns.
                                   [(keyword "work.request" (name k))
                                    (if (= :settlement k) (get limits k)
                                        (let [n (get limits k)]
                                          (when-not (and (integer? n) (<= Long/MIN_VALUE n Long/MAX_VALUE))
                                            (throw (ex-info "Invalid integral work limit" {:type ::invalid :limit k})))
                                          (long n)))])))]
        (try
          (artifact/publish-value! (artifact/datahike-store conn) intent
                                   (fn [ref]
                                     (d/transact conn [[:db.fn/call claim-tx
                                                        (assoc base :work.request/payload-ref ref) digest]])))
          (catch Exception e
            ;; Only a durable row resolves an uncertain write or a lost insert
            ;; race. Failed reads/errors propagate; nothing is scheduled then.
            (when-not (get-request conn id) (throw e))))
        (let [row (same-intent! (get-request conn id) digest)]
          {:request row :claimed? (= claim (:work.request/claim row))})))))

(defn payload [conn request]
  (artifact/get-value (artifact/datahike-store conn) (:work.request/payload-ref request)))

(defn record-job!
  "Publish the winning request's job pointer BEFORE its evaluation starts.
   An ambiguous successful response is safe; a different job is never adopted."
  [conn id claim job-id]
  (let [row (get-request conn id)]
    (when-not (and row (= claim (:work.request/claim row)) (uuid? job-id)
                   (or (and (= :admitting (:work.request/state row))
                            (nil? (:work.request/job row)))
                       (and (= :submitted (:work.request/state row))
                            (= job-id (:work.request/job row)))))
      (throw (ex-info "Work admission does not own this request" {:type ::not-owner})))
    (when (= :admitting (:work.request/state row))
      (try
        (d/transact conn [[:db.fn/cas (:db/id row) :work.request/state :admitting :submitted]
                          [:db.fn/cas (:db/id row) :work.request/job nil job-id]
                          [:db/add (:db/id row) :work.request/return-pending? true]])
        (catch Exception e
          (let [after (get-request conn id)]
            (when-not (and (= :submitted (:work.request/state after))
                           (= job-id (:work.request/job after)))
              (throw e))))))
    (get-request conn id)))

(defn mark-returned! [conn id]
  (let [row (or (get-request conn id)
                (throw (ex-info "No such work request" {:type ::missing})))]
    (d/transact conn [{:db/id (:db/id row) :work.request/return-pending? false}])))

(defn record-provisioned!
  "An allowance has an idempotent ledger receipt rather than an execution
   job. Only its exact durable claim may record successful provisioning."
  [conn id claim]
  (let [row (get-request conn id)]
    (when-not (and (= :allowance (:work.request/kind row))
                   (= claim (:work.request/claim row)))
      (throw (ex-info "Not an allowance admission" {:type ::not-owner})))
    (when (= :admitting (:work.request/state row))
      (try
        (d/transact conn [[:db.fn/cas (:db/id row) :work.request/state :admitting :provisioned]])
        (catch Exception e
          (when-not (= :provisioned (:work.request/state (get-request conn id)))
            (throw e)))))
    (get-request conn id)))

(defn interrupt!
  "A stopped/failed admission is observable, never automatically re-executed."
  [conn id incident]
  (let [row (or (get-request conn id)
                (throw (ex-info "No such work request" {:type ::missing})))]
    (when (= :admitting (:work.request/state row))
      (try
        (d/transact conn (cond-> [[:db.fn/cas (:db/id row) :work.request/state
                                   :admitting :interrupted]]
                           incident (conj [:db/add (:db/id row) :work.request/incident incident])))
        (catch Exception e
          ;; A submitted job won the race, or interruption already committed.
          ;; Neither permits another admission; genuine failed writes propagate.
          ;; A competing writer's committed CAS value is authoritative even
          ;; while this connection's published snapshot is catching up.
          (let [{:keys [error old]} (ex-data e)
                lost-transition? (and (= :transact/cas error)
                                      (= :work.request/state (nth old 1 nil))
                                      (contains? #{:submitted :interrupted} (nth old 2 nil)))]
            (when (and (not lost-transition?)
                       (= :admitting (:work.request/state (get-request conn id))))
              (throw e))))))))

(defn requests [conn]
  (let [db @conn]
    (mapv #(d/pull db '[*] %)
          (d/q '[:find [?e ...] :where [?e :work.request/id _]] db))))
