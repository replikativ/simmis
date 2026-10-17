(ns is.simm.model.workers
  "Host-only durable enrollment and external assignment ownership.
   These are transport receipts, NOT a second Job/Run lifecycle. Callers must
   authorize room access. No lease timeout asserts physical process exit."
  (:require [datahike.api :as d]
            [dvergr.agent.roster :as roster]
            [dvergr.artifact :as artifact]
            [hasch.core :as hasch])
  (:import [java.nio.charset StandardCharsets]
           [java.security MessageDigest SecureRandom]
           [java.util Base64 Date]))

(def schema
  (mapv (fn [[ident type]]
          (cond-> {:db/ident ident :db/valueType type :db/cardinality :db.cardinality/one}
            (contains? #{:simmis.worker/id :simmis.worker.assignment/id} ident)
            (assoc :db/unique :db.unique/value)))
        [[:simmis.worker/id :db.type/uuid]
         [:simmis.worker/by :db.type/uuid]
         [:simmis.worker/room :db.type/uuid]
         [:simmis.worker/generation :db.type/long]
         [:simmis.worker/credential-hash :db.type/string]
         [:simmis.worker/config-id :db.type/uuid]
         [:simmis.worker/config-ref :db.type/store-ref]
         [:simmis.worker/created-at :db.type/instant]
         [:simmis.worker/revoked-at :db.type/instant]
         ;; The assignment id IS the existing Run UUID.
         [:simmis.worker.assignment/id :db.type/uuid]
         [:simmis.worker.assignment/worker :db.type/uuid]
         [:simmis.worker.assignment/generation :db.type/long]
         [:simmis.worker.assignment/room :db.type/uuid]
         [:simmis.worker.assignment/request :db.type/uuid]
         [:simmis.worker.assignment/job :db.type/uuid]
         [:simmis.worker.assignment/intent-id :db.type/uuid]
         [:simmis.worker.assignment/input-ref :db.type/store-ref]
         [:simmis.worker.assignment/state :db.type/keyword]
         [:simmis.worker.assignment/queued-at :db.type/instant]
         [:simmis.worker.assignment/nonce :db.type/uuid]
         [:simmis.worker.assignment/lease :db.type/uuid]
         [:simmis.worker.assignment/secret-hash :db.type/string]
         [:simmis.worker.assignment/heartbeat-at :db.type/instant]
         [:simmis.worker.assignment/expires-at :db.type/instant]
         [:simmis.worker.assignment/cancel-at :db.type/instant]
         [:simmis.worker.assignment/output-id :db.type/uuid]
         [:simmis.worker.assignment/output-ref :db.type/store-ref]
         [:simmis.worker.assignment/stop-ack-at :db.type/instant]]))

(defn- refuse! [type message] (throw (ex-info message {:type type})))

(defn new-secret []
  (let [bytes (byte-array 32)]
    (.nextBytes (SecureRandom.) bytes)
    (.encodeToString (.withoutPadding (Base64/getUrlEncoder)) bytes)))

(defn secret-hash [secret]
  (when-not (and (string? secret) (re-matches #"[A-Za-z0-9_-]{43}" secret))
    (refuse! ::invalid "A private 256-bit worker secret is required"))
  (.encodeToString (Base64/getUrlEncoder)
                   (.digest (MessageDigest/getInstance "SHA-256")
                            (.getBytes ^String secret StandardCharsets/UTF_8))))

(defn- matches-hash? [expected actual]
  (and (string? expected) (string? actual)
       (MessageDigest/isEqual (.getBytes ^String expected StandardCharsets/UTF_8)
                              (.getBytes ^String actual StandardCharsets/UTF_8))))

(defn matches-secret? [hash secret]
  (try
    (matches-hash? hash (secret-hash secret))
    (catch Exception _ false)))

(defn- entity [db attr id]
  (when-let [eid (d/q '[:find ?e . :in $ ?attr ?id :where [?e ?attr ?id]] db attr id)]
    (d/pull db '[*] eid)))

(defn enrollment [conn id] (entity @conn :simmis.worker/id id))
(defn assignment [conn id] (entity @conn :simmis.worker.assignment/id id))

(defn- active-enrollment! [db id generation room]
  (let [worker (entity db :simmis.worker/id id)]
    (when-not (and worker (nil? (:simmis.worker/revoked-at worker))
                   (= generation (:simmis.worker/generation worker))
                   (= room (:simmis.worker/room worker)))
      (refuse! ::denied "Worker enrollment is unavailable or revoked"))
    worker))

(defn- insert-tx [db row attr id digest-attr]
  (if-let [old (entity db attr id)]
    (do (when-not (= (get row digest-attr) (get old digest-attr))
          (refuse! ::conflict "Identity already belongs to another immutable intent")) [])
    (do
      (when (= attr :simmis.worker.assignment/id)
        (active-enrollment! db (:simmis.worker.assignment/worker row)
                            (:simmis.worker.assignment/generation row) (:simmis.worker.assignment/room row)))
      [row])))

(defn- publish-row! [conn row attr id digest-attr ref-attr value]
  (try
    (artifact/publish-value! (artifact/datahike-store conn) value
                             (fn [ref]
                               (d/transact conn [[:db.fn/call insert-tx (assoc row ref-attr ref)
                                                  attr id digest-attr]])))
    (catch Exception error
      (when-not (= (get row digest-attr) (get (entity @conn attr id) digest-attr)) (throw error))))
  (entity @conn attr id))

(defn enroll!
  "The local client persists id+secret BEFORE enrollment, so lost responses
   can retry without creating another credential. Never return/log the hash."
  [conn {:keys [id by room secret config]}]
  (when-not (and (every? uuid? [id by room]) (map? config) (roster/data-value? config))
    (refuse! ::invalid "Invalid worker enrollment"))
  (let [hash (secret-hash secret)
        digest (hasch/uuid [:simmis/worker-v1 by room hash config])]
    (publish-row! conn {:db/id -1 :simmis.worker/id id :simmis.worker/by by :simmis.worker/room room
                        :simmis.worker/generation 1 :simmis.worker/credential-hash hash
                        :simmis.worker/config-id digest :simmis.worker/created-at (Date.)}
                  :simmis.worker/id id :simmis.worker/config-id :simmis.worker/config-ref config)))

(defn revoke! [conn id]
  (let [row (or (enrollment conn id) (refuse! ::missing "No such worker"))]
    ;; A repeated revoke cannot resurrect or change any assignment ownership.
    (when-not (:simmis.worker/revoked-at row)
      (try (d/transact conn [[:db.fn/cas (:db/id row) :simmis.worker/revoked-at nil (Date.)]])
           (catch Exception error
             (when-not (:simmis.worker/revoked-at (enrollment conn id)) (throw error)))))
    (enrollment conn id)))

(defn enqueue!
  "Freeze one selected worker's inputs against the existing request/Job/Run.
   Queuing does not grant the worker ownership or permission to execute."
  [conn {:keys [id worker generation room request job input]}]
  (when-not (and (every? uuid? [id worker room request job])
                 (pos-int? generation) (map? input) (roster/data-value? input))
    (refuse! ::invalid "Invalid worker assignment"))
  (active-enrollment! @conn worker generation room)
  (let [digest (hasch/uuid [:simmis/worker-assignment-v1 worker generation room request job input])]
    (publish-row! conn {:db/id -1 :simmis.worker.assignment/id id
                        :simmis.worker.assignment/worker worker :simmis.worker.assignment/generation generation
                        :simmis.worker.assignment/room room :simmis.worker.assignment/request request
                        :simmis.worker.assignment/job job :simmis.worker.assignment/intent-id digest
                        :simmis.worker.assignment/state :queued :simmis.worker.assignment/queued-at (Date.)}
                  :simmis.worker.assignment/id id :simmis.worker.assignment/intent-id
                  :simmis.worker.assignment/input-ref input)))

(defn payload [conn row field]
  (when-let [ref (get row field)] (artifact/get-value (artifact/datahike-store conn) ref)))

(defn- owned! [db id lease hash]
  (let [row (entity db :simmis.worker.assignment/id id)]
    (when-not (and row (= lease (:simmis.worker.assignment/lease row))
                   (matches-hash? (:simmis.worker.assignment/secret-hash row) hash))
      (refuse! ::denied "No such owned worker lease"))
    row))

(defn- updates [row fields]
  [(assoc fields :db/id (:db/id row))])

(defn- expired? [row now]
  (when-let [expiry (:simmis.worker.assignment/expires-at row)]
    (not (.after ^Date expiry ^Date now))))

(defn- quarantine [row now]
  (updates row {:simmis.worker.assignment/state :quarantined
                :simmis.worker.assignment/cancel-at (or (:simmis.worker.assignment/cancel-at row) now)}))

(defn- claim-tx [db id worker generation nonce hash lease now expires]
  (let [row (or (entity db :simmis.worker.assignment/id id) (refuse! ::missing "No such assignment"))]
    (active-enrollment! db worker generation (:simmis.worker.assignment/room row))
    (when-not (and (= worker (:simmis.worker.assignment/worker row))
                   (= generation (:simmis.worker.assignment/generation row)))
      (refuse! ::denied "Assignment belongs to another worker"))
    (if (= :queued (:simmis.worker.assignment/state row))
      (updates row {:simmis.worker.assignment/state :claimed :simmis.worker.assignment/nonce nonce
                    :simmis.worker.assignment/secret-hash hash :simmis.worker.assignment/lease lease
                    :simmis.worker.assignment/heartbeat-at now :simmis.worker.assignment/expires-at expires})
      (if (and (= nonce (:simmis.worker.assignment/nonce row))
               (= hash (:simmis.worker.assignment/secret-hash row)))
        (if (and (= :claimed (:simmis.worker.assignment/state row)) (expired? row now))
          (quarantine row now) [])
        (refuse! ::conflict "Assignment already claimed or withdrawn; never reassign this Run")))))

(defn claim!
  "An exclusive writer-serialized claim. Client nonce+secret are persisted
   locally first. Retries observe this owner, never extend an expired lease."
  [conn id worker generation nonce secret now ttl-ms]
  (when-not (and (every? uuid? [id worker nonce]) (pos-int? generation)
                 (instance? Date now) (integer? ttl-ms) (<= 1000 ttl-ms 60000))
    (refuse! ::invalid "Invalid claim"))
  (let [hash (secret-hash secret)
        lease (hasch/uuid [:simmis/worker-lease-v1 id worker generation nonce])]
    (try
      (d/transact conn [[:db.fn/call claim-tx id worker generation nonce hash lease now
                         (Date. (+ (.getTime ^Date now) ttl-ms))]])
      (catch Exception error
        (when (or (contains? #{::denied ::conflict ::missing ::invalid} (:type (ex-data error)))
                  (not (and (= nonce (:simmis.worker.assignment/nonce (assignment conn id)))
                            (= hash (:simmis.worker.assignment/secret-hash (assignment conn id))))))
          (throw error))))
    (assignment conn id)))

(defn- cancel-tx [db id now]
  (let [row (or (entity db :simmis.worker.assignment/id id) (refuse! ::missing "No such assignment"))]
    (cond
      (:simmis.worker.assignment/cancel-at row) []
      (= :queued (:simmis.worker.assignment/state row))
      ;; The same serialized writer decides claim vs withdraw. A never-claimed
      ;; assignment cannot have launched a conforming worker.
      (updates row {:simmis.worker.assignment/state :withdrawn :simmis.worker.assignment/cancel-at now
                    :simmis.worker.assignment/stop-ack-at now})
      (:simmis.worker.assignment/stop-ack-at row)
      (updates row {:simmis.worker.assignment/cancel-at now})
      :else (quarantine row now))))

(defn cancel! [conn id now]
  (when-not (instance? Date now) (refuse! ::invalid "Invalid cancellation time"))
  (d/transact conn [[:db.fn/call cancel-tx id now]])
  (assignment conn id))

(defn- heartbeat-tx [db id lease hash now ttl-ms]
  (let [row (owned! db id lease hash)
        worker (entity db :simmis.worker/id (:simmis.worker.assignment/worker row))]
    (cond
      (:simmis.worker.assignment/stop-ack-at row) []
      (or (:simmis.worker.assignment/cancel-at row) (expired? row now)
          (:simmis.worker/revoked-at worker)
          (not= (:simmis.worker/generation worker) (:simmis.worker.assignment/generation row)))
      (quarantine row now)
      :else (updates row {:simmis.worker.assignment/heartbeat-at now
                          :simmis.worker.assignment/expires-at (Date. (+ (.getTime ^Date now) ttl-ms))}))))

(defn heartbeat!
  "Authority layer must turn lost room permission into cancel!, not renewal.
   Expiry/revocation signals shutdown but never fabricates a stop ack."
  [conn id lease secret now ttl-ms]
  (when-not (and (instance? Date now) (integer? ttl-ms) (<= 1000 ttl-ms 60000))
    (refuse! ::invalid "Invalid heartbeat"))
  (d/transact conn [[:db.fn/call heartbeat-tx id lease (secret-hash secret) now ttl-ms]])
  (assignment conn id))

(defn- stopped-tx [db id lease hash now]
  (let [row (owned! db id lease hash)]
    (if (:simmis.worker.assignment/stop-ack-at row) []
        (updates row {:simmis.worker.assignment/state :stopped :simmis.worker.assignment/stop-ack-at now}))))

(defn acknowledge-stop!
  "Only the existing lease proof may report process-tree shutdown, even after
   enrollment revocation or expiry. This grants NO new input/output access."
  [conn id lease secret now]
  (when-not (instance? Date now) (refuse! ::invalid "Invalid shutdown acknowledgement time"))
  (d/transact conn [[:db.fn/call stopped-tx id lease (secret-hash secret) now]])
  (assignment conn id))

(defn- complete-tx [db id lease hash digest ref now]
  (let [row (owned! db id lease hash)]
    (if-let [old (:simmis.worker.assignment/output-id row)]
      (do (when-not (= old digest) (refuse! ::conflict "Lease already published a different result")) [])
      (do
        (active-enrollment! db (:simmis.worker.assignment/worker row)
                            (:simmis.worker.assignment/generation row) (:simmis.worker.assignment/room row))
        (when-not (and (= :claimed (:simmis.worker.assignment/state row))
                       (nil? (:simmis.worker.assignment/cancel-at row)) (not (expired? row now)))
          (refuse! ::denied "Lease is cancelled, expired or stopped; output cannot be admitted"))
        ;; Completion includes the worker's physical-stop acknowledgement.
        ;; The worker must have already stopped the process and descendants.
        (updates row {:simmis.worker.assignment/state :completed :simmis.worker.assignment/output-id digest
                      :simmis.worker.assignment/output-ref ref :simmis.worker.assignment/stop-ack-at now})))))

(defn complete!
  "Publish validated output only after the worker reports physical shutdown.
   Authority and file/byte limits belong to the host service; this transaction
   fences ownership/revocation/expiry and binds an immutable result digest."
  [conn id lease secret output now]
  (when-not (and (map? output) (roster/data-value? output) (instance? Date now))
    (refuse! ::invalid "Invalid worker result"))
  (let [digest (hasch/uuid [:simmis/worker-output-v1 output])
        hash (secret-hash secret)]
    (try
      (artifact/publish-value! (artifact/datahike-store conn) output
                               (fn [ref] (d/transact conn [[:db.fn/call complete-tx id lease hash digest ref now]])))
      (catch Exception error
        (when-not (and (matches-secret? (:simmis.worker.assignment/secret-hash (assignment conn id)) secret)
                       (= lease (:simmis.worker.assignment/lease (assignment conn id)))
                       (= digest (:simmis.worker.assignment/output-id (assignment conn id))))
          (throw error))))
    (assignment conn id)))
