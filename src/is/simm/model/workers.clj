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

(defn- checked-hash! [hash]
  (when-not (and (string? hash) (re-matches #"[A-Za-z0-9_-]{43}=" hash))
    (refuse! ::invalid "Expected a worker proof verifier, not a bearer secret"))
  hash)

(defn- entity [db attr id]
  (when-let [eid (d/q '[:find ?e . :in $ ?attr ?id :where [?e ?attr ?id]] db attr id)]
    (d/pull db '[*] eid)))

(defn enrollment [conn id] (entity @conn :simmis.worker/id id))
(defn assignment [conn id] (entity @conn :simmis.worker.assignment/id id))

(defn- active-enrollment! [db id generation room permitted?]
  (let [worker (entity db :simmis.worker/id id)]
    (when-not (and worker (nil? (:simmis.worker/revoked-at worker))
                   (= generation (:simmis.worker/generation worker))
                   (= room (:simmis.worker/room worker))
                   (or (nil? permitted?) (permitted? db worker)))
      (refuse! ::denied "Worker enrollment is unavailable or revoked"))
    worker))

(defn- insert-tx [db row attr id digest-attr permitted?]
  (if-let [old (entity db attr id)]
    (do (when-not (= (get row digest-attr) (get old digest-attr))
          (refuse! ::conflict "Identity already belongs to another immutable intent")) [])
    (do
      (when (= attr :simmis.worker.assignment/id)
        (active-enrollment! db (:simmis.worker.assignment/worker row)
                            (:simmis.worker.assignment/generation row) (:simmis.worker.assignment/room row) permitted?))
      [row])))

(defn- publish-row! [conn row attr id digest-attr ref-attr value & [permitted?]]
  (try
    (artifact/publish-value! (artifact/datahike-store conn) value
                             (fn [ref]
                               (d/transact conn [[:db.fn/call insert-tx (assoc row ref-attr ref)
                                                  attr id digest-attr permitted?]])))
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
  [conn {:keys [id worker generation room request job input permitted?]}]
  (when-not (and (every? uuid? [id worker room request job])
                 (pos-int? generation) (map? input) (roster/data-value? input))
    (refuse! ::invalid "Invalid worker assignment"))
  (active-enrollment! @conn worker generation room permitted?)
  (let [digest (hasch/uuid [:simmis/worker-assignment-v1 worker generation room request job input])]
    (publish-row! conn {:db/id -1 :simmis.worker.assignment/id id
                        :simmis.worker.assignment/worker worker :simmis.worker.assignment/generation generation
                        :simmis.worker.assignment/room room :simmis.worker.assignment/request request
                        :simmis.worker.assignment/job job :simmis.worker.assignment/intent-id digest
                        :simmis.worker.assignment/state :queued :simmis.worker.assignment/queued-at (Date.)}
                  :simmis.worker.assignment/id id :simmis.worker.assignment/intent-id
                  :simmis.worker.assignment/input-ref input permitted?)))

(defn payload [conn row field]
  (when-not (contains? #{:simmis.worker/config-ref :simmis.worker.assignment/input-ref
                         :simmis.worker.assignment/output-ref} field)
    (refuse! ::invalid "Unknown worker payload field"))
  (when-let [ref (get row field)]
    (let [value (artifact/get-value (artifact/datahike-store conn) ref)
          [expected basis]
          (case field
            :simmis.worker/config-ref
            [(:simmis.worker/config-id row)
             [:simmis/worker-v1 (:simmis.worker/by row) (:simmis.worker/room row)
              (:simmis.worker/credential-hash row) value]]
            :simmis.worker.assignment/input-ref
            [(:simmis.worker.assignment/intent-id row)
             [:simmis/worker-assignment-v1 (:simmis.worker.assignment/worker row)
              (:simmis.worker.assignment/generation row) (:simmis.worker.assignment/room row)
              (:simmis.worker.assignment/request row) (:simmis.worker.assignment/job row) value]]
            :simmis.worker.assignment/output-ref
            [(:simmis.worker.assignment/output-id row) [:simmis/worker-output-v1 value]])]
      (when-not (and (map? value) (= expected (hasch/uuid basis)))
        (refuse! ::corrupt "Worker payload is unavailable or does not match its immutable identity"))
      value)))

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

(defn- instant [source]
  ;; Production supplies a host clock evaluated IN the serialized transaction,
  ;; not a request timestamp that could grow stale during artifact publication.
  (let [now (if (fn? source) (source) source)]
    (when-not (instance? Date now) (refuse! ::invalid "Invalid host clock"))
    now))

(defn- claim-tx [db id worker generation nonce hash lease clock ttl-ms permitted?]
  (let [now (instant clock) expires (Date. (+ (.getTime ^Date now) ttl-ms))
        row (or (entity db :simmis.worker.assignment/id id) (refuse! ::missing "No such assignment"))]
    (active-enrollment! db worker generation (:simmis.worker.assignment/room row) permitted?)
    (when-not (and (= worker (:simmis.worker.assignment/worker row))
                   (= generation (:simmis.worker.assignment/generation row)))
      (refuse! ::denied "Assignment belongs to another worker"))
    (if (= :queued (:simmis.worker.assignment/state row))
      (do
        ;; A worker is one physical capacity slot. Expiry/quarantine cannot
        ;; free it: another Run needs the preceding owner's shutdown receipt.
        (when (d/q '[:find ?other . :in $ ?worker
                     :where [?other :simmis.worker.assignment/worker ?worker]
                     [?other :simmis.worker.assignment/lease _]
                     (not [?other :simmis.worker.assignment/stop-ack-at _])]
                   db worker)
          (refuse! ::busy "Worker still owns an unresolved process; confirm shutdown before another claim"))
        (updates row {:simmis.worker.assignment/state :claimed :simmis.worker.assignment/nonce nonce
                      :simmis.worker.assignment/secret-hash hash :simmis.worker.assignment/lease lease
                      :simmis.worker.assignment/heartbeat-at now :simmis.worker.assignment/expires-at expires}))
      (if (and (= nonce (:simmis.worker.assignment/nonce row))
               (= hash (:simmis.worker.assignment/secret-hash row)))
        (if (and (= :claimed (:simmis.worker.assignment/state row)) (expired? row now))
          (quarantine row now) [])
        (refuse! ::conflict "Assignment already claimed or withdrawn; never reassign this Run")))))

(defn claim-hashed!
  "An exclusive writer-serialized claim. Client nonce+secret are persisted
   locally first. The nonce IS the lease UUID, so an ambiguous committed claim
   can still acknowledge shutdown after enrollment revocation. A proposed
   bearer grants nothing until this exact proof/lease is durably owned.
   Retries observe this owner, never extend an expired lease."
  [conn id worker generation nonce hash now ttl-ms & [permitted?]]
  (when-not (and (every? uuid? [id worker nonce]) (pos-int? generation)
                 (or (instance? Date now) (fn? now)) (integer? ttl-ms) (<= 1000 ttl-ms 60000)
                 (string? hash) (re-matches #"[A-Za-z0-9_-]{43}=" hash))
    (refuse! ::invalid "Invalid claim"))
  (let [lease nonce]
    (try
      (d/transact conn [[:db.fn/call claim-tx id worker generation nonce hash lease now ttl-ms permitted?]])
      (catch Exception error
        (when (or (contains? #{::denied ::conflict ::missing ::invalid ::busy} (:type (ex-data error)))
                  (not (and (= nonce (:simmis.worker.assignment/nonce (assignment conn id)))
                            (= hash (:simmis.worker.assignment/secret-hash (assignment conn id))))))
          (throw error))))
    (assignment conn id)))

(defn claim! [conn id worker generation nonce secret now ttl-ms]
  (claim-hashed! conn id worker generation nonce (secret-hash secret) now ttl-ms))

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

(defn- withdraw-tx [db row digest ref clock]
  (let [id (:simmis.worker.assignment/id row)
        old (entity db :simmis.worker.assignment/id id)
        now (instant clock)]
    (if old
      (do
        (when-not (= digest (:simmis.worker.assignment/intent-id old))
          (refuse! ::conflict "Cannot close a different assignment intent"))
        (cancel-tx db id now))
      ;; This serialized tombstone also fences an enqueue whose response was
      ;; lost or whose writer work arrives later. insert-tx must never revive it.
      [(assoc row :simmis.worker.assignment/input-ref ref
              :simmis.worker.assignment/state :withdrawn
              :simmis.worker.assignment/cancel-at now
              :simmis.worker.assignment/stop-ack-at now)])))

(defn- publication-closed? [row digest]
  (and (= digest (:simmis.worker.assignment/intent-id row))
       (:simmis.worker.assignment/cancel-at row)
       (if (:simmis.worker.assignment/lease row)
         (contains? #{:quarantined :stopped :completed} (:simmis.worker.assignment/state row))
         (and (= :withdrawn (:simmis.worker.assignment/state row))
              (:simmis.worker.assignment/stop-ack-at row)))))

(defn withdraw!
  "Host-only closure of the SAME publication intent, even after revocation.
   Missing/queued intent becomes withdrawn+ack; an existing lease is quarantined
   without inventing shutdown. A late enqueue cannot revive this Run. Failed
   responses are reconciled by the actual closing state, not digest alone."
  [conn {:keys [id worker generation room request job input]} clock]
  (when-not (and (every? uuid? [id worker room request job]) (pos-int? generation)
                 (map? input) (roster/data-value? input)
                 (or (instance? Date clock) (fn? clock)))
    (refuse! ::invalid "Invalid worker publication closure"))
  (let [digest (hasch/uuid [:simmis/worker-assignment-v1 worker generation room request job input])
        row {:db/id -1 :simmis.worker.assignment/id id
             :simmis.worker.assignment/worker worker :simmis.worker.assignment/generation generation
             :simmis.worker.assignment/room room :simmis.worker.assignment/request request
             :simmis.worker.assignment/job job :simmis.worker.assignment/intent-id digest
             :simmis.worker.assignment/queued-at (Date.)}]
    (try
      (artifact/publish-value! (artifact/datahike-store conn) input
                               (fn [ref] (d/transact conn [[:db.fn/call withdraw-tx row digest ref clock]])))
      (catch Exception error
        (when-not (publication-closed? (assignment conn id) digest) (throw error))))
    (let [result (assignment conn id)]
      (when-not (publication-closed? result digest)
        (refuse! ::unavailable "Worker publication is not durably closed"))
      result)))

(defn- heartbeat-tx [db id lease hash clock ttl-ms permitted?]
  (let [now (instant clock) row (owned! db id lease hash)
        worker (entity db :simmis.worker/id (:simmis.worker.assignment/worker row))]
    (cond
      (:simmis.worker.assignment/stop-ack-at row) []
      (or (:simmis.worker.assignment/cancel-at row) (expired? row now)
          (:simmis.worker/revoked-at worker)
          (and permitted? (not (permitted? db worker)))
          (not= (:simmis.worker/generation worker) (:simmis.worker.assignment/generation row)))
      (quarantine row now)
      :else (updates row {:simmis.worker.assignment/heartbeat-at now
                          :simmis.worker.assignment/expires-at (Date. (+ (.getTime ^Date now) ttl-ms))}))))

(defn heartbeat-hashed!
  "Authority layer must turn lost room permission into cancel!, not renewal.
   Expiry/revocation signals shutdown but never fabricates a stop ack."
  [conn id lease hash now ttl-ms & [permitted?]]
  (checked-hash! hash)
  (when-not (and (or (instance? Date now) (fn? now)) (integer? ttl-ms) (<= 1000 ttl-ms 60000))
    (refuse! ::invalid "Invalid heartbeat"))
  (d/transact conn [[:db.fn/call heartbeat-tx id lease hash now ttl-ms permitted?]])
  (assignment conn id))

(defn heartbeat! [conn id lease secret now ttl-ms]
  (heartbeat-hashed! conn id lease (secret-hash secret) now ttl-ms))

(defn- stopped-tx [db id lease hash now]
  (let [row (owned! db id lease hash)]
    (if (:simmis.worker.assignment/stop-ack-at row) []
        (updates row {:simmis.worker.assignment/state :stopped :simmis.worker.assignment/stop-ack-at now}))))

(defn acknowledge-stop-hashed!
  "Only the existing lease proof may report process-tree shutdown, even after
   enrollment revocation or expiry. This grants NO new input/output access."
  [conn id lease hash now]
  (checked-hash! hash)
  (when-not (instance? Date now) (refuse! ::invalid "Invalid shutdown acknowledgement time"))
  (d/transact conn [[:db.fn/call stopped-tx id lease hash now]])
  (assignment conn id))

(defn acknowledge-stop! [conn id lease secret now]
  (acknowledge-stop-hashed! conn id lease (secret-hash secret) now))

(defn- complete-tx [db id lease hash digest ref clock permitted?]
  (let [now (instant clock) row (owned! db id lease hash)]
    (if-let [old (:simmis.worker.assignment/output-id row)]
      (do (when-not (= old digest) (refuse! ::conflict "Lease already published a different result")) [])
      (do
        (active-enrollment! db (:simmis.worker.assignment/worker row)
                            (:simmis.worker.assignment/generation row) (:simmis.worker.assignment/room row) permitted?)
        (when-not (and (= :claimed (:simmis.worker.assignment/state row))
                       (nil? (:simmis.worker.assignment/cancel-at row)) (not (expired? row now)))
          (refuse! ::denied "Lease is cancelled, expired or stopped; output cannot be admitted"))
        ;; Completion includes the worker's physical-stop acknowledgement.
        ;; The worker must have already stopped the process and descendants.
        (updates row {:simmis.worker.assignment/state :completed :simmis.worker.assignment/output-id digest
                      :simmis.worker.assignment/output-ref ref :simmis.worker.assignment/stop-ack-at now})))))

(defn complete-hashed!
  "Publish validated output only after the worker reports physical shutdown.
   Authority and file/byte limits belong to the host service; this transaction
   fences ownership/revocation/expiry and binds an immutable result digest."
  [conn id lease hash output now & [permitted?]]
  (checked-hash! hash)
  (when-not (and (map? output) (roster/data-value? output) (or (instance? Date now) (fn? now)))
    (refuse! ::invalid "Invalid worker result"))
  (let [digest (hasch/uuid [:simmis/worker-output-v1 output])]
    (try
      (artifact/publish-value! (artifact/datahike-store conn) output
                               (fn [ref] (d/transact conn [[:db.fn/call complete-tx id lease hash digest ref now permitted?]])))
      (catch Exception error
        (when-not (and (matches-hash? (:simmis.worker.assignment/secret-hash (assignment conn id)) hash)
                       (= lease (:simmis.worker.assignment/lease (assignment conn id)))
                       (= digest (:simmis.worker.assignment/output-id (assignment conn id))))
          (throw error))))
    (assignment conn id)))

(defn complete! [conn id lease secret output now]
  (complete-hashed! conn id lease (secret-hash secret) output now))
