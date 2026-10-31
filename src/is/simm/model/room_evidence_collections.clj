(ns is.simm.model.room-evidence-collections
  "Typed pre-request collection receipts. No work request or processing state."
  (:require [datahike.api :as d] [hasch.core :as hasch]
            [dvergr.artifact :as artifact]))

(def schema
  (mapv (fn [[ident type]]
          (cond-> {:db/ident ident :db/valueType type :db/cardinality :db.cardinality/one}
            (= ident :room.collection/id) (assoc :db/unique :db.unique/value)))
        [[:room.collection/id :db.type/uuid] [:room.collection/intent :db.type/store-ref]
         [:room.collection/intent-id :db.type/uuid] [:room.collection/claim :db.type/uuid]
         [:room.collection/state :db.type/keyword] [:room.collection/created :db.type/instant]
         [:room.collection/updated :db.type/instant] [:room.collection/proof :db.type/keyword]
         [:room.collection/native-ack :db.type/store-ref]]))

(defn receipt [db id]
  (when-let [e (:e (first (d/datoms db :avet :room.collection/id id)))] (d/pull db '[*] e)))
(defn- held! [reason] (throw (ex-info "Room collection held" {:type ::held :reason reason})))
(defn claim-tx [db intent ref token now]
  (when-not (and (= 1 (:version intent)) (uuid? (:request intent)) (uuid? ref) (uuid? token)
                 (instance? java.util.Date now)) (held! :invalid-intent))
  (let [id (:request intent) digest (hasch/uuid intent)]
    (if-let [old (receipt db id)]
      (do (when-not (= digest (:room.collection/intent-id old)) (held! :intent-conflict)) [])
      [{:room.collection/id id :room.collection/intent ref :room.collection/intent-id digest
        :room.collection/claim token :room.collection/state :claimed
        :room.collection/created now :room.collection/updated now}])))
(defn claim-result [db id token]
  (let [row (receipt db id)]
    (when-not row (held! :claim-unconfirmed))
    {:receipt row :winner? (and (= :claimed (:room.collection/state row))
                               (= token (:room.collection/claim row)))}))
(defn acknowledge-tx [db expected ack now]
  (let [current (receipt db (:room.collection/id expected))]
    (when-not (and (= expected current) (= :claimed (:room.collection/state current))
                   (uuid? ack) (instance? java.util.Date now))
      (held! :receipt-changed))
    [{:db/id (:db/id current) :room.collection/native-ack ack :room.collection/updated now}]))

(defn available-tx [db expected proof ack now]
  (let [current (receipt db (:room.collection/id expected))]
    (when-not (and (= current expected) (#{:claimed :available} (:room.collection/state current))
                   (#{:write-acknowledged :observed-present} proof)
                   (or (nil? ack) (uuid? ack)) (instance? java.util.Date now))
      (held! :receipt-changed))
    [(cond-> {:db/id (:db/id current) :room.collection/state :available
              :room.collection/proof proof :room.collection/updated now}
       ack (assoc :room.collection/native-ack ack))]))


(defn link-current-in!
  "Pure canonical-writer receipt row gate. Artifact IO belongs outside writer."
  [db canonical-request value]
  (let [row (receipt db canonical-request)]
      (when-not (and (map? value) (= #{:collection :intent :proof} (set (keys value)))
                     (every? uuid? [canonical-request (:collection value) (:intent value)])
                     (#{:write-acknowledged :observed-present} (:proof value))
                     (= canonical-request (:collection value))
                     (= :available (:room.collection/state row))
                     (= [(:collection value) (:intent value) (:proof value)]
                        (mapv row [:room.collection/id :room.collection/intent-id :room.collection/proof])))
        (held! :receipt-unavailable)))
  value)

(defn checked-link!
  "Before request creation: verify retained availability and exact future
   canonical request binding. Grants no write or processing permission."
  [control by room canonical-request value]
  (link-current-in! @control canonical-request value)
  (let [row (receipt @control canonical-request)
          intent (:value (artifact/get-bounded-value (artifact/datahike-store control)
                                                     (:room.collection/intent row) (* 4 1024 1024)))]
      (when-not (and (= :available (:room.collection/state row))
                     (= [(:collection value) (:intent value) (:proof value)]
                        (mapv row [:room.collection/id :room.collection/intent-id :room.collection/proof]))
                     (= (:intent value) (hasch/uuid intent))
                     (= [canonical-request by room] (mapv intent [:request :actor :source-room])))
        (held! :receipt-unavailable)))
  value)
