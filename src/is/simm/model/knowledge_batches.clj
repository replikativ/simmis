(ns is.simm.model.knowledge-batches
  "Inactive canonical batch claim/freeze/request-link fragments. Not an
   executor, dispatch permission, artifact verifier or processing certificate.
   HOST composes current authorization in the SAME serialized transaction."
  (:require [datahike.api :as d]
            [hasch.core :as hasch]
            [is.simm.model.knowledge-pins :as pins]
            [is.simm.model.knowledge-streams :as streams]
            [is.simm.model.work-request-identity :as identity]
            [malli.core :as m]))

(def schema
  (into
    (mapv (fn [[ident type]]
            (cond-> {:db/ident ident :db/valueType type :db/cardinality :db.cardinality/one}
              (= ident :knowledge.batch/id) (assoc :db/unique :db.unique/value)
              (= ident :knowledge.batch/stream) (assoc :db/index true)))
          [[:knowledge.batch/id :db.type/uuid] [:knowledge.batch/stream :db.type/uuid]
           [:knowledge.batch/by :db.type/uuid] [:knowledge.batch/room :db.type/uuid]
           [:knowledge.batch/target :db.type/uuid] [:knowledge.batch/claim :db.type/uuid]
           [:knowledge.batch/state :db.type/keyword]
           [:knowledge.batch/selection :db.type/store-ref] [:knowledge.batch/selection-id :db.type/uuid]
           [:knowledge.batch/frozen :db.type/store-ref] [:knowledge.batch/frozen-id :db.type/uuid]
           [:knowledge.batch/request :db.type/uuid] [:knowledge.batch/request-intent :db.type/uuid]])
    [{:db/ident :knowledge.item/batch :db/valueType :db.type/uuid :db/cardinality :db.cardinality/one}]))

(defn- refuse! [reason]
  (throw (ex-info "Knowledge batch transition is held" {:type ::held :reason reason})))

(defn- row [db attr value]
  (when-let [eid (:e (first (d/datoms db :avet attr value)))]
    (d/pull db '[*] eid)))

(defn batch [db id] (row db :knowledge.batch/id id))

(defn- portable [row]
  {:id (:knowledge.item/id row) :order (:knowledge.item/order row)
   :path (:knowledge.item/path row) :content (:knowledge.item/content row)
   :size (:knowledge.item/size row) :mode (:knowledge.item/mode row)})

(defn- selection! [value]
  ;; Full source/context/byte validation is a HOST prerequisite. These closed
  ;; writer checks establish the exact immutable claim and actual pending prefix,
  ;; not authority from a model-authored manifest or matching digest.
  (when-not (and (map? value)
                 (= #{:version :stream :expected :basis :batch-policy :reserved :items :remaining :evidence :id}
                    (set (keys value)))
                 (= 2 (:version value))
                 (vector? (:items value)) (<= 1 (count (:items value)) 99)
                 (integer? (:remaining value))
                 (<= 0 (:remaining value) (- streams/max-changes (count (:items value))))
                 (= (:id value) (hasch/uuid [:simmis/knowledge-batch-selection-v2 (dissoc value :id)])))
    (refuse! :invalid-selection))
  (streams/validate-binding! (:stream value) (:expected value))
  (streams/validate-basis! (:basis value))
  (when-not (:expected value) (refuse! :missing-stream))
  value)

(defn claim-tx
  "Before reading source bodies. Claim the whole stream and exact pending
   prefix atomically; only an acknowledged random-token winner may capture.
   Existing exact claims replay without authorizing capture or dispatch."
  [db selection ref claim]
  (selection! selection)
  (when-not (and (uuid? claim) (uuid? ref)) (refuse! :invalid-claim))
  (let [{:keys [stream expected basis items remaining id]} selection
        current (streams/stream db (:id stream))
        existing (batch db id)]
    (if existing
      (do
        (when-not (and (= id (:knowledge.batch/selection-id existing))
                       (= (:id stream) (:knowledge.batch/stream existing))
                       (= [(:by stream) (:room stream) (:target stream)]
                          [(:knowledge.batch/by existing) (:knowledge.batch/room existing) (:knowledge.batch/target existing)])
                       (= ref (:knowledge.batch/selection existing)))
          (refuse! :batch-conflict))
        [])
      (let [pending (streams/pending-items db (:id stream))
            selected (vec (take (count items) pending))
            pin (when current (pins/intent db (:knowledge.stream/cursor-root current)))
            discovery (when current (row db :knowledge.discovery/id (:knowledge.stream/last-discovery current)))]
        (when-not (and (= expected current)
                       (nil? (:knowledge.stream/active-batch current))
                       (nil? (pins/active db (:id stream)))
                       (= :retained (:knowledge.pin/state pin))
                       (= (:id stream) (:knowledge.pin/stream pin))
                       (= (:knowledge.stream/last-discovery current) (:knowledge.pin/discovery pin))
                       (true? (:knowledge.discovery/old-root-released? discovery))
                       (= [(:commit basis) (:max-tx basis) (streams/source-id basis)]
                          [(:knowledge.stream/cursor-commit current) (:knowledge.stream/cursor-tx current)
                           (:knowledge.stream/source current)]))
          (refuse! :stream-not-ready))
        (when-not (and (= items (mapv portable selected))
                       (= remaining (- (count pending) (count items))))
          (refuse! :pending-prefix-changed))
        (into [{:knowledge.batch/id id :knowledge.batch/stream (:id stream)
                :knowledge.batch/by (:by stream) :knowledge.batch/room (:room stream)
                :knowledge.batch/target (:target stream) :knowledge.batch/claim claim
                :knowledge.batch/state :capturing :knowledge.batch/selection ref :knowledge.batch/selection-id id}
               {:db/id (:db/id (row db :knowledge.stream/id (:id stream)))
                :knowledge.stream/active-batch id}]
              (mapcat (fn [item]
                        [{:db/id (:db/id item) :knowledge.item/state :claimed :knowledge.item/batch id}
                         [:db/retract (:db/id item) :knowledge.item/pending-stream (:id stream)]]) selected))))))

(defn claim-result
  "Readback only. The original physically exclusive HOST invocation must still
   own this operation; persisted random tokens alone are not capabilities."
  [db id claim]
  (let [row (batch db id)]
    (when-not row (refuse! :claim-unconfirmed))
    {:batch row :new? (and (= :capturing (:knowledge.batch/state row))
                           (= claim (:knowledge.batch/claim row)))}))

(def ^:private immutable-fields
  [:db/id :knowledge.batch/id :knowledge.batch/stream :knowledge.batch/by :knowledge.batch/room
   :knowledge.batch/target :knowledge.batch/claim :knowledge.batch/selection :knowledge.batch/selection-id])

(defn freeze-tx
  "After HOST verification and artifact publication of the exact v2 envelope.
   Does no artifact IO. Requires the original acknowledged claim row."
  [db expected frozen-ref frozen-id]
  (let [current (batch db (:knowledge.batch/id expected))]
    (when-not (and (uuid? frozen-ref) (uuid? frozen-id)
                   (every? #(contains? expected %) immutable-fields)
                   (= (select-keys expected immutable-fields) (select-keys current immutable-fields)))
      (refuse! :claim-changed))
    (if (and (#{:ready :submitted} (:knowledge.batch/state current))
             (= frozen-ref (:knowledge.batch/frozen current))
             (= frozen-id (:knowledge.batch/frozen-id current)))
      []
      (do
        (when-not (and (= expected current) (= :capturing (:knowledge.batch/state current))
                       (not-any? #(contains? current %) [:knowledge.batch/frozen :knowledge.batch/frozen-id])
                       (= (:knowledge.batch/id current)
                          (:knowledge.stream/active-batch (streams/stream db (:knowledge.batch/stream current)))))
          (refuse! :batch-not-capturing))
        [{:db/id (:db/id current) :knowledge.batch/state :ready
          :knowledge.batch/frozen frozen-ref :knowledge.batch/frozen-id frozen-id}]))))

(def origin-schema
  [:map {:closed true} [:batch :uuid] [:selection :uuid] [:frozen :uuid]])

(defn request-tx
  "Fixed composition for the canonical workflow request claim. No benchmark
   or generic request can consume a live batch. A different request is held;
   matching replay never starts a Job. Not yet wired into request admission."
  [db origin request intent-id]
  (when-not (and (m/validate origin-schema origin) (uuid? intent-id)
                 (= :workflow (:work.request/kind request))
                 (every? uuid? (map request [:work.request/id :work.request/client-id :work.request/by :work.request/room]))
                 (= intent-id (:work.request/intent-id request))
                 (= (:batch origin) (:work.request/knowledge-batch request))
                 (= (:frozen origin) (:work.request/knowledge-frozen request))
                 (= (:work.request/id request)
                    (identity/request-id (:work.request/by request) (:work.request/room request) (:work.request/client-id request))))
    (refuse! :invalid-request-origin))
  (let [current (batch db (:batch origin))
        stream (when current (streams/stream db (:knowledge.batch/stream current)))
        recorded (row db :work.request/id (:work.request/id request))
        binding [:work.request/id :work.request/client-id :work.request/intent-id
                 :work.request/knowledge-batch :work.request/knowledge-frozen
                 :work.request/by :work.request/room :work.request/kind]]
    (when-not (and current
                   (= (:selection origin) (:knowledge.batch/selection-id current))
                   (= (:frozen origin) (:knowledge.batch/frozen-id current))
                   (= (:work.request/by request) (:knowledge.batch/by current))
                   (= (:work.request/room request) (:knowledge.batch/room current))
                   (= [(:knowledge.batch/by current) (:knowledge.batch/room current) (:knowledge.batch/target current)]
                      [(:knowledge.stream/by stream) (:knowledge.stream/room stream) (:knowledge.stream/target stream)]))
      (refuse! :request-origin-mismatch))
    (if (:knowledge.batch/request current)
      (do (when-not (and recorded (= (select-keys request binding) (select-keys recorded binding))
                         (= [(:work.request/id request) intent-id]
                            [(:knowledge.batch/request current) (:knowledge.batch/request-intent current)]))
            (refuse! :request-conflict))
          [])
      (do
        ;; The request insert and first link must be joint. Never repair an
        ;; unrelated/partial preexisting request by manufacturing a linkage.
        (when recorded (refuse! :partial-request-link))
        (when-not (and (= :ready (:knowledge.batch/state current))
                       (= (:batch origin) (:knowledge.stream/active-batch stream)))
          (refuse! :inputs-not-ready))
        [{:db/id (:db/id current) :knowledge.batch/state :submitted
          :knowledge.batch/request (:work.request/id request) :knowledge.batch/request-intent intent-id}]))))
