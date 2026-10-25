(ns is.simm.model.knowledge-pins
  "Internal durable intents for exclusively owned, never-reused GC root IDs.
   This journal performs no root IO, authorization, cursor movement or dispatch.
   One active intent per stream bounds uncertain pin creation and cleanup."
  (:require [datahike.api :as d]
            [dvergr.artifact :as artifact]
            [hasch.core :as hasch]
            [is.simm.model.knowledge-streams :as streams]))

(def schema
  (mapv (fn [[ident type]]
          (cond-> {:db/ident ident :db/valueType type :db/cardinality :db.cardinality/one}
            (#{:knowledge.pin/id :knowledge.pin/active-stream} ident) (assoc :db/unique :db.unique/value)))
        [[:knowledge.pin/id :db.type/uuid] [:knowledge.pin/stream :db.type/uuid]
         [:knowledge.pin/active-stream :db.type/uuid] [:knowledge.pin/claim :db.type/uuid]
         [:knowledge.pin/state :db.type/keyword] [:knowledge.pin/payload :db.type/store-ref]
         [:knowledge.pin/payload-id :db.type/uuid] [:knowledge.pin/discovery :db.type/uuid]]))

(defn- refuse! [reason]
  (throw (ex-info "Knowledge checkpoint intent refused the transition" {:type ::refused :reason reason})))

(defn- row [db attr value]
  (when-let [eid (d/q '[:find ?e . :in $ ?attr ?value :where [?e ?attr ?value]] db attr value)]
    (d/pull db '[*] eid)))

(defn intent [db id] (row db :knowledge.pin/id id))
(defn active [db stream] (row db :knowledge.pin/active-stream stream))

(defn- prepare-tx [db value expected payload-id ref root claim]
  (when-not (= value (streams/descriptor (dissoc value :id))) (refuse! :invalid-stream))
  (when-not (= expected (streams/stream db (:id value))) (refuse! :cursor-conflict))
  (when (:knowledge.stream/active-batch expected) (refuse! :batch-active))
  (when (d/q '[:find ?e . :in $ ?stream :where [?e :knowledge.item/stream ?stream]
               [?e :knowledge.item/state :claimed]] db (:id value))
    (refuse! :orphaned-claim))
  (when-let [previous (:knowledge.stream/last-discovery expected)]
    (when-not (true? (:knowledge.discovery/old-root-released?
                     (row db :knowledge.discovery/id previous)))
      (refuse! :cleanup-pending)))
  (if-let [existing (active db (:id value))]
    (if (= payload-id (:knowledge.pin/payload-id existing))
      []
      (refuse! :pin-intent-busy))
    (do
      (when (intent db root) (refuse! :root-id-reused))
      [{:knowledge.pin/id root :knowledge.pin/stream (:id value) :knowledge.pin/active-stream (:id value)
        :knowledge.pin/claim claim :knowledge.pin/state :creating
        :knowledge.pin/payload ref :knowledge.pin/payload-id payload-id}])))

(defn prepare!
  "HOST must validate the published basis and owned target before calling.
   Only :new? winner may invoke native pin!. Missing roots after uncertainty or
   restart are held, never recreated. Payload retains original cursor/basis."
  [conn value expected basis]
  (streams/validate-binding! value expected)
  (streams/validate-basis! basis)
  (let [payload {:version 1 :stream value :expected expected :basis basis}
        digest (hasch/uuid payload) root (random-uuid) claim (random-uuid)]
    (when (> (alength (.getBytes ^String (pr-str payload) "UTF-8")) 16384)
      (refuse! :intent-too-large))
    (artifact/publish-value!
     (artifact/datahike-store conn) payload
     (fn [ref] (d/transact conn [[:db.fn/call prepare-tx value expected digest ref root claim]])))
    (let [stored (active @conn (:id value))]
      (when-not (= digest (:knowledge.pin/payload-id stored)) (refuse! :intent-changed))
      {:intent stored :new? (and (= root (:knowledge.pin/id stored)) (= claim (:knowledge.pin/claim stored)))})))

(defn payload
  "Read and verify one retained intent. Hash integrity alone grants no cleanup
   authority; HOST must match the exact physical store, root record and owner."
  [conn row]
  (let [value (artifact/get-bounded-value (artifact/datahike-store conn) (:knowledge.pin/payload row) 16384)
        body (:value value)]
    (when-not (and (map? body) (= (:knowledge.pin/payload-id row) (hasch/uuid body))
                   (= (:knowledge.pin/stream row) (get-in body [:stream :id]))
                   (= #{:version :stream :expected :basis} (set (keys body))) (= 1 (:version body)))
      (refuse! :invalid-retained-intent))
    (streams/validate-binding! (:stream body) (:expected body))
    (streams/validate-basis! (:basis body))
    body))

(defn mark-pinned-tx
  "HOST-only composition AFTER exact registry+record confirmation. No root IO."
  [db expected]
  (let [current (intent db (:knowledge.pin/id expected))]
    (when-not (= current expected) (refuse! :intent-changed))
    (case (:knowledge.pin/state current)
      :creating [{:db/id (:db/id current) :knowledge.pin/state :pinned}]
      :pinned []
      (refuse! :invalid-state))))

(defn discovery-tx
  "Compose with the existing discovery journal in the SAME writer transaction,
   after HOST pin/plan verification. An exact historical replay is harmless;
   original intent payload and expected stream must still match exactly."
  [db expected-intent value expected-stream plan plan-ref]
  (let [root (:knowledge.pin/id expected-intent) current (intent db root)
        did (streams/discovery-id value expected-stream plan root)]
    (when-not (and (= (:knowledge.pin/stream current) (:id value))
                   (= (:knowledge.pin/claim current) (:knowledge.pin/claim expected-intent))
                   (= (:knowledge.pin/payload-id current) (:knowledge.pin/payload-id expected-intent)
                      (hasch/uuid {:version 1 :stream value :expected expected-stream :basis (:after plan)})))
      (refuse! :intent-changed))
    (case (:knowledge.pin/state current)
      :pinned
      (do
        (when-not (= current expected-intent) (refuse! :intent-changed))
        (into [{:db/id (:db/id current) :knowledge.pin/state :committed :knowledge.pin/discovery did}]
              (streams/discovery-tx db value expected-stream plan root plan-ref)))
      (:committed :retained :released)
      (do (when-not (= did (:knowledge.pin/discovery current)) (refuse! :discovery-mismatch))
          (streams/discovery-tx db value expected-stream plan root plan-ref))
      (refuse! :invalid-state))))

(defn finish-tx
  "HOST-only after exact old-root release/readback. Clears the stream intent
   lock, retaining its immutable owner/basis receipt. This does NOT release a
   native root and does not certify source processing."
  [db expected]
  (let [current (intent db (:knowledge.pin/id expected))
        event (row db :knowledge.discovery/id (:knowledge.pin/discovery current))
        stream (streams/stream db (:knowledge.pin/stream current))
        old (:knowledge.discovery/before-root event)
        old-intent (when old (intent db old))
        binding [:db/id :knowledge.pin/id :knowledge.pin/stream :knowledge.pin/claim
                 :knowledge.pin/payload :knowledge.pin/payload-id :knowledge.pin/discovery]]
    (if (and (#{:retained :released} (:knowledge.pin/state current))
             (= (select-keys current binding) (select-keys expected binding))
             (true? (:knowledge.discovery/old-root-released? event)))
      []
      (do
       (when-not (= current expected) (refuse! :intent-changed))
       (when-not (and (= :committed (:knowledge.pin/state current))
                   (= (:knowledge.pin/id current) (:knowledge.stream/cursor-root stream)
                      (:knowledge.discovery/after-root event))
                   (= (:knowledge.pin/discovery current) (:knowledge.stream/last-discovery stream))
                   (or (nil? old) (and (= :retained (:knowledge.pin/state old-intent))
                                       (= (:knowledge.pin/stream current) (:knowledge.pin/stream old-intent)))))
         (refuse! :cleanup-binding-mismatch))
       (cond-> [{:db/id (:db/id current) :knowledge.pin/state :retained}
             [:db/retract (:db/id current) :knowledge.pin/active-stream (:knowledge.pin/stream current)]
             {:db/id (:db/id event) :knowledge.discovery/old-root-released? true}]
         old (conj {:db/id (:db/id old-intent) :knowledge.pin/state :released}))))))
