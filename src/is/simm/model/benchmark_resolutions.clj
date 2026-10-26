(ns is.simm.model.benchmark-resolutions
  "Pure HOST preparation journal. These fragments neither retain physical
   checkpoints nor authorize execution. The fixed HOST must verify artifacts,
   source authority, root readback and native cleanup before advancing them."
  (:require [datahike.api :as d]
            [hasch.core :as hasch]
            [is.simm.model.work-requests :as requests]
            [malli.core :as m]))

(def schema
  (mapv (fn [[ident type]]
          (cond-> {:db/ident ident :db/valueType type :db/cardinality :db.cardinality/one}
            (#{:benchmark.resolution/id :benchmark.resolution/request
               :benchmark.case/id :benchmark.case/root} ident)
            (assoc :db/unique :db.unique/value)
            (= :benchmark.case/resolution ident) (assoc :db/index true)))
        [[:benchmark.resolution/id :db.type/uuid]
         [:benchmark.resolution/request :db.type/uuid]
         [:benchmark.resolution/by :db.type/uuid]
         [:benchmark.resolution/room :db.type/uuid]
         [:benchmark.resolution/client-id :db.type/uuid]
         [:benchmark.resolution/job :db.type/uuid]
         [:benchmark.resolution/request-claim :db.type/uuid]
         [:benchmark.resolution/request-intent :db.type/uuid]
         [:benchmark.resolution/claim :db.type/uuid]
         [:benchmark.resolution/state :db.type/keyword]
         [:benchmark.resolution/semantic-id :db.type/uuid]
         [:benchmark.resolution/plan :db.type/store-ref]
         [:benchmark.resolution/plan-id :db.type/uuid]
         [:benchmark.resolution/resolved :db.type/store-ref]
         [:benchmark.resolution/resolved-id :db.type/uuid]
         [:benchmark.case/id :db.type/uuid]
         [:benchmark.case/resolution :db.type/uuid]
         [:benchmark.case/ordinal :db.type/long]
         [:benchmark.case/claim :db.type/uuid]
         [:benchmark.case/state :db.type/keyword]
         [:benchmark.case/root :db.type/uuid]
         [:benchmark.case/selection :db.type/store-ref]
         [:benchmark.case/selection-id :db.type/uuid]]))

(def plan-schema
  [:map {:closed true}
   [:version [:= 1]] [:semantic-id :uuid] [:source-id :uuid]
   [:substitution [:= :clean-selected-fixture-checkpoint-v1]]
   [:cases [:vector {:min 1 :max 8}
            [:map {:closed true} [:ordinal [:int {:min 0 :max 7}]]
             [:case [:or [:map {:closed true} [:kind [:= :default]]]
                     [:map {:closed true} [:kind [:= :named]]
                      [:name [:string {:min 1 :max 256}]]]]]
             [:manifest-id :uuid]]]]])

(defn- refuse! [reason]
  (throw (ex-info "Benchmark preparation is held" {:type ::held :reason reason})))

(defn validate-plan! [plan]
  (when-not (and (m/validate plan-schema plan)
                 (= (mapv :ordinal (:cases plan)) (vec (range (count (:cases plan)))))
                 (apply distinct? (map :case (:cases plan))))
    (refuse! :invalid-plan))
  plan)

(defn plan-id [plan] (hasch/uuid [:simmis/benchmark-resolution-plan-v1 (validate-plan! plan)]))
(defn case-id [resolution ordinal] (hasch/uuid [:simmis/benchmark-case-v1 resolution ordinal]))

(defn- row [db attr id]
  (when-let [eid (d/q '[:find ?e . :in $ ?a ?id :where [?e ?a ?id]] db attr id)]
    (d/pull db '[*] eid)))

(defn resolution [db request-id] (row db :benchmark.resolution/request request-id))

(defn cases [db resolution-id]
  (->> (d/datoms db :avet :benchmark.case/resolution resolution-id)
       (take 9) (mapv #(d/pull db '[*] (:e %)))
       (sort-by :benchmark.case/ordinal) vec))

(defn- request! [db expected]
  (when-not (and expected (= expected (requests/request-in db (:work.request/id expected)))
                 (= :submitted (:work.request/state expected))
                 (= :benchmark (:work.request/kind expected))
                 (true? (:work.request/return-pending? expected))
                 (every? uuid? (map expected [:work.request/id :work.request/job
                                              :work.request/claim :work.request/intent-id])))
    (refuse! :request-changed)))

(defn- request-binding [request]
  {:benchmark.resolution/request (:work.request/id request)
   :benchmark.resolution/by (:work.request/by request)
   :benchmark.resolution/room (:work.request/room request)
   :benchmark.resolution/client-id (:work.request/client-id request)
   :benchmark.resolution/job (:work.request/job request)
   :benchmark.resolution/request-claim (:work.request/claim request)
   :benchmark.resolution/request-intent (:work.request/intent-id request)})

(defn- current! [db request expected plan]
  (request! db request)
  (when-not (and expected (= expected (resolution db (:work.request/id request)))
                 (= (request-binding request) (select-keys expected (keys (request-binding request))))
                 (= (plan-id plan) (:benchmark.resolution/plan-id expected))
                 (= (:semantic-id plan) (:benchmark.resolution/semantic-id expected)))
    (refuse! :resolution-changed))
  expected)

(defn prepare-tx
  "Reserve every case root before any native effect. A no-op replay conveys
   no construction right; only the original fresh resolution/claim owns IO.
   HOST supplies a verified bounded plan artifact, not arbitrary source code."
  [db request plan plan-ref {:keys [id claim cases] :as tokens}]
  (request! db request)
  (validate-plan! plan)
  (let [digest (plan-id plan)]
    (if-let [existing (resolution db (:work.request/id request))]
      (do (current! db request existing plan) [])
      (do
        (when-not (and (= #{:id :claim :cases} (set (keys tokens)))
                       (every? uuid? [id claim]) (vector? cases)
                       (= (count (:cases plan)) (count cases))
                       (every? #(and (= #{:claim :root} (set (keys %)))
                                     (every? uuid? (vals %))) cases)
                       (apply distinct? (map :root cases))
                       (apply distinct? (map :claim cases)))
          (refuse! :invalid-tokens))
        (when (or (row db :benchmark.resolution/id id)
                  (some #(row db :benchmark.case/id (case-id id %)) (range (count cases)))
                  (some #(row db :benchmark.case/root (:root %)) cases))
          (refuse! :identity-reused))
        (into [(merge (request-binding request)
                      {:benchmark.resolution/id id :benchmark.resolution/claim claim
                       :benchmark.resolution/state :preparing
                       :benchmark.resolution/semantic-id (:semantic-id plan)
                       :benchmark.resolution/plan plan-ref :benchmark.resolution/plan-id digest})]
              (map-indexed (fn [ordinal token]
                             {:benchmark.case/id (case-id id ordinal)
                              :benchmark.case/resolution id :benchmark.case/ordinal (long ordinal)
                              :benchmark.case/claim (:claim token) :benchmark.case/root (:root token)
                              :benchmark.case/state :planned}) cases))))))

(defn- case! [db resolution plan expected]
  (when-not (and expected (= expected (row db :benchmark.case/id (:benchmark.case/id expected)))
                 (integer? (:benchmark.case/ordinal expected))
                 (<= 0 (:benchmark.case/ordinal expected) (dec (count (:cases plan))))
                 (= (:benchmark.resolution/id resolution) (:benchmark.case/resolution expected))
                 (= (:benchmark.case/id expected)
                    (case-id (:benchmark.resolution/id resolution) (:benchmark.case/ordinal expected))))
    (refuse! :case-changed))
  expected)

(defn case-pinning-tx
  "Selection has already been published and verified by HOST. Reserve its
   exact digest before pin IO. Replays never grant another native attempt."
  [db request expected plan expected-case selection-id selection-ref]
  (current! db request expected plan)
  (case! db expected plan expected-case)
  (when-not (and (= :preparing (:benchmark.resolution/state expected)) (uuid? selection-id))
    (refuse! :invalid-state))
  (case (:benchmark.case/state expected-case)
    :planned [{:db/id (:db/id expected-case) :benchmark.case/state :pinning
               :benchmark.case/selection selection-ref :benchmark.case/selection-id selection-id}]
    (:pinning :retained)
    (do (when-not (and (= selection-id (:benchmark.case/selection-id expected-case))
                       (= selection-ref (:benchmark.case/selection expected-case)))
          (refuse! :selection-changed)) [])
    (refuse! :invalid-state)))

(defn case-retained-tx
  "HOST-only ACK after full physical root readback. This state transition is
   not itself proof of pinning; a failed/ambiguous pin must remain held."
  [db request expected plan expected-case]
  (current! db request expected plan)
  (case! db expected plan expected-case)
  (when-not (= :preparing (:benchmark.resolution/state expected)) (refuse! :invalid-state))
  (case (:benchmark.case/state expected-case)
    :pinning [{:db/id (:db/id expected-case) :benchmark.case/state :retained}]
    :retained []
    (refuse! :invalid-state)))

(defn ready-tx
  "HOST-only after every builder cleanup ACK and resolved-artifact validation.
   Exact retained cases are checked atomically with the ready pointer. Evidence
   remains retained after settlement; there is no automatic resume or expiry."
  [db request expected plan expected-cases resolved-id resolved-ref]
  (current! db request expected plan)
  (when-not (and (uuid? resolved-id) (vector? expected-cases)
                 (= expected-cases (cases db (:benchmark.resolution/id expected)))
                 (= (count (:cases plan)) (count expected-cases))
                 (= (mapv :ordinal (:cases plan)) (mapv :benchmark.case/ordinal expected-cases))
                 (every? #(and (= :retained (:benchmark.case/state %))
                                (uuid? (:benchmark.case/selection-id %))) expected-cases))
    (refuse! :cases-not-retained))
  (doseq [c expected-cases] (case! db expected plan c))
  (case (:benchmark.resolution/state expected)
    :preparing [{:db/id (:db/id expected) :benchmark.resolution/state :ready
                 :benchmark.resolution/resolved-id resolved-id :benchmark.resolution/resolved resolved-ref}]
    :ready (do (when-not (and (= resolved-id (:benchmark.resolution/resolved-id expected))
                              (= resolved-ref (:benchmark.resolution/resolved expected)))
                 (refuse! :resolved-changed)) [])
    (refuse! :invalid-state)))
