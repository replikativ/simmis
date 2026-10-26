(ns is.simm.model.knowledge-stream-retirement
  "Clean-stream retirement fragments. The canonical fence precedes native root
   release. HOST retains original physical ownership and observes root absence
   before finishing; these pure fragments perform no IO or authorization."
  (:require [datahike.api :as d]
            [hasch.core :as hasch]
            [is.simm.model.knowledge-pins :as pins]
            [is.simm.model.knowledge-streams :as streams]
            [malli.core :as m]))

(def schema
  (mapv (fn [[ident type]] {:db/ident ident :db/valueType type :db/cardinality :db.cardinality/one})
        [[:knowledge.pin/retirement-id :db.type/uuid]
         [:knowledge.pin/retirement-client :db.type/uuid]
         [:knowledge.pin/retirement-by :db.type/uuid]
         [:knowledge.pin/retirement-at :db.type/instant]
         [:knowledge.pin/retirement-reason :db.type/string]
         [:knowledge.pin/retirement-stream-binding :db.type/uuid]
         [:knowledge.pin/retirement-pin-binding :db.type/uuid]]))

(def ^:private decision-schema
  [:map {:closed true} [:id :uuid] [:stream :uuid] [:root :uuid] [:by :uuid] [:client :uuid]
   [:at [:fn inst?]] [:reason [:string {:min 1 :max 1000}]]])

(defn- refuse! [reason]
  (throw (ex-info "Knowledge stream retirement is held" {:type ::held :reason reason})))

(defn decision [facts]
  (when (contains? facts :id) (refuse! :invalid-decision))
  (when-not (m/validate decision-schema (assoc facts :id #uuid "00000000-0000-0000-0000-000000000000"))
    (refuse! :invalid-decision))
  (assoc facts :id (hasch/uuid [:simmis/knowledge-stream-retirement-v1
                               (select-keys facts [:stream :root :by :client])])))

(def ^:private pin-fields
  [:db/id :knowledge.pin/id :knowledge.pin/stream :knowledge.pin/claim
   :knowledge.pin/by :knowledge.pin/room :knowledge.pin/target
   :knowledge.pin/payload :knowledge.pin/payload-id :knowledge.pin/discovery])

(defn expected
  "Inert comparisons from canonical stream/pin and its HOST-verified payload."
  [stream pin payload]
  {:stream (dissoc stream :knowledge.stream/state)
   :pin (select-keys pin pin-fields) :value (:stream payload) :basis (:basis payload)})

(defn- checked! [db expected value]
  (when-not (and (= #{:stream :pin :value :basis} (set (keys expected)))
                 (= (set pin-fields) (set (keys (:pin expected))))
                 (m/validate decision-schema value)
                 (= (:id value) (:id (decision (dissoc value :id)))))
    (refuse! :invalid-decision))
  (let [s (streams/stream db (:stream value)) p (pins/intent db (:root value))
        descriptor (:value expected) basis (:basis expected)
        event (when (uuid? (:knowledge.pin/discovery p))
                (d/pull db '[*] [:knowledge.discovery/id (:knowledge.pin/discovery p)]))]
    (when-not (and s p (= (:stream expected) (dissoc s :knowledge.stream/state))
                   (= (:pin expected) (select-keys p pin-fields))
                   (= (:stream value) (:id descriptor) (:knowledge.pin/stream p))
                   (= (:root value) (:knowledge.stream/cursor-root s) (:knowledge.discovery/after-root event))
                   (= [(:by descriptor) (:room descriptor) (:target descriptor)]
                      (mapv p [:knowledge.pin/by :knowledge.pin/room :knowledge.pin/target]))
                   (= (:knowledge.pin/discovery p) (:knowledge.stream/last-discovery s))
                   (= (:stream value) (:knowledge.discovery/stream event))
                   (= (:knowledge.discovery/revision event) (:knowledge.stream/revision s))
                   (true? (:knowledge.discovery/old-root-released? event))
                   (not (contains? p :knowledge.pin/active-stream))
                   (#{:active :retiring :retired} (:knowledge.stream/state s)))
      (refuse! :stream-or-pin-changed))
    (streams/validate-binding! descriptor s)
    (streams/validate-basis! basis)
    (when-not (= [(:commit basis) (:max-tx basis) (streams/source-id basis)]
                 (mapv s [:knowledge.stream/cursor-commit :knowledge.stream/cursor-tx :knowledge.stream/source]))
      (refuse! :cursor-basis-changed))
    (when (or (:knowledge.stream/active-batch s) (pins/active db (:stream value))
              (seq (take 1 (d/datoms db :avet :knowledge.item/pending-stream (:stream value))))
              (d/q '[:find ?e . :in $ ?stream [?state ...]
                     :where [?e :knowledge.item/stream ?stream] [?e :knowledge.item/state ?state]]
                   db (:stream value) [:pending :claimed]))
      (refuse! :stream-not-clean))
    {:stream s :pin p}))

(def ^:private audit-fields
  (mapv :db/ident schema))

(defn- audit [expected value]
  {:knowledge.pin/retirement-id (:id value) :knowledge.pin/retirement-client (:client value)
   :knowledge.pin/retirement-by (:by value) :knowledge.pin/retirement-at (:at value)
   :knowledge.pin/retirement-reason (:reason value)
   :knowledge.pin/retirement-stream-binding (hasch/uuid (:stream expected))
   :knowledge.pin/retirement-pin-binding (hasch/uuid (:pin expected))})

(defn- same-audit! [p expected value]
  (when-not (and (inst? (:knowledge.pin/retirement-at p))
                 (= (dissoc (select-keys p audit-fields) :knowledge.pin/retirement-at)
                    (dissoc (audit expected value) :knowledge.pin/retirement-at)))
    (refuse! :partial-or-conflicting-retirement)))

(defn begin-tx
  "Durably fence a clean stream before removing native retention. An identical
   retry observes the exact original receipt; no phase can reopen ingestion."
  [db expected value]
  (let [{:keys [stream pin]} (checked! db expected value)]
    (case (:knowledge.stream/state stream)
      :active
      (do
        (when-not (and (= :retained (:knowledge.pin/state pin))
                       (not-any? #(contains? pin %) audit-fields))
          (refuse! :pin-not-retirable))
        [{:db/id [:knowledge.stream/id (:stream value)] :knowledge.stream/state :retiring}
         (assoc (audit expected value) :db/id (:db/id pin))])
      (:retiring :retired)
      (do
        (same-audit! pin expected value)
        (when-not (= (if (= :retired (:knowledge.stream/state stream)) :released :retained)
                     (:knowledge.pin/state pin))
          (refuse! :partial-retirement))
        []))))

(defn retiring!
  "Canonical release-intent check, not native release permission by itself."
  [db expected value]
  (when (seq (begin-tx db expected value)) (refuse! :retirement-not-begun))
  (let [result (checked! db expected value)]
    (when-not (= :retiring (get-in result [:stream :knowledge.stream/state]))
      (refuse! :not-retiring))
    result))

(defn finish-tx
  "HOST-only after exact original physical root absence. Never release a root
   here or infer its absence from metadata; canonical replay is read-only."
  [db expected value]
  (when (seq (begin-tx db expected value)) (refuse! :retirement-not-begun))
  (let [{:keys [stream pin]} (checked! db expected value)]
    (if (= :retired (:knowledge.stream/state stream))
      []
      [{:db/id [:knowledge.stream/id (:stream value)] :knowledge.stream/state :retired}
       {:db/id (:db/id pin) :knowledge.pin/state :released}])))
