(ns is.simm.model.knowledge-dispositions
  "Typed historical filing receipts on canonical work requests, not authority.
   No execution, authorization, Attempt certification or artifact reads happen
   here. The host verifies those before composing this helper with Proposal
   filing in ONE serialized control-database transaction."
  (:require [datahike.api :as d]
            [hasch.core :as hasch]
            [is.simm.model.work-requests :as requests]
            [malli.core :as m]))

(def schema
  (mapv (fn [[ident type]]
          {:db/ident ident :db/valueType type :db/cardinality :db.cardinality/one})
        [[:knowledge.disposition/id :db.type/uuid]
         [:knowledge.disposition/batch :db.type/uuid]
         [:knowledge.disposition/version :db.type/long]
         [:knowledge.disposition/request :db.type/uuid]
         [:knowledge.disposition/intent :db.type/uuid]
         [:knowledge.disposition/job :db.type/uuid]
         [:knowledge.disposition/candidate :db.type/uuid]
         [:knowledge.disposition/attempt-content :db.type/uuid]
         [:knowledge.disposition/basis :db.type/uuid]
         [:knowledge.disposition/output :db.type/uuid]
         [:knowledge.disposition/kind :db.type/keyword]
         [:knowledge.disposition/manifest :db.type/uuid]
         [:knowledge.disposition/proposal :db.type/uuid]]))

(def value-schema
  [:map {:closed true}
   [:id :uuid] [:version [:enum 1 2]] [:request :uuid] [:intent :uuid]
   [:job :uuid] [:candidate :uuid] [:attempt-content :uuid]
   [:basis :uuid] [:output :uuid] [:kind [:enum :proposed :no-op]]
   [:manifest {:optional true} :uuid] [:proposal {:optional true} :uuid]
   [:batch {:optional true} :uuid]])

(defn- refuse! [reason]
  (throw (ex-info "Knowledge filing disposition is invalid"
                  {:type ::invalid :reason reason})))

(defn validate!
  "Integrity only. A self-hashed receipt does not establish host provenance."
  [value]
  (when-not (m/validate value-schema value) (refuse! :invalid-shape))
  (case (:kind value)
    :proposed
    (when-not (and (uuid? (:manifest value))
                   (= (:proposal value)
                      (hasch/uuid [(case (:version value) 1 :simmis/knowledge-file-proposal-v1 2 :simmis/knowledge-file-proposal-v2) (:manifest value)])))
      (refuse! :invalid-proposed-binding))
    :no-op
    (when (or (contains? value :manifest) (contains? value :proposal))
      (refuse! :invalid-no-op-binding)))
  (when-not (= (:id value)
               (hasch/uuid [(case (:version value) 1 :simmis/knowledge-disposition-v1 2 :simmis/knowledge-disposition-v2) (dissoc value :id)]))
    (refuse! :content-mismatch))
  value)

(defn make
  "Seal exact semantic fields; timestamps and execution status are not fields.
   An explicit verified empty/unchanged capture may be :no-op. Missing capture
   cannot produce a receipt, because :output is required."
  ([fields] (make fields 1))
  ([fields version]
  (when-not (map? fields) (refuse! :invalid-shape))
  (when (contains? fields :id) (refuse! :caller-supplied-id))
  (when (contains? fields :version) (refuse! :caller-supplied-version))
  (let [body (assoc fields :version version)]
    (validate! (assoc body :id (hasch/uuid [(case version 1 :simmis/knowledge-disposition-v1 2 :simmis/knowledge-disposition-v2) body]))))))

(def ^:private binding-keys
  [:work.request/id :work.request/client-id :work.request/by :work.request/room
   :work.request/kind :work.request/intent-id :work.request/publication-basis-id
   :work.request/payload-ref :work.request/job :work.request/state
   :work.request/return-pending?])

(defn request-binding
  "Freeze a strict canonical-row projection AFTER the host has validated the
   retained payload using requests/publication-basis. This helper cannot read
   or certify the artifact. Pass the projection to tx-data for writer-side
   comparison; do not reconstruct a fresh binding after preparing a filing."
  [row]
  (let [markers [:work.request/knowledge-batch :work.request/knowledge-frozen]
        binding (select-keys row (into binding-keys markers))]
    (when-not (and (= (set binding-keys) (set (keys (apply dissoc binding markers))))
                   (or (not-any? #(contains? binding %) markers)
                       (every? uuid? (map binding markers)))
                   (every? uuid? (map binding
                                      [:work.request/id :work.request/client-id :work.request/by
                                       :work.request/room :work.request/intent-id
                                       :work.request/publication-basis-id :work.request/payload-ref
                                       :work.request/job]))
                   (= :workflow (:work.request/kind binding))
                   (= :submitted (:work.request/state binding))
                   (false? (:work.request/return-pending? binding))
                   (= (:work.request/id binding)
                      (requests/request-id (:work.request/by binding) (:work.request/room binding)
                                           (:work.request/client-id binding))))
      (refuse! :invalid-request-binding))
    binding))

(def ^:private disposition-keys (mapv :db/ident schema))

(defn- entity [value]
  (into {} (map (fn [[k v]] [(keyword "knowledge.disposition" (name k)) v])) value))

(defn- metadata [row]
  (into {} (map (fn [[k v]] [(keyword (name k)) v]))
        (select-keys row disposition-keys)))

(defn- matches-request? [binding value]
  (and (= [(:request value) (:intent value) (:job value) (:basis value)]
          (mapv binding [:work.request/id :work.request/intent-id
                        :work.request/job :work.request/publication-basis-id]))
       (= (:batch value) (:work.request/knowledge-batch binding))))

(defn disposition
  "Read the typed receipt from a canonical request row. Partial/corrupt rows
   fail closed; complete absence is nil. Authorize before exposing this data."
  [row]
  (when (some #(contains? row %) disposition-keys)
    (let [value (validate! (metadata row))]
      (when-not (matches-request? (request-binding row) value) (refuse! :request-mismatch))
      value)))

(defn tx-data
  "INTERNAL storage composition, called against the serialized writer DB.
   Returns a request-row update or [] for exact replay. For :proposed the host
   MUST compose this with canonical Proposal filing in the SAME transaction;
   this helper alone neither checks nor creates that Proposal. It does not
   establish current authorization, default ownership, or Attempt provenance.
   No arbitrary transaction callback or live artifact IO is accepted."
  [db expected-binding value]
  (validate! value)
  (when-not (= expected-binding (request-binding expected-binding))
    (refuse! :invalid-expected-binding))
  (let [eid (d/q '[:find ?e . :in $ ?id :where [?e :work.request/id ?id]]
                 db (:request value))
        row (when eid (d/pull db '[*] eid))]
    (when-not row (refuse! :missing-request))
    (when-not (= expected-binding (request-binding row)) (refuse! :request-changed))
    (when-not (matches-request? expected-binding value)
      (refuse! :request-mismatch))
    (if-let [existing (disposition row)]
      (if (= value existing)
        []
        (throw (ex-info "Knowledge request already has a different filing disposition"
                        {:type ::conflict :request (:request value)})))
      [(assoc (entity value) :db/id eid)])))
