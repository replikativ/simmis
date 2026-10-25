(ns is.simm.model.knowledge-schedule-batches
  "Inactive occurrence-to-batch retention. Pure writer checks and bounded
   artifact storage, not authority, a request claim, or dispatch ownership.
   HOST must verify the retained deployment and frozen evidence first."
  (:require [datahike.api :as d]
            [dvergr.artifact :as artifact]
            [hasch.core :as hasch]
            [is.simm.model.knowledge-batches :as batches]
            [is.simm.model.knowledge-streams :as streams]
            [malli.core :as m]))

(def max-binding-bytes (* 16 1024))

(def schema
  [{:db/ident :schedule.occurrence/knowledge-batch :db/valueType :db.type/uuid
    :db/cardinality :db.cardinality/one :db/unique :db.unique/value}
   {:db/ident :schedule.occurrence/batch-ref :db/valueType :db.type/store-ref
    :db/cardinality :db.cardinality/one}
   {:db/ident :schedule.occurrence/batch-revision :db/valueType :db.type/uuid
    :db/cardinality :db.cardinality/one}])

(def ^:private value-schema
  [:map {:closed true} [:version [:= 1]] [:id :uuid]
   [:activation :uuid] [:revision :uuid] [:target-ref :uuid]
   [:occurrence :uuid] [:claim :uuid] [:by :uuid] [:room :uuid]
   [:batch batches/origin-schema]])

(defn- refuse! [reason]
  (throw (ex-info "Scheduled knowledge batch binding is held"
                  {:type ::held :reason reason})))

(defn- row [db attr id]
  (when (uuid? id)
    (when-let [eid (:e (first (d/datoms db :avet attr id)))]
      (d/pull db '[*] eid))))

(defn bound-occurrence
  "Internal reverse metadata lookup. Caller authorizes before observation."
  [db batch-id]
  (row db :schedule.occurrence/knowledge-batch batch-id))

(defn binding-value
  "Finite inert deployment/claim facts supplied by the fixed HOST verifier.
   The digest establishes integrity, not permission or a retained-artifact proof."
  [facts]
  (when (or (contains? facts :id) (contains? facts :version)) (refuse! :invalid-binding))
  (let [value (assoc facts :version 1)]
    ;; Closed finite types BEFORE hashing any caller-supplied collection.
    (when-not (m/validate value-schema (assoc value :id #uuid "00000000-0000-0000-0000-000000000000"))
      (refuse! :invalid-binding))
    (assoc value :id (hasch/uuid [:simmis/scheduled-knowledge-batch-v1 value]))))

(defn- validate! [value]
  (when-not (and (m/validate value-schema value)
                 (= (:id value) (hasch/uuid [:simmis/scheduled-knowledge-batch-v1 (dissoc value :id)])))
    (refuse! :invalid-binding))
  value)

(def ^:private binding-fields
  [:schedule.occurrence/knowledge-batch :schedule.occurrence/batch-ref
   :schedule.occurrence/batch-revision])

(defn- claimed! [db value]
  (let [o (row db :schedule.occurrence/id (:occurrence value))
        a (row db :schedule.activation/id (:activation value))]
    (when-not (and o a
                   (= [(:activation value) (:revision value) (:claim value)]
                      [(:schedule.occurrence/activation o) (:schedule.occurrence/revision o)
                       (:schedule.occurrence/claim o)])
                   (= :admitting (:schedule.occurrence/state o))
                   (= (:occurrence value) (:schedule.activation/active-occurrence a))
                   (= [(:by value) (:room value) (:revision value) (:target-ref value)]
                      [(:schedule.activation/by a) (:schedule.activation/room a)
                       (:schedule.activation/revision a) (:schedule.activation/target-ref a)]))
      (refuse! :occurrence-changed))
    ;; Pause withdraws pending work, not this already claimed admission.
    o))

(defn- ready! [db value]
  (let [{:keys [batch selection frozen]} (:batch value)
        b (batches/batch db batch)
        stream (when b (streams/stream db (:knowledge.batch/stream b)))]
    (when-not (and b stream (= :ready (:knowledge.batch/state b))
                   (nil? (:knowledge.batch/request b)) (nil? (:knowledge.batch/request-intent b))
                   (= [batch selection frozen (:by value) (:room value)]
                      [(:knowledge.batch/id b) (:knowledge.batch/selection-id b)
                       (:knowledge.batch/frozen-id b) (:knowledge.batch/by b) (:knowledge.batch/room b)])
                   (every? uuid? [(:knowledge.batch/selection b) (:knowledge.batch/frozen b)])
                   (= batch (:knowledge.stream/active-batch stream))
                   (= [(:by value) (:room value) (:knowledge.batch/target b)]
                      [(:knowledge.stream/by stream) (:knowledge.stream/room stream) (:knowledge.stream/target stream)]))
      (refuse! :batch-not-ready))))

(defn retention-tx
  "First install only. Even identical prior retention cannot manufacture a new
   original acknowledgement. No bodies, artifact IO, provider or callbacks."
  [db value ref]
  (validate! value)
  (when-not (uuid? ref) (refuse! :invalid-reference))
  (let [o (claimed! db value)]
    (ready! db value)
    (when (or (some #(contains? o %) binding-fields)
              (some #(contains? o %) [:schedule.occurrence/input-id :schedule.occurrence/input-ref
                                      :schedule.occurrence/input-revision])
              (bound-occurrence db (get-in value [:batch :batch])))
      (refuse! :already-retained))
    [{:db/id (:db/id o) :schedule.occurrence/knowledge-batch (get-in value [:batch :batch])
      :schedule.occurrence/batch-ref ref :schedule.occurrence/batch-revision (:id value)}]))

(defn- matches! [o value ref]
  (when-not (and o
                 (= [(:occurrence value) (:activation value) (:revision value) (:claim value)]
                    [(:schedule.occurrence/id o) (:schedule.occurrence/activation o)
                     (:schedule.occurrence/revision o) (:schedule.occurrence/claim o)])
                 (= [(get-in value [:batch :batch]) ref (:id value)]
                    (mapv o binding-fields)))
    (refuse! :retained-binding-mismatch))
  o)

(defn retain!
  "Store the bounded binding and return ONLY its original positive writer ACK.
   The caller must retain native ownership through publication and handoff.
   An uncertain return is inspection-only, never permission to redispatch."
  [conn value]
  (validate! value)
  (let [prepared (artifact/prepare-value value max-binding-bytes)]
    (artifact/publish-value!
     (artifact/datahike-store conn) value
     (fn [ref]
       (when-not (= ref (:ref prepared)) (refuse! :artifact-reference-changed))
       (let [report (d/transact conn [[:db.fn/call retention-tx value ref]])]
         (when-not (and (:db-before report) (:db-after report)) (refuse! :missing-original-ack))
         (let [before (claimed! (:db-before report) value)
               after (claimed! (:db-after report) value)]
           (when (some #(contains? before %) binding-fields) (refuse! :missing-original-ack))
           (matches! after value ref)))))))

(defn retained
  "Bounded historical observation only. No active/ready test and no new claim.
   HOST still verifies the original deployment and canonical joint request."
  [conn occurrence]
  (let [present (mapv #(contains? occurrence %) binding-fields)]
    (cond
      (every? false? present) nil
      (not-every? true? present) (refuse! :partial-binding)
      :else
      (let [ref (:schedule.occurrence/batch-ref occurrence)
            envelope (artifact/get-bounded-value (artifact/datahike-store conn) ref max-binding-bytes)
            value (:value envelope)]
        (validate! value)
        (matches! occurrence value ref)
        value))))
