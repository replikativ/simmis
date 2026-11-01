(ns is.simm.model.knowledge-submitted-recovery
  "Atomic recovery of an exactly linked failed update. HOST supplies verified
   native/frozen proof and current authority. This model never executes work,
   withdraws a scheduled occurrence, refunds resources or releases source pins."
  (:require [clojure.string :as str]
            [datahike.api :as d]
            [dvergr.room.store :as native-store]
            [hasch.core :as hasch]
            [is.simm.model.knowledge-batches :as batches]
            [is.simm.model.knowledge-dispositions :as dispositions]
            [is.simm.model.knowledge-pins :as pins]
            [is.simm.model.knowledge-processing :as processing]
            [is.simm.model.knowledge-schedule-batches :as scheduled]
            [is.simm.model.knowledge-streams :as streams]
            [is.simm.model.work-requests :as requests]
            [malli.core :as m]))

(def schema
  (mapv (fn [[ident type]] {:db/ident ident :db/valueType type :db/cardinality :db.cardinality/one})
        [[:knowledge.batch/recovery-version :db.type/long]
         [:knowledge.batch/recovery-request :db.type/uuid]
         [:knowledge.batch/recovery-job :db.type/uuid]
         [:knowledge.batch/recovery-job-status :db.type/keyword]
         [:knowledge.batch/recovery-failure :db.type/keyword]
         [:knowledge.batch/recovery-core-ack :db.type/uuid]
         [:knowledge.batch/recovery-adapter-ack :db.type/uuid]
         [:knowledge.batch/recovery-family-ack :db.type/uuid]
         [:knowledge.batch/recovery-candidate :db.type/uuid]
         [:knowledge.batch/recovery-attempt-content :db.type/uuid]]))

(def ^:private native-schema
  [:map {:closed true} [:kind [:enum :candidate-failed :job-failed]]
   [:job :uuid] [:status [:enum :completed :failed :cancelled]]
   [:core :uuid] [:adapter :uuid] [:family :uuid]
   [:candidate {:optional true} :uuid] [:attempt-content {:optional true} :uuid]])

(def ^:private decision-schema
  [:map {:closed true} [:id :uuid] [:batch :uuid] [:request :uuid] [:room :uuid]
   [:by :uuid] [:client :uuid] [:at [:fn inst?]] [:reason [:string {:min 1 :max 1000}]]])

(defn- refuse! [reason]
  (throw (ex-info "Failed knowledge update recovery is held" {:type ::held :reason reason})))

(defn native-proof!
  "Shape/identity checks only. A self-consistent map is not HOST provenance."
  [value]
  (when-not (and (m/validate native-schema value)
                 (= (:core value) (native-store/job-cleanup-ack-id (:job value) :core))
                 (= (:adapter value) (native-store/job-cleanup-ack-id (:job value) :adapter))
                 (= (:family value) (native-store/family-return-ack-id (:job value)))
                 (case (:kind value)
                   :candidate-failed (and (= :completed (:status value))
                                          (every? uuid? (map value [:candidate :attempt-content])))
                   :job-failed (and (#{:failed :cancelled} (:status value))
                                    (not-any? #(contains? value %) [:candidate :attempt-content]))))
    (refuse! :invalid-native-proof))
  value)

(defn decision [facts]
  (when (contains? facts :id) (refuse! :invalid-decision))
  (let [id (hasch/uuid [:simmis/submitted-knowledge-recovery-v1
                        (select-keys facts [:batch :request :room :by :client])])
        value (assoc facts :id id)]
    (when-not (m/validate decision-schema value) (refuse! :invalid-decision))
    value))

(defn expected [proof request native schedule]
  {:proof (processing/expected proof) :request (dispositions/request-binding request)
   :native (native-proof! native) :schedule schedule})

(defn- receipt-fields [value native before after]
  (cond-> {:knowledge.batch/recovery-version 1
           :knowledge.batch/recovery-id (:id value) :knowledge.batch/recovery-request (:request value)
           :knowledge.batch/recovery-client (:client value) :knowledge.batch/recovery-by (:by value)
           :knowledge.batch/recovery-reason (:reason value) :knowledge.batch/recovery-mode :fresh-update
           :knowledge.batch/recovery-from :submitted
           :knowledge.batch/recovery-generation-before before :knowledge.batch/recovery-generation-after after
           :knowledge.batch/recovery-job (:job native) :knowledge.batch/recovery-job-status (:status native)
           :knowledge.batch/recovery-failure (:kind native) :knowledge.batch/recovery-core-ack (:core native)
           :knowledge.batch/recovery-adapter-ack (:adapter native) :knowledge.batch/recovery-family-ack (:family native)}
    (:candidate native) (assoc :knowledge.batch/recovery-candidate (:candidate native)
                               :knowledge.batch/recovery-attempt-content (:attempt-content native))))

(defn- native-receipt [batch]
  (cond-> {:kind (:knowledge.batch/recovery-failure batch) :job (:knowledge.batch/recovery-job batch)
           :status (:knowledge.batch/recovery-job-status batch) :core (:knowledge.batch/recovery-core-ack batch)
           :adapter (:knowledge.batch/recovery-adapter-ack batch) :family (:knowledge.batch/recovery-family-ack batch)}
    (some #(contains? batch %) [:knowledge.batch/recovery-candidate :knowledge.batch/recovery-attempt-content])
    (assoc :candidate (:knowledge.batch/recovery-candidate batch)
           :attempt-content (:knowledge.batch/recovery-attempt-content batch))))

(defn- item [row]
  {:id (:knowledge.item/id row) :order (:knowledge.item/order row) :path (:knowledge.item/path row)
   :content (:knowledge.item/content row) :size (:knowledge.item/size row) :mode (:knowledge.item/mode row)})

(defn- successor! [before]
  (when-not (and (integer? before) (<= 0 before) (< before Long/MAX_VALUE))
    (refuse! :generation-exhausted))
  (long (inc before)))

(defn- scheduled-failure? [native occurrence]
  (case (:kind native)
    :candidate-failed (= [(:candidate native) (:attempt-content native)]
                         (mapv occurrence [:schedule.occurrence/failed-candidate :schedule.occurrence/failed-attempt-content]))
    :job-failed (= [(:job native) (:status native)]
                   (mapv occurrence [:schedule.occurrence/failed-job :schedule.occurrence/failed-job-status]))
    false))

(defn recovered?
  "Strict metadata advice for schedule Resume, never native recovery authority.
   Historical replay permits later discovery/admission; the failed batch must
   no longer own the active pointer or any item. Corrupt receipts return false."
  [db batch-id request-id]
  (try
    (let [batch (batches/batch db batch-id)
          request (requests/request-in db request-id)
          stream (streams/stream db (:knowledge.batch/stream batch))
          native (native-proof! (native-receipt batch))
          descriptor (streams/descriptor
                      (into {} (for [k [:by :room :target :policy :program :epoch]]
                                 [k (get stream (keyword "knowledge.stream" (name k)))])))
          _ (streams/validate-binding! descriptor stream)
          reverse-links (vec (take 2 (d/datoms db :avet :work.request/knowledge-batch batch-id)))
          before (:knowledge.batch/recovery-generation-before batch)
          after (successor! before)
          value (decision {:batch batch-id :request request-id :room (:knowledge.batch/room batch)
                           :by (:knowledge.batch/recovery-by batch) :client (:knowledge.batch/recovery-client batch)
                           :at (:knowledge.batch/recovery-at batch) :reason (:knowledge.batch/recovery-reason batch)})
          fields (receipt-fields value native before after)]
      (and (every? uuid? (map batch [:knowledge.batch/id :knowledge.batch/stream :knowledge.batch/by
                                     :knowledge.batch/room :knowledge.batch/target :knowledge.batch/claim
                                     :knowledge.batch/selection :knowledge.batch/selection-id
                                     :knowledge.batch/frozen :knowledge.batch/frozen-id]))
           (= batch-id (:knowledge.batch/id batch) (:knowledge.batch/selection-id batch))
           (= (:knowledge.batch/stream batch) (:id descriptor))
           (= [(:knowledge.batch/by batch) (:knowledge.batch/room batch) (:knowledge.batch/target batch)]
              (mapv descriptor [:by :room :target]))
           (= 1 (count reverse-links)) (= (:db/id request) (:e (first reverse-links)))
           (nil? (dispositions/disposition request))
           (= :closed (:knowledge.batch/state batch)) (= :failed-requeued (:knowledge.batch/outcome batch))
           (= fields (select-keys batch (keys fields)))
           (nil? (:knowledge.batch/disposition batch)) (nil? (:knowledge.batch/adoption batch))
           (nil? (:knowledge.batch/decision-filing batch))
           (= request-id (:knowledge.batch/request batch) (:knowledge.batch/recovery-request batch))
           (= (:work.request/intent-id request) (:knowledge.batch/request-intent batch))
           (= batch-id (:work.request/knowledge-batch request))
           (= (:knowledge.batch/frozen-id batch) (:work.request/knowledge-frozen request))
           (= (:work.request/by request) (:knowledge.batch/by batch))
           (= (:work.request/room request) (:knowledge.batch/room batch))
           (= (:job native) (:work.request/job (dispositions/request-binding request)))
           stream (not= batch-id (:knowledge.stream/active-batch stream))
           (if-let [occurrence (scheduled/bound-occurrence db batch-id)]
             (and (= request-id (:schedule.occurrence/request occurrence))
                  (= batch-id (:schedule.occurrence/knowledge-batch occurrence))
                  (= :linked (:schedule.occurrence/state occurrence))
                  (scheduled-failure? native occurrence))
             true)
           (integer? (:knowledge.stream/batch-generation stream))
           (<= 0 (:knowledge.stream/batch-generation stream) Long/MAX_VALUE)
           (<= after (:knowledge.stream/batch-generation stream))
           (empty? (take 1 (d/datoms db :avet :knowledge.item/batch batch-id)))))
    (catch clojure.lang.ExceptionInfo _ false)))

(defn recorded-native!
  "Read the complete historical receipt after current caller authorization.
   It acknowledges a past recovery, never supplies fresh native permission."
  [db batch-id request-id]
  (when-not (recovered? db batch-id request-id) (refuse! :incomplete-historical-recovery))
  (native-proof! (native-receipt (batches/batch db batch-id))))

(defn requeue-tx
  "One serialized state transition: close the failed batch, requeue its exact
   versions and increment generation once. Preserve original Request, scheduled
   history, discovery cursor/root and all source/version bytes. Replay is empty."
  [db {:keys [proof request native schedule] :as expected} value]
  (when-not (and (= #{:proof :request :native :schedule} (set (keys expected)))
                 (m/validate decision-schema value)
                 (= value (decision (dissoc value :id)))
                 (= request (dispositions/request-binding request)))
    (refuse! :invalid-expected))
  (native-proof! native)
  (let [{:keys [origin selection binding]} proof
        bid (:batch value) batch (batches/batch db bid)
        row (requests/request-in db (:request value))
        stream (streams/stream db (:knowledge.batch/stream batch))
        members (vec (take 100 (d/datoms db :avet :knowledge.item/batch bid)))
        items (->> members (map #(d/pull db '[*] (:e %)))
                   (sort-by (juxt :knowledge.item/order :knowledge.item/path)) vec)
        before (get-in selection [:expected :knowledge.stream/batch-generation]) after (successor! before)
        sid (:knowledge.batch/stream batch)
        reverse-links (vec (take 2 (d/datoms db :avet :work.request/knowledge-batch bid)))]
    (when-not (and batch row stream
                   (= 2 (:version selection)) (vector? (:items selection)) (<= 1 (count (:items selection)) 99)
                   (= bid (:batch origin) (:selection origin) (:id selection))
                   (= bid (hasch/uuid [:simmis/knowledge-batch-selection-v2 (dissoc selection :id)]))
                   (= proof (processing/expected {:origin origin :selection selection :row batch}))
                   (= request (dispositions/request-binding row))
                   (= (:request value) (:work.request/id row) (:knowledge.batch/request batch))
                   (= (:room value) (:knowledge.batch/room batch) (:work.request/room row))
                   (= (:work.request/by row) (:knowledge.batch/by batch))
                   (= (:work.request/intent-id row) (:knowledge.batch/request-intent batch))
                   (= (:frozen origin) (:knowledge.batch/frozen-id batch) (:work.request/knowledge-frozen row))
                   (= (:job native) (:work.request/job row))
                   (= 1 (count reverse-links)) (= (:db/id row) (:e (first reverse-links)))
                   (= schedule (scheduled/bound-occurrence db bid))
                   (if schedule
                     (and (= (:knowledge.batch/scheduled-occurrence batch) (:schedule.occurrence/id schedule))
                          (= (:request value) (:schedule.occurrence/request schedule))
                          (= :linked (:schedule.occurrence/state schedule))
                          (= bid (:schedule.occurrence/knowledge-batch schedule))
                          (scheduled-failure? native schedule))
                     (nil? schedule)))
      (refuse! :binding-changed))
    (streams/validate-binding! (:stream selection) stream)
    (when (or (dispositions/disposition row)
              (some #(contains? batch %) [:knowledge.batch/disposition :knowledge.batch/adoption :knowledge.batch/decision-filing]))
      (refuse! :conflicting-resolution))
    (if (= :closed (:knowledge.batch/state batch))
      (do
        (when-not (and (recovered? db bid (:request value))
                       (= (receipt-fields value native before after)
                          (select-keys batch (keys (receipt-fields value native before after)))))
          (refuse! :conflicting-recovery))
        [])
      (do
        (streams/admissible! stream)
        (when-not (and (= :submitted (:knowledge.batch/state batch))
                       (nil? (:knowledge.batch/outcome batch))
                       (not-any? #(and (= "knowledge.batch" (namespace %))
                                       (str/starts-with? (name %) "recovery-")) (keys batch))
                       (= (assoc (:expected selection) :knowledge.stream/active-batch bid) stream)
                       (nil? (pins/active db sid))
                       (= (:items selection) (mapv item items))
                       (every? #(and (= sid (:knowledge.item/stream %)) (= :claimed (:knowledge.item/state %))
                                     (nil? (:knowledge.item/pending-stream %))) items))
          (refuse! :batch-not-recoverable))
        (into [(merge {:db/id (:db/id batch) :knowledge.batch/state :closed :knowledge.batch/outcome :failed-requeued
                       :knowledge.batch/recovery-at (:at value)} (receipt-fields value native before after))
               [:db/retract [:knowledge.stream/id sid] :knowledge.stream/active-batch bid]
               {:db/id [:knowledge.stream/id sid] :knowledge.stream/batch-generation after}]
              (mapcat (fn [i]
                        [{:db/id (:db/id i) :knowledge.item/state :pending :knowledge.item/pending-stream sid}
                         [:db/retract (:db/id i) :knowledge.item/batch bid]]) items))))))
