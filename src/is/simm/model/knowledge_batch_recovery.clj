(ns is.simm.model.knowledge-batch-recovery
  "Fixed unlinked capturing/ready abandonment. HOST verifies retained artifacts,
   retains the original physical discovery gate and reauthorizes in the writer.
   No execution, request repair, refund, source release or activation resume."
  (:require [datahike.api :as d]
            [hasch.core :as hasch]
            [is.simm.model.knowledge-batches :as batches]
            [is.simm.model.knowledge-pins :as pins]
            [is.simm.model.knowledge-schedule-batches :as scheduled]
            [is.simm.model.knowledge-streams :as streams]
            [malli.core :as m]))

(def schema
  (mapv (fn [[ident type]] {:db/ident ident :db/valueType type :db/cardinality :db.cardinality/one})
        [[:knowledge.batch/outcome :db.type/keyword]
         [:knowledge.batch/recovery-id :db.type/uuid] [:knowledge.batch/recovery-client :db.type/uuid]
         [:knowledge.batch/recovery-by :db.type/uuid] [:knowledge.batch/recovery-at :db.type/instant]
         [:knowledge.batch/recovery-reason :db.type/string] [:knowledge.batch/recovery-from :db.type/keyword]
         [:knowledge.batch/recovery-mode :db.type/keyword]
         [:knowledge.batch/recovery-generation-before :db.type/long]
         [:knowledge.batch/recovery-generation-after :db.type/long]]))

(def ^:private decision-schema
  [:map {:closed true} [:id :uuid] [:batch :uuid] [:room :uuid] [:by :uuid] [:client :uuid]
   [:at [:fn inst?]] [:reason [:string {:min 1 :max 1000}]] [:mode [:enum :skip :requeue]]])

(defn- refuse! [reason]
  (throw (ex-info "Knowledge batch recovery is held" {:type ::held :reason reason})))

(defn decision [facts]
  (when (contains? facts :id) (refuse! :invalid-decision))
  (when-not (m/validate decision-schema (assoc facts :id #uuid "00000000-0000-0000-0000-000000000000"))
    (refuse! :invalid-decision))
  (assoc facts :id (hasch/uuid [:simmis/knowledge-batch-recovery-v1 (select-keys facts [:batch :room :by :client])])))

(def ^:private binding-fields
  [:db/id :knowledge.batch/id :knowledge.batch/stream :knowledge.batch/by :knowledge.batch/room
   :knowledge.batch/target :knowledge.batch/claim :knowledge.batch/selection :knowledge.batch/selection-id
   :knowledge.batch/frozen :knowledge.batch/frozen-id :knowledge.batch/scheduled-occurrence])

(defn- binding [row] (into {} (map (fn [k] [k (get row k)])) binding-fields))

(defn expected
  "Inert facts from a bounded, HOST-verified selection or frozen proof."
  [{:keys [row selection]} schedule]
  {:binding (binding row) :selection selection :schedule schedule})

(defn- item [row]
  {:id (:knowledge.item/id row) :order (:knowledge.item/order row) :path (:knowledge.item/path row)
   :content (:knowledge.item/content row) :size (:knowledge.item/size row) :mode (:knowledge.item/mode row)})

(defn- checked! [db expected value from mode]
  (when-not (and (= #{:binding :selection :schedule} (set (keys expected)))
                 (= (set binding-fields) (set (keys (:binding expected))))
                 (m/validate decision-schema value)
                 (= mode (:mode value))
                 (= (:id value) (:id (decision (dissoc value :id)))))
    (refuse! :invalid-decision))
  (let [{:keys [selection]} expected
        bid (:batch value) b (batches/batch db bid)
        stream (when b (streams/stream db (:knowledge.batch/stream b)))
        members (vec (take 100 (d/datoms db :avet :knowledge.item/batch bid)))
        rows (->> members (map #(d/pull db '[*] (:e %))) (sort-by (juxt :knowledge.item/order :knowledge.item/path)) vec)]
    (when-not (and b stream (= 2 (:version selection))
                   (vector? (:items selection)) (<= 1 (count (:items selection)) 99)
                   (= (:id selection) bid (:knowledge.batch/selection-id b))
                   (= (:id selection) (hasch/uuid [:simmis/knowledge-batch-selection-v2 (dissoc selection :id)]))
                   (= (:binding expected) (binding b))
                   (= (:room value) (:knowledge.batch/room b))
                   (every? uuid? (map b [:knowledge.batch/id :knowledge.batch/stream :knowledge.batch/by
                                        :knowledge.batch/room :knowledge.batch/target :knowledge.batch/claim
                                        :knowledge.batch/selection]))
                   (case from
                     :ready (every? uuid? (map b [:knowledge.batch/frozen :knowledge.batch/frozen-id]))
                     :capturing (and (not-any? #(contains? b %) [:knowledge.batch/frozen :knowledge.batch/frozen-id
                                                                :knowledge.batch/scheduled-occurrence])
                                     (nil? (:schedule expected)))
                     false)
                   (if (and (= :requeue mode) (= :closed (:knowledge.batch/state b)))
                     ;; Subsequent discovery/claim/filing may change those items.
                     ;; The old closed batch must no longer own any of them.
                     (empty? rows)
                     (and (= (:items selection) (mapv item rows))
                          (every? #(and (= (:knowledge.batch/stream b) (:knowledge.item/stream %))
                                         (nil? (:knowledge.item/pending-stream %))) rows))))
      (refuse! :batch-or-members-changed))
    (streams/validate-binding! (:stream selection) stream)
    (when (some #(contains? b %) [:knowledge.batch/disposition :knowledge.batch/adoption :knowledge.batch/decision-filing])
      (refuse! :conflicting-resolution))
    ;; Neither a stripped batch pointer nor a missing Job proves no work was
    ;; admitted. Refuse any canonical reverse request marker, indexed and bounded.
    (when (or (:knowledge.batch/request b) (:knowledge.batch/request-intent b)
              (seq (take 1 (d/datoms db :avet :work.request/knowledge-batch bid))))
      (refuse! :request-linked))
    (let [o (scheduled/bound-occurrence db bid)
          anchor (:schedule expected) v (:value anchor)]
      (when-not (and (= (boolean o) (boolean anchor))
                     (or (nil? o)
                         (and (= {:batch bid :selection (:knowledge.batch/selection-id b) :frozen (:knowledge.batch/frozen-id b)}
                                 (:batch v))
                              (= (:schedule.occurrence/id o) (:occurrence v))
                              (= (:schedule.occurrence/batch-ref o) (:reference anchor))
                              (= (:schedule.occurrence/batch-revision o) (:id v)))))
        (refuse! :schedule-binding-changed)))
    {:batch b :stream stream :items rows}))

(defn- same-decision? [b value]
  (= [(:id value) (:client value) (:by value) (:reason value) (:mode value)]
     (mapv b [:knowledge.batch/recovery-id :knowledge.batch/recovery-client :knowledge.batch/recovery-by
              :knowledge.batch/recovery-reason :knowledge.batch/recovery-mode])))

(def ^:private generation-fields
  [:knowledge.batch/recovery-generation-before :knowledge.batch/recovery-generation-after])

(defn- successor! [before]
  (when-not (and (integer? before) (<= 0 before) (< before Long/MAX_VALUE))
    (refuse! :generation-exhausted))
  (long (inc before)))

(defn- abandon-from-tx
  [db expected value from mode]
  (let [{:keys [batch stream items]} (checked! db expected value from mode)
        bid (:batch value) sid (:knowledge.batch/stream batch) schedule (:schedule expected)
        requeue? (= :requeue mode)
        outcome (if requeue? :abandoned-requeue :abandoned-skip)
        original (:expected (:selection expected))
        before (:knowledge.stream/batch-generation original)
        after (when requeue? (successor! before))]
    (if (= :closed (:knowledge.batch/state batch))
      (do
        (when-not (and (= outcome (:knowledge.batch/outcome batch))
                       (= from (:knowledge.batch/recovery-from batch))
                       (inst? (:knowledge.batch/recovery-at batch)) (same-decision? batch value)
                       (not= bid (:knowledge.stream/active-batch stream))
                       (if requeue?
                         (and (= [before after] (mapv batch generation-fields))
                              (<= after (:knowledge.stream/batch-generation stream)))
                         (and (not-any? #(contains? batch %) generation-fields)
                              (every? #(= :skipped (:knowledge.item/state %)) items))))
          (refuse! :partial-or-conflicting-recovery))
        (when schedule (scheduled/withdrawn! db schedule))
        [])
      (do
        (streams/admissible! stream)
        (when-not (and (= from (:knowledge.batch/state batch))
                       (not-any? #(contains? batch %) [:knowledge.batch/outcome
                                                      :knowledge.batch/recovery-id :knowledge.batch/recovery-client
                                                      :knowledge.batch/recovery-by :knowledge.batch/recovery-at
                                                      :knowledge.batch/recovery-reason :knowledge.batch/recovery-from
                                                      :knowledge.batch/recovery-mode
                                                      :knowledge.batch/recovery-generation-before
                                                      :knowledge.batch/recovery-generation-after])
                       (= (assoc original :knowledge.stream/active-batch bid) stream)
                       (nil? (pins/active db (:knowledge.batch/stream batch)))
                       (every? #(= :claimed (:knowledge.item/state %)) items))
          (refuse! :batch-not-abandonable))
        (into [(cond-> {:db/id (:db/id batch) :knowledge.batch/state :closed :knowledge.batch/outcome outcome
                :knowledge.batch/recovery-id (:id value) :knowledge.batch/recovery-client (:client value)
                :knowledge.batch/recovery-by (:by value) :knowledge.batch/recovery-at (:at value)
                :knowledge.batch/recovery-reason (:reason value) :knowledge.batch/recovery-from from
                :knowledge.batch/recovery-mode mode}
                 requeue? (assoc :knowledge.batch/recovery-generation-before (long before)
                                 :knowledge.batch/recovery-generation-after after))
               [:db/retract [:knowledge.stream/id sid] :knowledge.stream/active-batch bid]]
              (concat (when requeue? [{:db/id [:knowledge.stream/id sid] :knowledge.stream/batch-generation after}])
                      (mapcat (fn [i]
                                (if requeue?
                                  [{:db/id (:db/id i) :knowledge.item/state :pending :knowledge.item/pending-stream sid}
                                   [:db/retract (:db/id i) :knowledge.item/batch bid]]
                                  [{:db/id (:db/id i) :knowledge.item/state :skipped}])) items)
                      (when schedule (scheduled/withdraw-unlinked-tx db schedule))))))))

(defn abandon-tx
  "Atomic skip of EXACT ready versions, terminal audit and original scheduled
   slot. Historical replay verifies the joint result and returns no mutations."
  [db expected value]
  (abandon-from-tx db expected value :ready :skip))

(defn abandon-capturing-tx
  "Skip a physically abandoned capture, with explicit absent frozen/scheduled
   fields. HOST must retain the original publisher's exclusive gate throughout."
  [db expected value]
  (abandon-from-tx db expected value :capturing :skip))

(defn requeue-ready-tx
  "Restore eligibility atomically with a new generation, never dispatch. Old
   frozen bytes and the closed batch remain history, not retry authority."
  [db expected value]
  (abandon-from-tx db expected value :ready :requeue))

(defn requeue-capturing-tx
  "Requeue a physically abandoned capture under the original publisher gate."
  [db expected value]
  (abandon-from-tx db expected value :capturing :requeue))
