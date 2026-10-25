(ns is.simm.model.knowledge-processing
  "Internal joint filing fragments. The HOST certifies the Attempt, verifies
   frozen artifacts and authorizes on the writer DB before composing this with
   the canonical disposition and optional Proposal. No artifact IO or execution."
  (:require [datahike.api :as d]
            [hasch.core :as hasch]
            [is.simm.model.knowledge-batches :as batches]
            [is.simm.model.knowledge-streams :as streams]
            [is.simm.model.work-requests :as requests]
            [malli.core :as m]))

(def schema
  (mapv (fn [[ident type]]
          {:db/ident ident :db/valueType type :db/cardinality :db.cardinality/one})
        [[:knowledge.batch/disposition :db.type/uuid]
         [:knowledge.batch/outcome :db.type/keyword]]))

(defn- refuse! [reason]
  (throw (ex-info "Knowledge processing receipt is incomplete"
                  {:type ::held :reason reason})))

(def ^:private batch-binding-keys
  [:db/id :knowledge.batch/id :knowledge.batch/stream :knowledge.batch/by :knowledge.batch/room
   :knowledge.batch/target :knowledge.batch/claim :knowledge.batch/selection :knowledge.batch/selection-id
   :knowledge.batch/frozen :knowledge.batch/frozen-id :knowledge.batch/request :knowledge.batch/request-intent])

(defn expected
  "Inert writer input from a HOST-verified retained batch proof. This projection
   is not a certificate or authority; the writer compares its canonical rows."
  [{:keys [origin selection row]}]
  {:origin origin :selection selection :binding (select-keys row batch-binding-keys)})

(defn- item [row]
  {:id (:knowledge.item/id row) :order (:knowledge.item/order row)
   :path (:knowledge.item/path row) :content (:knowledge.item/content row)
   :size (:knowledge.item/size row) :mode (:knowledge.item/mode row)})

(defn- binding! [db {:keys [origin selection binding] :as expected} value]
  (when-not (and (= #{:origin :selection :binding} (set (keys expected)))
                 (= (set batch-binding-keys) (set (keys binding)))
                 (m/validate batches/origin-schema origin)
                 (= (:batch origin) (:selection origin) (:id selection))
                 (= 2 (:version selection))
                 (= (:id selection) (hasch/uuid [:simmis/knowledge-batch-selection-v2 (dissoc selection :id)]))
                 (vector? (:items selection)) (<= 1 (count (:items selection)) 99)
                 (= (:batch origin) (:batch value)))
    (refuse! :invalid-expected-batch))
  (let [row (requests/request-in db (:request value))
        batch (batches/batch db (:batch origin))
        descriptor (:stream selection) stream (streams/stream db (:id descriptor))
        members (vec (take 100 (d/datoms db :avet :knowledge.item/batch (:batch origin))))
        rows (->> members (map #(d/pull db '[*] (:e %)))
                  (sort-by (juxt :knowledge.item/order :knowledge.item/path)) vec)]
    (when-not (and row batch stream
                   (= binding (select-keys batch batch-binding-keys))
                   (= :workflow (:work.request/kind row))
                   (= [(:request value) (:intent value) (:job value) (:basis value)]
                      (mapv row [:work.request/id :work.request/intent-id :work.request/job
                                 :work.request/publication-basis-id]))
                   (= [(:batch origin) (:frozen origin)]
                      [(:work.request/knowledge-batch row) (:work.request/knowledge-frozen row)])
                   (= [(:selection origin) (:frozen origin)]
                      [(:knowledge.batch/selection-id batch) (:knowledge.batch/frozen-id batch)])
                   (= [(:request value) (:intent value)]
                      [(:knowledge.batch/request batch) (:knowledge.batch/request-intent batch)])
                   (= [(:by descriptor) (:room descriptor) (:target descriptor) (:id descriptor)]
                      [(:knowledge.batch/by batch) (:knowledge.batch/room batch)
                       (:knowledge.batch/target batch) (:knowledge.batch/stream batch)])
                   (= [(:by descriptor) (:room descriptor)]
                      [(:work.request/by row) (:work.request/room row)])
                   (< (count members) 100)
                   (= (:items selection) (mapv item rows))
                   (every? #(and (= (:id descriptor) (:knowledge.item/stream %))
                                  (= (:batch origin) (:knowledge.item/batch %))
                                  (nil? (:knowledge.item/pending-stream %))) rows))
      (refuse! :canonical-binding-mismatch))
    (streams/validate-binding! descriptor stream)
    {:request row :batch batch :stream stream :items rows}))

(defn filing-tx
  "Compose with the exact canonical disposition insert on the SAME writer DB.
   A proposed update holds all selected items until actual adoption. A certified
   no-op processes precisely those items. Replays validate the whole receipt;
   they never clear a later batch or silently repair a partial publication."
  [db expected value]
  (when-not (and (uuid? (:id value)) (#{:proposed :no-op} (:kind value)))
    (refuse! :invalid-disposition))
  (let [{:keys [request batch stream items]} (binding! db expected value)
        id (:knowledge.batch/id batch)
        proposed? (= :proposed (:kind value))
        expected-state (if proposed? :review :closed)
        item-state (if proposed? :claimed :processed)
        existing (:knowledge.batch/disposition batch)
        canonical (:knowledge.disposition/id request)]
    (if (or existing canonical)
      (do
        (when-not (and (= (:id value) existing canonical)
                       (= expected-state (:knowledge.batch/state batch))
                       (every? #(= item-state (:knowledge.item/state %)) items)
                       (if proposed?
                         (and (= id (:knowledge.stream/active-batch stream))
                              (nil? (:knowledge.batch/outcome batch)))
                         (and (= :no-op (:knowledge.batch/outcome batch))
                              (not= id (:knowledge.stream/active-batch stream)))))
          (refuse! :partial-filing))
        [])
      (do
        (when-not (and (= :submitted (:knowledge.batch/state batch))
                       (= id (:knowledge.stream/active-batch stream))
                       (nil? (:knowledge.batch/outcome batch))
                       (every? #(= :claimed (:knowledge.item/state %)) items))
          (refuse! :batch-not-submitted))
        (cond-> [{:db/id (:db/id batch) :knowledge.batch/state expected-state
                  :knowledge.batch/disposition (:id value)}]
          (not proposed?)
          (into (concat [{:db/id (:db/id batch) :knowledge.batch/outcome :no-op}
                         [:db/retract [:knowledge.stream/id (:knowledge.stream/id stream)]
                          :knowledge.stream/active-batch id]]
                        (map (fn [row] {:db/id (:db/id row) :knowledge.item/state :processed}) items))))))))
