(ns is.simm.model.knowledge-processing
  "Internal joint filing fragments. The HOST certifies the Attempt, verifies
   frozen artifacts and authorizes on the writer DB before composing this with
   the canonical disposition and optional Proposal. No artifact IO or execution."
  (:require [datahike.api :as d]
            [is.simm.model.file-adoptions :as adoptions]
            [is.simm.model.file-outputs :as manifests]
            [is.simm.model.knowledge-pins :as pins]
            [hasch.core :as hasch]
            [is.simm.model.knowledge-batches :as batches]
            [is.simm.model.knowledge-streams :as streams]
            [is.simm.model.work-requests :as requests]
            [malli.core :as m]))

(def schema
  (mapv (fn [[ident type]]
          {:db/ident ident :db/valueType type :db/cardinality :db.cardinality/one})
        [[:knowledge.batch/disposition :db.type/uuid]
         [:knowledge.batch/outcome :db.type/keyword]
         [:knowledge.batch/adoption :db.type/uuid]
         [:knowledge.batch/decision-filing :db.type/string]]))

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

(defn- adoption-binding! [selection value operation]
  (adoptions/validate! operation)
  (when-not (and (= :proposed (:kind value)) (= :accepted (:state operation))
                 (= [(:proposal value) (:manifest value) (:id value) (get-in selection [:stream :room])]
                    [(:proposal operation) (:manifest operation) (:disposition operation) (:room operation)])
                 (= (get-in selection [:stream :target]) (pins/target-id (:target operation))))
    (refuse! :adoption-binding-mismatch)))

(defn- accepted-facts! [db selection value batch]
  (when-not (uuid? (:knowledge.batch/adoption batch)) (refuse! :missing-adoption))
  (let [operation (adoptions/operation db (:knowledge.batch/adoption batch))]
    (adoption-binding! selection value operation)
    (adoptions/terminal-facts! db operation)
    operation))

(defn- open-proposal? [db value]
  (when-let [owner (manifests/owner db (:manifest value))]
    (let [proposal (d/pull db [:proposal/status] (:proposal-eid owner))
          fork (d/pull db [:proposal.fork/status] (:fork-eid owner))]
      (and (= (:proposal value) (:proposal owner))
           (= :open (:proposal/status proposal)) (nil? (:proposal.fork/status fork))
           (contains? #{nil :claimed :applying :aborted}
                      (:state (adoptions/current-operation db (:fork-eid owner))))))))

(defn- dismissal-facts! [db value batch]
  (let [owner (manifests/owner db (:manifest value))
        proposal (when owner (d/pull db [:proposal/status :proposal/filing-hash :proposal/resolved-by :proposal/resolved-at]
                                     (:proposal-eid owner)))
        fork (when owner (d/pull db [:proposal.fork/status :proposal/resolved-by :proposal/resolved-at]
                                (:fork-eid owner)))
        by (:proposal/resolved-by proposal) at (:proposal/resolved-at proposal)]
    (when-not (and owner (= (:proposal value) (:proposal owner))
                   (= :dismissed (:proposal/status proposal) (:proposal.fork/status fork))
                   (uuid? by) (= by (:proposal/resolved-by fork))
                   (instance? java.util.Date at) (= at (:proposal/resolved-at fork))
                   (string? (:knowledge.batch/decision-filing batch))
                   (re-matches #"[0-9a-f]{64}" (:knowledge.batch/decision-filing batch))
                   (= (:knowledge.batch/decision-filing batch) (:proposal/filing-hash proposal))
                   (nil? (:knowledge.batch/adoption batch)))
      (refuse! :partial-dismissal))
    (when-let [operation (adoptions/withdrawn-operation db (:fork-eid owner))]
      (when-not (and (= [(:proposal value) (:manifest value) (:id value) (:knowledge.batch/room batch)
                         (:knowledge.batch/decision-filing batch)]
                        [(:proposal operation) (:manifest operation) (:disposition operation) (:room operation)
                         (:filing operation)])
                     (= (:knowledge.batch/target batch) (pins/target-id (:target operation))))
        (refuse! :withdrawn-filing-mismatch)))
    {:by by :at at}))

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
        accepted? (and proposed? (= :closed (:knowledge.batch/state batch))
                       (= :accepted (:knowledge.batch/outcome batch)))
        dismissed? (and proposed? (= :closed (:knowledge.batch/state batch))
                        (= :dismissed (:knowledge.batch/outcome batch)))
        item-state (cond dismissed? :skipped (and proposed? (not accepted?)) :claimed :else :processed)
        existing (:knowledge.batch/disposition batch)
        canonical (:knowledge.disposition/id request)]
    (if (or existing canonical)
      (do
        (when-not (and (= (:id value) existing canonical)
                       (or accepted? dismissed? (= expected-state (:knowledge.batch/state batch)))
                       (every? #(= item-state (:knowledge.item/state %)) items)
                       (cond
                         accepted? (and (nil? (:knowledge.batch/decision-filing batch))
                                        (not= id (:knowledge.stream/active-batch stream))
                                        (accepted-facts! db (:selection expected) value batch))
                         dismissed? (and (not= id (:knowledge.stream/active-batch stream))
                                         (dismissal-facts! db value batch))
                         proposed? (and (= id (:knowledge.stream/active-batch stream))
                                        (nil? (:knowledge.batch/outcome batch))
                                        (nil? (:knowledge.batch/adoption batch))
                                        (nil? (:knowledge.batch/decision-filing batch))
                                        (open-proposal? db value))
                         :else (and (= :no-op (:knowledge.batch/outcome batch))
                                    (nil? (:knowledge.batch/adoption batch))
                                    (nil? (:knowledge.batch/decision-filing batch))
                                    (not= id (:knowledge.stream/active-batch stream)))))
          (refuse! :partial-filing))
        [])
      (do
        (when-not (and (= :submitted (:knowledge.batch/state batch))
                       (= id (:knowledge.stream/active-batch stream))
                       (nil? (:knowledge.batch/outcome batch))
                       (nil? (:knowledge.batch/adoption batch))
                       (nil? (:knowledge.batch/decision-filing batch))
                       (every? #(= :claimed (:knowledge.item/state %)) items))
          (refuse! :batch-not-submitted))
        (cond-> [{:db/id (:db/id batch) :knowledge.batch/state expected-state
                  :knowledge.batch/disposition (:id value)}]
          (not proposed?)
          (into (concat [{:db/id (:db/id batch) :knowledge.batch/outcome :no-op}
                         [:db/retract [:knowledge.stream/id (:knowledge.stream/id stream)]
                          :knowledge.stream/active-batch id]]
                        (map (fn [row] {:db/id (:db/id row) :knowledge.item/state :processed}) items))))))))

(defn acceptance-tx
  "Fixed HOST composition AFTER observing the original scoped winning native
   receipt. This fragment does not observe publication or authorize acceptance.
   First acceptance composes with adoption/component/Proposal transitions on
   this writer DB; replay requires the complete joint terminal facts."
  [db expected value accepted]
  (adoption-binding! (:selection expected) value accepted)
  (let [{:keys [request batch stream items]} (binding! db expected value)
        id (:knowledge.batch/id batch)]
    (when-not (and (nil? (:knowledge.batch/decision-filing batch))
                   (= (:id value) (:knowledge.batch/disposition batch) (:knowledge.disposition/id request)))
      (refuse! :missing-filing))
    (if (= :closed (:knowledge.batch/state batch))
      (do
        (when-not (and (= :accepted (:knowledge.batch/outcome batch))
                       (= accepted (accepted-facts! db (:selection expected) value batch))
                       (every? #(= :processed (:knowledge.item/state %)) items)
                       (not= id (:knowledge.stream/active-batch stream)))
          (refuse! :partial-acceptance))
        [])
      (let [current (adoptions/operation db (:id accepted))
            immutable #(dissoc % :state :updated-at :commit :head :noop? :reconciled-by)]
        (when-not (and (= :review (:knowledge.batch/state batch))
                       (nil? (:knowledge.batch/outcome batch)) (nil? (:knowledge.batch/adoption batch))
                       (= id (:knowledge.stream/active-batch stream))
                       (every? #(= :claimed (:knowledge.item/state %)) items)
                       (= :applying (:state current))
                       (= (immutable current) (immutable accepted))
                       (not (.before ^java.util.Date (:updated-at accepted) (:updated-at current))))
          (refuse! :acceptance-prestate-mismatch))
        (into [{:db/id (:db/id batch) :knowledge.batch/state :closed
                :knowledge.batch/outcome :accepted :knowledge.batch/adoption (:id accepted)}
               [:db/retract [:knowledge.stream/id (:knowledge.stream/id stream)] :knowledge.stream/active-batch id]]
              (map (fn [row] {:db/id (:db/id row) :knowledge.item/state :processed}) items))))))

(defn dismissal-tx
  "Fixed composition with an authorized Proposal/component withdrawal. Skipped
   is a terminal decision about this source version, not processed knowledge or
   permission to replay/requeue it. Compare the original complete filing and
   selected membership; exact replay verifies the existing joint decision."
  [db expected value by at]
  (when-not (and (= :proposed (:kind value)) (uuid? by) (instance? java.util.Date at))
    (refuse! :invalid-dismissal))
  (let [{:keys [request batch stream items]} (binding! db expected value)
        id (:knowledge.batch/id batch)]
    (when-not (= (:id value) (:knowledge.batch/disposition batch) (:knowledge.disposition/id request))
      (refuse! :missing-filing))
    (if (= :closed (:knowledge.batch/state batch))
      (do
        (when-not (and (= :dismissed (:knowledge.batch/outcome batch))
                       (= {:by by :at at} (dismissal-facts! db value batch))
                       (every? #(= :skipped (:knowledge.item/state %)) items)
                       (not= id (:knowledge.stream/active-batch stream)))
          (refuse! :partial-dismissal))
        [])
      (let [owner (manifests/owner db (:manifest value))
            filing (when owner (:proposal/filing-hash (d/pull db [:proposal/filing-hash] (:proposal-eid owner))))]
        (when-not (and (= :review (:knowledge.batch/state batch))
                       (nil? (:knowledge.batch/outcome batch)) (nil? (:knowledge.batch/adoption batch))
                       (nil? (:knowledge.batch/decision-filing batch))
                       (= id (:knowledge.stream/active-batch stream))
                       (every? #(= :claimed (:knowledge.item/state %)) items)
                       (open-proposal? db value) (string? filing) (re-matches #"[0-9a-f]{64}" filing))
          (refuse! :dismissal-prestate-mismatch))
        (into [{:db/id (:db/id batch) :knowledge.batch/state :closed
                :knowledge.batch/outcome :dismissed :knowledge.batch/decision-filing filing}
               [:db/retract [:knowledge.stream/id (:knowledge.stream/id stream)] :knowledge.stream/active-batch id]]
              (map (fn [row] {:db/id (:db/id row) :knowledge.item/state :skipped}) items))))))
