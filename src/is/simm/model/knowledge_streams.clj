(ns is.simm.model.knowledge-streams
  "Internal durable discovery journal, not execution or processing authority.
   HOST callers must authorize the exact stream/source, verify/pin the new
   published checkpoint, and retain both pins across this write. A discovery
   only enqueues source versions. It never certifies a Run or marks it processed.
   Not registered in the live runtime or exposed as a tool."
  (:require [datahike.api :as d]
            [dvergr.artifact :as artifact]
            [hasch.core :as hasch]
            [is.simm.documents.files :as files]
            [malli.core :as m]))

(def max-changes 20000)
(def max-plan-bytes (* 16 1024 1024))

(def schema
  (mapv (fn [[ident type]]
          (cond-> {:db/ident ident :db/valueType type :db/cardinality :db.cardinality/one}
            (#{:knowledge.item/stream :knowledge.item/pending-stream} ident) (assoc :db/index true)
            (#{:knowledge.stream/id :knowledge.item/id :knowledge.discovery/id} ident)
            (assoc :db/unique :db.unique/value)))
        [[:knowledge.stream/id :db.type/uuid]
         [:knowledge.stream/by :db.type/uuid] [:knowledge.stream/room :db.type/uuid]
         [:knowledge.stream/target :db.type/uuid] [:knowledge.stream/policy :db.type/uuid]
         [:knowledge.stream/source :db.type/uuid]
         [:knowledge.stream/program :db.type/uuid] [:knowledge.stream/epoch :db.type/uuid]
         [:knowledge.stream/revision :db.type/long] [:knowledge.stream/next-order :db.type/long]
         [:knowledge.stream/batch-generation :db.type/long]
         [:knowledge.stream/state :db.type/keyword]
         [:knowledge.stream/cursor-commit :db.type/uuid] [:knowledge.stream/cursor-root :db.type/uuid]
         [:knowledge.stream/cursor-tx :db.type/long] [:knowledge.stream/last-discovery :db.type/uuid]
         [:knowledge.stream/active-batch :db.type/uuid]
         [:knowledge.item/id :db.type/uuid] [:knowledge.item/stream :db.type/uuid]
         [:knowledge.item/pending-stream :db.type/uuid]
         [:knowledge.item/path :db.type/string] [:knowledge.item/content :db.type/uuid]
         [:knowledge.item/size :db.type/long] [:knowledge.item/mode :db.type/long]
         [:knowledge.item/order :db.type/long] [:knowledge.item/state :db.type/keyword]
         [:knowledge.item/discovery :db.type/uuid]
         [:knowledge.discovery/id :db.type/uuid] [:knowledge.discovery/stream :db.type/uuid]
         [:knowledge.discovery/revision :db.type/long] [:knowledge.discovery/payload :db.type/store-ref]
         [:knowledge.discovery/payload-id :db.type/uuid]
         [:knowledge.discovery/before-root :db.type/uuid] [:knowledge.discovery/after-root :db.type/uuid]
         [:knowledge.discovery/old-root-released? :db.type/boolean]]))

(def descriptor-schema
  [:map {:closed true} [:by :uuid] [:room :uuid] [:target :uuid]
   [:policy :uuid] [:program :uuid] [:epoch :uuid]])

(defn- refuse! [reason]
  (throw (ex-info "Knowledge discovery journal refused the transition"
                  {:type ::refused :reason reason})))

(defn descriptor
  "Stable semantic identity. HOST supplies hashes of the exact owned target,
   normalized policy and program source; model/cadence/request are not fields.
   A new revisit epoch is explicit. Hash integrity is not authorization."
  [value]
  (when-not (m/validate descriptor-schema value) (refuse! :invalid-descriptor))
  (assoc value :id (hasch/uuid [:simmis/knowledge-stream-v1 value])))

(defn- row [db attr id]
  (when-let [eid (d/q '[:find ?e . :in $ ?attr ?id :where [?e ?attr ?id]] db attr id)]
    (d/pull db '[*] eid)))

(defn stream [db id]
  (some-> (row db :knowledge.stream/id id) (dissoc :db/id)))

(defn admissible!
  "Fresh mutation fence. Nil is an uncreated stream, never a retired retry."
  [row]
  (when (and row (not= :active (:knowledge.stream/state row)))
    (refuse! :stream-closed))
  row)

(defn items
  "Internal bounded metadata observation. Authorize before exposing it."
  [db id]
  (let [eids (vec (take (inc max-changes)
                       (map :e (d/datoms db :avet :knowledge.item/stream id))))]
    (when (> (count eids) max-changes) (refuse! :item-inventory-limit))
    (->> eids (map #(d/pull db '[*] %)) (sort-by (juxt :knowledge.item/order :knowledge.item/path)) vec)))

(defn item-id [stream-id path metadata]
  (hasch/uuid [:simmis/knowledge-item-v1 stream-id path
               (select-keys metadata [:content :size :mode])]))

(defn pending-items
  "Bounded pending-only index, independent of the number of historical items.
   pending-stream is a derived lookup key maintained with state in the SAME
   writer transaction. It is not processing authority: verify every result.
   Future claim/filing writers must retract it when leaving :pending."
  [db id]
  (let [rows (mapv #(d/pull db '[*] (:e %))
                   (take (inc max-changes) (d/datoms db :avet :knowledge.item/pending-stream id)))]
    (when (> (count rows) max-changes) (refuse! :pending-inventory-limit))
    (doseq [row rows]
      (when-not (and (= id (:knowledge.item/stream row)) (= :pending (:knowledge.item/state row))
                     (uuid? (:knowledge.item/content row)) (= 33188 (:knowledge.item/mode row))
                     (integer? (:knowledge.item/order row)) (<= 0 (:knowledge.item/order row))
                     (integer? (:knowledge.item/size row)) (<= 0 (:knowledge.item/size row))
                     (= (:knowledge.item/id row)
                        (item-id id (:knowledge.item/path row)
                                 {:content (:knowledge.item/content row) :size (:knowledge.item/size row)
                                  :mode (:knowledge.item/mode row)})))
        (refuse! :invalid-pending-index))
      (files/path! (:knowledge.item/path row)))
    (vec (sort-by (juxt :knowledge.item/order :knowledge.item/path) rows))))

(def ^:private file-schema
  [:map {:closed true} [:role [:enum :trigger :context]] [:content :uuid]
   [:size [:int {:min 0 :max Long/MAX_VALUE}]] [:mode [:= 33188]]])

(def ^:private basis-schema
  [:map {:closed true} [:repository :uuid] [:store :uuid] [:backend :keyword]
   [:branch :keyword] [:ref [:string {:min 1 :max 512}]] [:commit :uuid] [:configuration :uuid]
   [:max-tx [:int {:min 0 :max Long/MAX_VALUE}]] [:datoms [:int {:min 0 :max Long/MAX_VALUE}]]])

(defn source-id
  "Internal physical source/config identity, not authorization or retention."
  [basis]
  (hasch/uuid [:simmis/knowledge-source-v1
               (select-keys basis [:repository :store :backend :branch :ref :configuration])]))

(defn validate-basis! [value]
  (when-not (m/validate basis-schema value) (refuse! :invalid-basis))
  value)

(defn- validate-plan! [plan]
  (when-not (and (map? plan) (= #{:before :after :changes} (set (keys plan)))
                 (or (nil? (:before plan)) (m/validate basis-schema (:before plan)))
                 (m/validate basis-schema (:after plan))
                 (vector? (:changes plan)) (<= (count (:changes plan)) max-changes))
    (refuse! :invalid-plan))
  (when (and (:before plan)
             (or (not= (source-id (:before plan)) (source-id (:after plan)))
                 (> (get-in plan [:before :max-tx]) (get-in plan [:after :max-tx]))))
    (refuse! :source-conflict))
  (when (and (nil? (:before plan))
             (not-every? #(and (= :added (:kind %)) (nil? (:before %))) (:changes plan)))
    (refuse! :invalid-bootstrap))
  (let [paths (mapv :path (:changes plan))]
    (when-not (= paths (vec (sort (distinct paths)))) (refuse! :invalid-change-order)))
  (doseq [{:keys [path kind before after] :as change} (:changes plan)]
    (when-not (and (= #{:path :kind :before :after} (set (keys change)))
                   (string? path) (<= 1 (count path) 512)
                   (not= before after)
                   (or (nil? before) (m/validate file-schema before))
                   (or (nil? after) (m/validate file-schema after))
                   (= kind (cond (nil? before) :added (nil? after) :removed :else :modified)))
      (refuse! :invalid-change)))
  (doseq [{:keys [path]} (:changes plan)] (files/path! path))
  (when (> (alength (.getBytes ^String (pr-str plan) "UTF-8")) max-plan-bytes)
    (refuse! :plan-too-large))
  plan)

(defn- entity [ns value]
  (into {} (map (fn [[k v]] [(keyword ns (name k)) v])) value))

(defn- descriptor-fields [value]
  (select-keys value [:id :by :room :target :policy :program :epoch]))

(defn- stored-descriptor [value]
  (into {} (map (fn [[k v]] [(keyword (name k)) v]))
        (select-keys value (map #(keyword "knowledge.stream" (name %))
                                [:id :by :room :target :policy :program :epoch]))))

(defn validate-binding!
  "Integrity/shape only for an original cursor row; not current authorization."
  [value expected]
  (when-not (= value (descriptor (dissoc value :id))) (refuse! :invalid-descriptor))
  (when (some? expected)
    (let [allowed (set (map :db/ident (filter #(= "knowledge.stream" (namespace (:db/ident %))) schema)))
          required (disj allowed :knowledge.stream/active-batch)]
      (when-not (and (map? expected) (every? allowed (keys expected))
                     (every? #(contains? expected %) required)
                     (= (descriptor-fields value) (stored-descriptor expected))
                     (every? uuid? (map expected [:knowledge.stream/cursor-commit :knowledge.stream/cursor-root
                                                  :knowledge.stream/last-discovery :knowledge.stream/source]))
                     (pos-int? (:knowledge.stream/revision expected))
                     (< (:knowledge.stream/revision expected) Long/MAX_VALUE)
                     (integer? (:knowledge.stream/batch-generation expected))
                     (<= 0 (:knowledge.stream/batch-generation expected) Long/MAX_VALUE)
                     (#{:active :retiring :retired} (:knowledge.stream/state expected))
                     (integer? (:knowledge.stream/cursor-tx expected)) (<= 0 (:knowledge.stream/cursor-tx expected) Long/MAX_VALUE)
                     (integer? (:knowledge.stream/next-order expected))
                     (<= 0 (:knowledge.stream/next-order expected) (- Long/MAX_VALUE max-changes))
                     (or (not (contains? expected :knowledge.stream/active-batch))
                         (uuid? (:knowledge.stream/active-batch expected))))
        (refuse! :invalid-stream-binding))))
  expected)

(defn- pending-for-path [db id path]
  (d/q '[:find [?e ...] :in $ ?stream ?path
         :where [?e :knowledge.item/stream ?stream] [?e :knowledge.item/path ?path]
                [?e :knowledge.item/state :pending]] db id path))

(defn discovery-id [descriptor expected plan root]
  (hasch/uuid [:simmis/knowledge-discovery-v1 (:id descriptor)
               expected (hasch/uuid plan) root]))

(defn discovery-tx
  "INTERNAL serialized composition. No artifact IO or pin operations here.
   CAS compares the complete original stream row. Exact ACK-loss replay is a
   no-op; any other cursor advance conflicts. Claimed/processed versions remain
   untouched. Displaced pending versions become superseded/removed, with the
   exact metadata delta retained in the discovery artifact."
  [db value expected plan root payload-ref]
  (when-not (= value (descriptor (dissoc value :id))) (refuse! :invalid-descriptor))
  (validate-plan! plan)
  (when-not (and (uuid? root) (uuid? payload-ref)) (refuse! :invalid-pin-or-artifact))
  (let [id (:id value) current (stream db id)
        did (discovery-id value expected plan root)
        digest (hasch/uuid plan)
        existing (row db :knowledge.discovery/id did)]
    (if existing
      (do
        (when-not (and (= id (:knowledge.discovery/stream existing))
                       (= digest (:knowledge.discovery/payload-id existing))
                       (= payload-ref (:knowledge.discovery/payload existing))
                       (= (inc (or (:knowledge.stream/revision expected) 0)) (:knowledge.discovery/revision existing))
                       (= (:knowledge.stream/cursor-root expected) (:knowledge.discovery/before-root existing))
                       (= root (:knowledge.discovery/after-root existing)))
          (refuse! :discovery-conflict))
        [])
      (do
        (when-not (= expected current) (refuse! :cursor-conflict))
        (admissible! current)
        ;; Initial policy serializes discovery with batch retention/processing.
        ;; Otherwise moving/releasing the cursor pin could collect a claimed
        ;; source before its separate frozen-input artifact has been retained.
        (when (:knowledge.stream/active-batch current) (refuse! :batch-active))
        (when (d/q '[:find ?e . :in $ ?stream
                     :where [?e :knowledge.item/stream ?stream]
                            [?e :knowledge.item/state :claimed]] db id)
          (refuse! :orphaned-claim))
        (when (and current
                   (not (and (pos-int? (:knowledge.stream/revision current))
                             (< (:knowledge.stream/revision current) Long/MAX_VALUE)
                             (integer? (:knowledge.stream/batch-generation current))
                             (<= 0 (:knowledge.stream/batch-generation current) Long/MAX_VALUE)
                             (integer? (:knowledge.stream/next-order current))
                             (<= 0 (:knowledge.stream/next-order current) (- Long/MAX_VALUE max-changes)))))
          (refuse! :invalid-stream-counters))
        (when (and current (not= (descriptor-fields value) (stored-descriptor current)))
          (refuse! :descriptor-conflict))
        (when-not (if current
                    (and (= (:knowledge.stream/source current) (source-id (:after plan)))
                         (= (:knowledge.stream/source current) (source-id (:before plan)))
                         (= (:knowledge.stream/cursor-commit current) (get-in plan [:before :commit]))
                         (= (:knowledge.stream/cursor-tx current) (get-in plan [:before :max-tx])))
                    (nil? (:before plan)))
          (refuse! :baseline-conflict))
        (when (= root (:knowledge.stream/cursor-root current)) (refuse! :pin-reused))
        (let [revision (inc (or (:knowledge.stream/revision current) 0))
              start (or (:knowledge.stream/next-order current) 0)
              pending (filterv #(and (get-in % [:after]) (= :trigger (get-in % [:after :role]))) (:changes plan))
              eligible (filterv #(let [metadata (:after %) iid (item-id id (:path %) metadata)
                                      old (row db :knowledge.item/id iid)]
                                  (when (and old (not (#{:pending :processed :skipped :superseded :removed}
                                                       (:knowledge.item/state old))))
                                    (refuse! :invalid-item-state))
                                  ;; Only an explicit future requeue operation
                                  ;; may revisit an already processed/skipped version.
                                  (not (#{:processed :skipped :claimed :pending} (:knowledge.item/state old)))) pending)
              new-ids (set (map #(item-id id (:path %) (:after %)) pending))
              superseded (mapcat (fn [{:keys [path after]}]
                                   (for [eid (pending-for-path db id path)
                                         :let [old (d/pull db '[:knowledge.item/id] eid)]
                                         :when (not (contains? new-ids (:knowledge.item/id old)))]
                                     [{:db/id eid :knowledge.item/state (if after :superseded :removed)
                                       :knowledge.item/discovery did}
                                      [:db/retract eid :knowledge.item/pending-stream id]])) (:changes plan))
              queued (map-indexed
                      (fn [n {:keys [path after]}]
                        (let [iid (item-id id path after) old (row db :knowledge.item/id iid)]
                          (cond-> (entity "knowledge.item" {:id iid :stream id :path path
                                                            :pending-stream id :content (:content after) :size (long (:size after))
                                                            :mode (long (:mode after)) :state :pending
                                                            :order (long (+ start n)) :discovery did})
                            old (assoc :db/id (:db/id old))))) eligible)
              cursor (merge (entity "knowledge.stream" value)
                            {:knowledge.stream/revision (long revision)
                             :knowledge.stream/next-order (long (+ start (count eligible)))
                             :knowledge.stream/batch-generation (long (or (:knowledge.stream/batch-generation current) 0))
                             :knowledge.stream/state :active
                             :knowledge.stream/cursor-commit (get-in plan [:after :commit])
                             :knowledge.stream/source (source-id (:after plan))
                             :knowledge.stream/cursor-tx (long (get-in plan [:after :max-tx]))
                             :knowledge.stream/cursor-root root :knowledge.stream/last-discovery did})
              event (cond-> {:knowledge.discovery/id did :knowledge.discovery/stream id
                             :knowledge.discovery/revision (long revision) :knowledge.discovery/payload payload-ref
                             :knowledge.discovery/payload-id digest :knowledge.discovery/after-root root
                             :knowledge.discovery/old-root-released? (nil? current)}
                      current (assoc :knowledge.discovery/before-root (:knowledge.stream/cursor-root current)))]
          (into [event (cond-> cursor current (assoc :db/id (:db/id (row db :knowledge.stream/id id))))]
                (concat (mapcat identity superseded) queued)))))))

(defn discover!
  "INTERNAL artifact-retaining journal write, after HOST authorization + pin.
   Returns the exact durable discovery id. It grants no permission to release
   the old pin or dispatch: the owning HOST lifecycle must recheck those gates."
  [conn value expected plan root]
  (when-not (= value (descriptor (dissoc value :id))) (refuse! :invalid-descriptor))
  (validate-plan! plan)
  (let [id (discovery-id value expected plan root)]
    (artifact/publish-value!
     (artifact/datahike-store conn) plan
     (fn [ref] (d/transact conn [[:db.fn/call discovery-tx value expected plan root ref]])))
    id))
