(ns is.simm.model.summary-records
  "Room-local immutable summary revisions and independent projection receipts.
   These records do not schedule work or acknowledge wiki processing."
  (:require [datahike.api :as d]
            [dvergr.artifact :as artifact]
            [hasch.core :as hasch]
            [is.simm.model.summary-envelope :as envelope]
            [is.simm.model.room-summaries :as summaries]))

(def schema
  (mapv (fn [[ident type]]
          (cond-> {:db/ident ident :db/valueType type :db/cardinality :db.cardinality/one}
            (#{:room.summary/revision :summary.projection/id :summary.head/node :summary.path/id} ident)
            (assoc :db/unique :db.unique/value)
            (#{:room.summary/node :room.summary/room :room.summary/request :summary.head/room
               :summary.projection/revision :summary.projection/path} ident) (assoc :db/index true)))
        [[:room.summary/revision :db.type/uuid] [:room.summary/node :db.type/uuid]
         [:room.summary/room :db.type/uuid] [:room.summary/store :db.type/uuid]
         [:room.summary/kind :db.type/keyword] [:room.summary/timezone :db.type/string]
         [:room.summary/from :db.type/instant] [:room.summary/until :db.type/instant]
         [:room.summary/created-at :db.type/instant] [:room.summary/coverage-id :db.type/uuid]
         [:room.summary/request :db.type/uuid] [:room.summary/payload :db.type/store-ref]
         [:room.summary/model :db.type/string] [:room.summary/bundle :db.type/uuid]
         [:room.summary/target :db.type/uuid]
         [:room.summary/state :db.type/keyword] [:room.summary/generation :db.type/long]
         [:summary.head/node :db.type/uuid] [:summary.head/room :db.type/uuid]
         [:summary.head/revision :db.type/uuid] [:summary.head/generation :db.type/long]
         [:summary.path/id :db.type/uuid] [:summary.path/owner :db.type/uuid]
         [:summary.path/content :db.type/uuid] [:summary.path/state :db.type/keyword]
         [:summary.projection/id :db.type/uuid] [:summary.projection/revision :db.type/uuid]
         [:summary.projection/source-basis :db.type/long]
         [:summary.projection/kind :db.type/keyword] [:summary.projection/target :db.type/uuid]
         [:summary.projection/room :db.type/uuid] [:summary.projection/node :db.type/uuid]
         [:summary.projection/path :db.type/string] [:summary.projection/state :db.type/keyword]
         [:summary.projection/before :db.type/uuid] [:summary.projection/content :db.type/uuid]
         [:summary.projection/intent :db.type/store-ref] [:summary.projection/receipt :db.type/store-ref]]))

(def fields (into [:db/id] (map :db/ident (filter #(= "room.summary" (namespace (:db/ident %))) schema))))
(def projection-fields
  (into [:db/id] (map :db/ident (filter #(= "summary.projection" (namespace (:db/ident %))) schema))))

(defn installed? [db] (contains? (:schema db) :room.summary/revision))

(defn ensure-schema!
  "Only a current authorized HOST writer may call this; reads never migrate."
  [conn]
  (let [missing (filterv #(not (contains? (:schema @conn) (:db/ident %))) schema)]
    (when (seq missing) (d/transact conn missing))))

(defn revision-in [db id]
  (when (and (installed? db) (uuid? id))
    (when-let [eid (:e (first (d/datoms db :avet :room.summary/revision id)))]
      (d/pull db fields eid))))

(defn projection-in [db id]
  (when (and (installed? db) (uuid? id))
    (when-let [eid (:e (first (d/datoms db :avet :summary.projection/id id)))]
      (d/pull db projection-fields eid))))

(defn head-in [db room node]
  (when-not (and (uuid? room) (uuid? node))
    (throw (ex-info "Invalid summary head identity" {})))
  (when (installed? db)
    (when-let [eid (:e (first (d/datoms db :avet :summary.head/node node)))]
      (let [row (d/pull db '[*] eid)]
        (when-not (= room (:summary.head/room row))
          (throw (ex-info "Summary head belongs to another room" {})))
        row))))

(defn revisions-in
  "Metadata only. A bounded latest projection; readers must preserve the
   :complete? flag instead of claiming an unlimited historical archive."
  [db room limit]
  (when-not (and (uuid? room) (integer? limit) (<= 1 limit 256))
    (throw (ex-info "Invalid summary listing" {})))
  (if-not (installed? db)
    {:rows [] :complete? true}
    (let [{:keys [rows total]}
          (reduce (fn [{:keys [rows total]} ref]
                    {:rows (->> (conj rows (d/pull db fields (:e ref)))
                                (sort-by (juxt :room.summary/until :room.summary/created-at
                                               (comp str :room.summary/revision)) #(compare %2 %1))
                                (take limit) vec)
                     :total (inc total)})
                  {:rows [] :total 0} (d/datoms db :avet :room.summary/room room))]
      {:rows rows :total total :complete? (<= total limit)})))

(defn latest-in [db room node]
  (when-let [head (head-in db room node)]
    (revision-in db (:summary.head/revision head))))

(defn heads-in [db room limit]
  (when-not (and (uuid? room) (integer? limit) (<= 1 limit 256))
    (throw (ex-info "Invalid summary head listing" {})))
  (if-not (installed? db)
    {:rows [] :total 0 :complete? true}
    (let [{:keys [rows total]}
          (reduce (fn [{:keys [rows total]} datom]
                    (let [head (d/pull db '[*] (:e datom))
                          row (revision-in db (:summary.head/revision head))]
                      (when-not (= room (:room.summary/room row))
                        (throw (ex-info "Summary head revision is missing or foreign" {})))
                      {:total (inc total)
                       :rows (->> (conj rows row)
                                  (sort-by (juxt :room.summary/until (comp str :room.summary/revision)) #(compare %2 %1))
                                  (take limit) vec)}))
                  {:rows [] :total 0} (d/datoms db :avet :summary.head/room room))]
      {:rows rows :total total :complete? (<= total limit)})))

(defn- record-tx [db row previous]
  (if-let [prior (revision-in db (:room.summary/revision row))]
    (do (when-not (= row (dissoc prior :db/id))
          (throw (ex-info "Summary revision already names another record" {}))) [])
    (let [head (head-in db (:room.summary/room row) (:room.summary/node row))]
      (when-not (and (= previous (:summary.head/revision head))
                     (= (:room.summary/generation row) (inc (or (:summary.head/generation head) 0))))
        (throw (ex-info "A newer summary head already exists" {:type ::head-changed})))
      [row {:summary.head/node (:room.summary/node row) :summary.head/room (:room.summary/room row)
            :summary.head/revision (:room.summary/revision row)
            :summary.head/generation (:room.summary/generation row)
            :db/id (or (:db/id head) -1)}])))

(declare payload! target-id)

(defn publish!
  "Persist the validated report AND exact input bytes in the Room's own store.
   Request/world checkpoint retirement cannot substitute or erase this basis.
   Caller independently verifies certified execution and current authority."
  [conn {:keys [selection report program coverage-id inputs created-at previous generation] :as payload}]
  (summaries/validate-report! selection report)
  (envelope/validate! selection inputs)
  (when-not (and (uuid? coverage-id) (map? inputs) (inst? created-at)
                 (or (nil? previous) (uuid? previous)) (pos-int? generation)
                 (= (:target program) (target-id (:target payload)))
                 (= (get-in @conn [:config :store :id]) (:store selection)))
    (throw (ex-info "Summary native source does not match its Room store" {})))
  (let [id (summaries/revision-id selection report program)
        period (:period selection)
        row {:room.summary/revision id :room.summary/node (summaries/node-id selection)
             :room.summary/room (:room selection) :room.summary/store (:store selection)
             :room.summary/kind (:kind period) :room.summary/timezone (:timezone period)
             :room.summary/from (:from period) :room.summary/until (:until period)
             :room.summary/created-at created-at :room.summary/request (:request program)
             :room.summary/coverage-id coverage-id :room.summary/model (:model program)
             :room.summary/bundle (:bundle program) :room.summary/generation generation
             :room.summary/target (:target program)
             :room.summary/state (if (summaries/ready? selection report) :ready :partial)}]
    (if-let [existing (revision-in @conn id)]
      (when-not (= (dissoc payload :created-at) (dissoc (payload! conn existing) :created-at))
        (throw (ex-info "Summary revision already names another payload" {})))
      (do
        (artifact/prepare-value payload (* 8 1024 1024))
        (artifact/publish-value!
         (artifact/datahike-store conn) payload
         #(d/transact conn [[:db.fn/call record-tx (assoc row :room.summary/payload %) previous]]))))
    (revision-in @conn id)))

(defn payload! [conn row]
  (let [envelope (artifact/get-bounded-value (artifact/datahike-store conn)
                                             (:room.summary/payload row) (* 8 1024 1024))
        {:keys [selection report program] :as payload} (:value envelope)]
    (envelope/validate! selection (:inputs payload))
    (when-not (and envelope (= (:room.summary/revision row) (summaries/revision-id selection report program))
                   (= (:room.summary/node row) (summaries/node-id selection))
                   (= (:room.summary/room row) (:room selection))
                   (= (:room.summary/store row) (:store selection)
                      (get-in @conn [:config :store :id]))
                   (= (:room.summary/request row) (:request program)))
      (throw (ex-info "Summary retained evidence is unavailable" {})))
    (when-not (and (= (:room.summary/generation row) (:generation payload))
                   (= (:room.summary/target row) (:target program) (target-id (:target payload))))
      (throw (ex-info "Summary retained destination or generation changed" {})))
    (when-not (= (select-keys row [:room.summary/from :room.summary/until :room.summary/kind
                                  :room.summary/timezone :room.summary/model :room.summary/bundle
                                  :room.summary/state :room.summary/coverage-id :room.summary/created-at])
                 {:room.summary/from (get-in selection [:period :from])
                  :room.summary/until (get-in selection [:period :until])
                  :room.summary/kind (get-in selection [:period :kind])
                  :room.summary/timezone (get-in selection [:period :timezone])
                  :room.summary/model (:model program) :room.summary/bundle (:bundle program)
                  :room.summary/state (if (summaries/ready? selection report) :ready :partial)
                  :room.summary/coverage-id (:coverage-id payload) :room.summary/created-at (:created-at payload)})
      (throw (ex-info "Summary indexed metadata does not match its retained evidence" {})))
    payload))

(defn- reserve-projection-tx [db intent]
  (if-let [prior (projection-in db (:summary.projection/id intent))]
    (do (when-not (= intent (dissoc prior :db/id :summary.projection/receipt))
          (throw (ex-info "Summary projection has another or uncertain intent" {}))) [])
    [intent]))

(defn reserve-projection! [conn intent]
  (d/transact conn [[:db.fn/call reserve-projection-tx intent]])
  (projection-in @conn (:summary.projection/id intent)))

(defn- projection-state-tx [db id expected state receipt]
  (let [row (projection-in db id)]
    (when-not (= expected (:summary.projection/state row))
      (throw (ex-info "Summary projection no longer owns this delivery" {})))
    [(cond-> {:db/id (:db/id row) :summary.projection/state state}
       receipt (assoc :summary.projection/receipt receipt))]))

(defn projection-state! [conn id expected state & [receipt]]
  (d/transact conn [[:db.fn/call projection-state-tx id expected state receipt]])
  (projection-in @conn id))

(defn path-id [room target path]
  (hasch/uuid [:simmis/generated-summary-path-v1 room target path]))

(def target-keys [:repository :store :backend :branch :ref])
(defn target-id [target]
  (when-not (and (every? uuid? ((juxt :repository :store) target))
                 (keyword? (:backend target)) (keyword? (:branch target))
                 (string? (:ref target)) (<= 1 (count (:ref target)) 4096))
    (throw (ex-info "Invalid summary destination" {})))
  (hasch/uuid [:simmis/summary-destination-v1 (select-keys target target-keys)]))

(defn path-in [db id]
  (when-not (uuid? id) (throw (ex-info "Invalid generated path identity" {})))
  (when (installed? db)
    (when-let [eid (:e (first (d/datoms db :avet :summary.path/id id)))]
      (d/pull db '[*] eid))))

(defn- claim-projection-tx [db id expected-owner]
  (let [row (projection-in db id)
        key (path-id (:summary.projection/room row) (:summary.projection/target row) (:summary.projection/path row))
        path (path-in db key)
        head (when (#{:markdown :chat} (:summary.projection/kind row))
               (head-in db (:summary.projection/room row) (:summary.projection/node row)))]
    (when-not (and row (= :reserved (:summary.projection/state row))
                   (= expected-owner (:summary.path/owner path))
                   (or (nil? path) (= :published (:summary.path/state path)))
                   (or (not (#{:markdown :chat} (:summary.projection/kind row)))
                       (= (:summary.projection/revision row) (:summary.head/revision head))))
      (throw (ex-info "Generated summary path is busy or its generation changed" {:type ::projection-held})))
    [{:db/id (:db/id row) :summary.projection/state :writing}
     {:db/id (or (:db/id path) -1) :summary.path/id key
      :summary.path/owner id :summary.path/state :writing}]))

(defn claim-projection!
  "Only the acknowledged reserved->writing winner may perform the effect.
   A later observation of writing never authorizes replay."
  [conn id expected-owner]
  (d/transact conn [[:db.fn/call claim-projection-tx id expected-owner]])
  (projection-in @conn id))

(defn- finish-projection-tx [db id receipt]
  (let [row (projection-in db id)
        key (path-id (:summary.projection/room row) (:summary.projection/target row) (:summary.projection/path row))
        path (path-in db key)]
    (when-not (and (= :writing (:summary.projection/state row))
                   (= id (:summary.path/owner path)) (= :writing (:summary.path/state path)))
      (throw (ex-info "Generated summary delivery lost its owner" {})))
    [{:db/id (:db/id row) :summary.projection/state :published :summary.projection/receipt receipt}
     {:db/id (:db/id path) :summary.path/state :published
      :summary.path/content (:summary.projection/content row)}]))

(defn finish-projection! [conn id receipt]
  (d/transact conn [[:db.fn/call finish-projection-tx id receipt]])
  (projection-in @conn id))
