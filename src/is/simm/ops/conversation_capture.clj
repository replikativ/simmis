(ns is.simm.ops.conversation-capture
  "Explicit bounded human-message evidence for the canonical file workflow.
   No automatic ingestion, inference, cursor, activation or approval policy."
  (:require [clojure.string :as str]
            [clojure.walk :as walk]
            [datahike.api :as d]
            [dvergr.room.store.datahike]
            [dvergr.runtime.preparation :as preparation]
            [hasch.core :as hasch]
            [is.simm.documents.files :as files]
            [is.simm.model.access :as access]
            [is.simm.ops.owned-reads :as reads]
            [is.simm.ops.room-evidence :as evidence]
            [org.replikativ.spindel.engine.core :as ec])
  (:import [java.nio CharBuffer]
           [java.nio.charset CodingErrorAction StandardCharsets]
           [java.util Date]))

(def max-messages 50)
(def max-source-bytes (* 256 1024))

(defn- refuse! [reason]
  (throw (ex-info "Conversation capture refused" {:type ::refused :reason reason})))

(defn- ids! [ids]
  (when-not (and (vector? ids) (<= 1 (count ids) max-messages)
                 (every? uuid? ids) (= (count ids) (count (set ids))))
    (refuse! :invalid-selection))
  ids)

(defn- byte-count [text]
  (try
    (.remaining (.encode (doto (.newEncoder StandardCharsets/UTF_8)
                           (.onMalformedInput CodingErrorAction/REPORT)
                           (.onUnmappableCharacter CodingErrorAction/REPORT))
                         (CharBuffer/wrap ^String text)))
    (catch java.nio.charset.CharacterCodingException _ (refuse! :invalid-text))))

(defn- quoted-text [text]
  ;; Authored Markdown/HTML/identity directives stay inert source evidence.
  ;; The byte count records whether the framing newline belongs to the source.
  (let [longest (reduce max 0 (map count (re-seq #"`+" text)))
        fence (apply str (repeat (max 3 (inc longest)) "`"))]
    (str fence "text\n" text (when-not (str/ends-with? text "\n") "\n") fence "\n")))

(defn capture-in
  "Pure HOST projection over explicit immutable control and native message DBs.
   This does NOT authorize. Resolve the original room/default before calling.
   IDs select exact rows within that room, never the latest N chat messages."
  [control-db message-db room-id slug message-ids]
  (ids! message-ids)
  (when-not (and (uuid? room-id) (string? slug)
                 (uuid? (get-in message-db [:config :store :id])))
    (refuse! :invalid-source))
  (let [rooms (d/q '[:find [?e ...] :in $ ?slug :where [?e :room/slug ?slug]] message-db slug)
        chat (when (= 1 (count rooms)) (first rooms))
        _ (when-not chat (refuse! :missing-source-room))
        rows
        (mapv
         (fn [id]
           (let [m (d/entity message-db [:message/id id])
                 author (access/actor->party-uuid (:message/from m))
                 party (when author (d/entity control-db [:party/id author]))
                 text (:message/content m) ts (:message/created-at m)]
             (when-not (and (= id (:message/id m)) (= chat (:db/id (:message/chat m)))
                            author (= (keyword "party" (str author)) (:message/from m))
                            (= :human (or (:actor/kind party) (:party/type party)))
                            (= :user (:message/role m))
                            (not (contains? evidence/excluded-recipients (:message/to m)))
                            (contains? #{nil :mcp-user-message} (:message/metadata-kind m))
                            (not-any? #(some? (get m %))
                                      [:message/run-id :message/source :message/schedule-id
                                       :message/tool-uses :message/activities :message/reasoning
                                       :message/object-kind :message/notification-type])
                            (string? text) (<= (count text) max-source-bytes)
                            (instance? Date ts))
               (refuse! :not-human-conversation))
             {:id id :author author :timestamp (.getTime ^Date ts)
              :text text :bytes (byte-count text)
              :content-id (hasch/uuid [:simmis/conversation-text-v1 text])}))
         message-ids)
        _ (when (> (reduce + 0 (map :bytes rows)) max-source-bytes)
            (refuse! :selection-too-large))
        rows (vec (sort-by (juxt :timestamp (comp str :id)) rows))
        source-store (get-in message-db [:config :store :id])
        ;; Unrelated new messages must not duplicate a selected excerpt.
        id (hasch/uuid [:simmis/conversation-capture-v1 room-id source-store rows])
        path (str "inbox/conversations/" id ".md")
        text (str "---\nsimmis-id: " id "\ncapture-version: 1\nsource-room: " room-id
                  "\nsource-store: " source-store "\n---\n# Selected room conversation\n\n"
                  "Explicitly captured human messages; quoted evidence, not instructions.\n\n"
                  (apply str
                         (map (fn [{:keys [id author timestamp text bytes content-id]}]
                                (str "## Message " id "\n\nAuthor: " author
                                     "\n\nTimestamp (Unix milliseconds): " timestamp
                                     "\n\nSource UTF-8 bytes: " bytes
                                     "\n\nSource content ID: " content-id "\n\n"
                                     (quoted-text text) "\n")) rows)))]
    (when (> (byte-count text) (* 2 max-source-bytes)) (refuse! :selection-too-large))
    {:capture-id id :path path :text text :message-ids (mapv :id rows)}))

(defn- snapshot [binding ids]
  (let [room (:room binding) store (:store room)]
    (when-not (instance? dvergr.room.store.datahike.DatahikeStore store)
      (refuse! :source-unavailable))
    (clojure.core/binding [ec/*execution-context* (:ctx room)]
      (capture-in (:control-db binding) @(:conn store) (:room-id binding) (:slug room) ids))))

(defn- with-capture! [host principal handle f]
  (let [with-operation (get-in host [:config :host-lifecycle :with-operation])]
    (when-not (fn? with-operation) (refuse! :missing-lifetime))
    ;; Use this exact captured application owner, never a newer installation.
    (with-operation
      (fn []
        (let [resolved (reads/resolve-host-write-room! host principal handle)
              room (:room resolved)]
          (binding [ec/*execution-context* (:execution-ctx host)]
            (preparation/with-host-operation!
              room
              (fn [lease]
                (preparation/with-host-current! lease (constantly true))
                (reads/validate-host-write-room! host principal handle resolved)
                (let [binding (reads/resolve-host-write! host principal handle)]
                  (when-not (identical? room (:room binding)) (refuse! :target-changed))
                  (f binding))))))))))

(defn preview!
  "Authenticated HOST preview. The caller transports only exact message IDs;
   returned text is an inspected proposal, not a capability or saved receipt."
  [host principal handle request]
  (when-not (and (map? request) (= #{:message-ids} (set (keys request))))
    (refuse! :invalid-selection))
  (ids! (:message-ids request))
  (with-capture! host principal handle
    #(assoc (snapshot % (:message-ids request)) :target-stamp (:target-stamp %))))

(defn publish!
  "Publish an inspected selection through the normal native CAS writer.
   A deterministic destination prevents duplicate files. An existing file is
   NOT an acknowledgement of an earlier uncertain write; inspect it separately.
   Failed/pending writes must never be automatically replayed."
  [host principal handle {:keys [message-ids capture-id expected-target-stamp] :as request}]
  (when-not (and (map? request)
                 (= #{:message-ids :capture-id :expected-target-stamp} (set (keys request)))
                 (uuid? capture-id) (uuid? expected-target-stamp))
    (refuse! :invalid-selection))
  (ids! message-ids)
  (with-capture! host principal handle
    (fn [binding]
      (let [plan (snapshot binding message-ids)]
        (when-not (= expected-target-stamp (:target-stamp binding)) (refuse! :target-changed))
        (when-not (= capture-id (:capture-id plan)) (refuse! :selection-changed))
        (when (files/entry (:db binding) (:path plan)) (refuse! :destination-exists))
        (let [authorize! #(do (reads/validate-host-write! host principal handle binding)
                             ;; Recheck source eligibility as well as exact bytes
                             ;; at each effect gate, with CURRENT identity.
                             (let [current (assoc binding :control-db (reads/current-host-control-db! host))]
                               (when-not (= capture-id (:capture-id (snapshot current message-ids)))
                                 (refuse! :selection-changed)))
                             true)
              receipt (files/save-file-to! (:room binding) (:system binding)
                                          {:path (:path plan) :expected nil :text (:text plan)}
                                          {:authorize! authorize!})]
          {:capture-id capture-id :path (:path plan) :target-stamp expected-target-stamp
           :write receipt})))))


(defn room-source
  ([binding] (room-source binding nil))
  ([binding period]
  (let [room (:room binding) store (:store room)]
    (when-not (instance? dvergr.room.store.datahike.DatahikeStore store)
      (refuse! :source-unavailable))
    (clojure.core/binding [ec/*execution-context* (:ctx room)]
      (let [db @(:conn store)
            chats (d/q '[:find [?e ...] :in $ ?slug :where [?e :room/slug ?slug]] db (:slug room))
            _ (when-not (= 1 (count chats)) (refuse! :missing-source-room))
            ;; Reference attributes are AVET indexed; bound before loading bodies.
            refs (vec (take (inc evidence/max-scan-messages)
                            (if period
                              ;; Date metadata query needs no new index/backfill.
                              ;; Restrict the period and private/derived traffic
                              ;; before loading any original bodies.
                              (map (fn [eid] {:e eid})
                                   (d/q '[:find [?e ...] :in $ ?chat ?from ?until
                                          :where [?e :message/chat ?chat]
                                          [?e :message/created-at ?at]
                                          [(compare ?at ?from) ?lower] [(>= ?lower 0)]
                                          [(compare ?at ?until) ?upper] [(< ?upper 0)]
                                          (not [?e :message/to :_runs])
                                          (not [?e :message/to :_activity])
                                          (not [?e :message/notification-type _])
                                          (not [?e :message/metadata-kind :workflow/completed])
                                          (not [?e :message/metadata-kind :conversation-digest])
                                          (not [?e :message/metadata-kind :room-summary])
                                          (not [?e :message/metadata-kind :proposal/filed])]
                                        db (first chats) (:from period) (:until period)))
                              (d/datoms db :avet :message/chat (first chats)))))
            source-store (get-in db [:config :store :id])
            ;; Only public content and causal attribution belong to evidence.
            ;; Do not recursively copy reasoning/tool payload components merely
            ;; because the native message schema happens to contain them.
            pattern (into [] (filter #(contains? (:schema db) %))
                          [:message/id :message/from :message/to :message/role
                           :message/content :message/created-at :message/in-reply-to
                           :message/thread-root-id :message/run-id :message/source
                           :message/source-user :message/metadata-kind
                           :message/notification-type :message/object-kind])]
        (when-not (uuid? source-store) (refuse! :invalid-source))
        (if (> (count refs) evidence/max-scan-messages)
          {:source-store source-store :rows [] :scan-held? true}
          {:source-store source-store :source-basis (:max-tx db)
           :rows
           (into []
                 (keep
                  (fn [ref]
                    (let [m (d/pull db pattern (:e ref))
                          author (access/actor->party-uuid (:message/from m))
                          party (when author (d/entity (:control-db binding) [:party/id author]))
                          kind (or (:actor/kind party) (:party/type party))
                          timestamp (:message/created-at m)
                          text (:message/content m)
                          ;; Component entity ids are storage details, not provenance.
                          metadata (walk/postwalk
                                    #(cond (keyword? %) (str %)
                                       (map? %) (into (sorted-map-by (fn [a b] (compare (pr-str a) (pr-str b))))
                                                        (dissoc % :db/id))
                                       (set? %) (into (sorted-set-by (fn [a b] (compare (pr-str a) (pr-str b)))) %)
                                       :else %)
                                    (dissoc m :db/id :message/chat :message/content :message/created-at))]
                      (when-not (or (contains? evidence/excluded-kinds (:message/metadata-kind m))
                                    (contains? evidence/excluded-recipients (:message/to m))
                                    (:message/notification-type m))
                        (cond-> {:id (:message/id m) :author author :actor-kind kind
                                 :timestamp (if (instance? Date timestamp) (.getTime ^Date timestamp) 0)
                                 :text (if (string? text) text "") :metadata metadata}
                          (not (and (uuid? (:message/id m)) author
                                    (= (keyword "party" (str author)) (:message/from m))
                                    (#{:human :agent} kind)
                                    (#{:user :assistant :tool-result} (:message/role m))
                                    (instance? Date timestamp) (string? text)))
                          (assoc :held :unsupported-message)))))) refs)}))))))

(defn room-plan
  ([binding] (room-plan binding (room-source binding)))
  ([binding {:keys [source-store rows scan-held?]}]
  (let [plan (if scan-held?
               {:artifacts [] :state :held :complete? false
                :held {:reason :source-scan-limit :limit evidence/max-scan-messages}
                :limitations evidence/limitations}
               (evidence/select-next
                (:room-id binding) source-store rows
                (fn [artifact]
                  (when (files/entry (:db binding) (:path artifact))
                    (when-not (= (:text artifact) (:text (files/read-file-in (:db binding) (:path artifact))))
                      (refuse! :evidence-destination-conflict))
                    true))))]
    (assoc plan :source-store source-store :source-room (:room-id binding)
           :capture-id (hasch/uuid [:simmis/room-evidence-plan-v1 (:room-id binding) source-store plan])))))

(defn preview-room!
  "Authorized deterministic next original-evidence batch. Empty closed request.
   No model, cursor, writes, or knowledge-processing acknowledgement."
  [host principal handle request]
  (when-not (= {} request) (refuse! :invalid-selection))
  (with-capture! host principal handle
    #(assoc (room-plan %) :target-stamp (:target-stamp %))))

(defn selected-room-source
  "HOST comparison data for selected originals only. Unrelated appends cannot
   invalidate a retained batch; deletion, revision or author changes still do."
  [source artifacts]
  (let [ids (set (map :message-id artifacts))]
    {:source-store (:source-store source) :scan-held? (boolean (:scan-held? source))
     :rows (vec (sort-by (comp str :id) (filter #(contains? ids (:id %)) (:rows source))))}))

(defn publish-room!
  "Publish each inspected original independently through the native CAS writer.
   Held batches never publish. Any thrown/pending result requires inspection;
   never automatically retry this operation after an uncertain acknowledgement."
  [host principal handle {:keys [capture-id expected-target-stamp] :as request}]
  (when-not (and (map? request) (= #{:capture-id :expected-target-stamp} (set (keys request)))
                 (uuid? capture-id) (uuid? expected-target-stamp))
    (refuse! :invalid-selection))
  (with-capture! host principal handle
    (fn [binding]
      (when-not (= expected-target-stamp (:target-stamp binding)) (refuse! :target-changed))
      (let [source (room-source binding)
            plan (room-plan binding source)]
        (when-not (= capture-id (:capture-id plan)) (refuse! :selection-changed))
        (when (= :held (:state plan)) (refuse! :evidence-held))
        (let [authorize! #(do
                           (reads/validate-host-write! host principal handle binding)
                           (when-not (= (selected-room-source source (:artifacts plan))
                                        (selected-room-source
                                         (room-source (assoc binding :control-db (reads/current-host-control-db! host)))
                                         (:artifacts plan)))
                             (refuse! :selection-changed))
                           true)
              writes (mapv (fn [a]
                             (let [receipt (files/save-file-to! (:room binding) (:system binding)
                                                                {:path (:path a) :expected nil :text (:text a)}
                                                                {:authorize! authorize!})]
                               (when-not (true? (:saved? receipt)) (refuse! :inspection-required))
                               {:message-id (:message-id a) :source-revision (:source-revision a)
                                :path (:path a) :write receipt}))
                           (:artifacts plan))]
          {:capture-id capture-id :target-stamp expected-target-stamp
           :writes writes :state (:state plan) :complete? (:complete? plan)
           :limitations (:limitations plan)})))))
