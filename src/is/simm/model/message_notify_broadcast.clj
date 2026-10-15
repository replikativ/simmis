(ns is.simm.model.message-notify-broadcast
  "Server-side per-user mention notifications. When a chat message @mentions
   parties, publish a private notification to each mentioned party's OWN topic
   `:notify/<party-id>` — auth-gated to that party in
   `web/data-plane-authorized?`, so a payload only ever reaches its recipient
   (no shared-topic client-side filtering).

   Beyond the fan-out (`notify-message!`, which never notifies the author),
   this namespace owns the per-room notification preference
   (`notify-pref-level` / `set-notify-pref!`) and the per-room read cursors
   that unread counts are derived from (`set-read-cursor!`, `mark-read!`,
   `unread-counts-for-party`). Design note:
   doc/archive/mentions-notifications-contacts-design.md."
  (:require [clojure.string :as str]
            [datahike.api :as d]
            [is.simm.model.system-db :as system-db]
            [is.simm.model.parties :as parties]
            [is.simm.model.rooms :as rooms]
            [is.simm.model.room-databases :as room-dbs]
            [is.simm.model.references :as refs]
            [is.simm.model.db :as db]
            [kabel.pubsub :as pubsub]
            [kabel.pubsub.protocol :as proto]
            [taoensso.telemere :as log]))

(defn notify-topic
  "The per-user notification topic for `party-id` (a keyword `:notify/<uuid>`)."
  [party-id]
  (keyword "notify" (str party-id)))

(defn notify-topic?
  "If `topic` is a per-user notify topic, return its party-uuid, else nil.
   Used by the pubsub subscribe gate to authorize a party to its OWN stream."
  [topic]
  (when (and (keyword? topic) (= "notify" (namespace topic)))
    (try (java.util.UUID/fromString (name topic)) (catch Exception _ nil))))

(defn ensure-topic-registered!
  "Register `party-id`'s notify topic if not already. Idempotent."
  [peer party-id]
  (let [t (notify-topic party-id)]
    (when (and peer (not (pubsub/topic-registered? peer t)))
      (pubsub/register-topic! peer t {:strategy (proto/pub-sub-only-strategy nil)}))))

(defn register-all-parties!
  "Register a notify topic for every existing party so their clients can
   subscribe on login. New parties register lazily on first publish. Best-effort."
  [peer]
  (try
    (when-let [sys-db (some-> (system-db/get-conn) deref)]
      (let [pids (d/q '[:find [?pid ...] :where [?e :party/id ?pid]] sys-db)]
        (doseq [pid pids] (ensure-topic-registered! peer pid))
        (log/log! {:level :info :id ::notify-topics-registered
                   :data {:party-count (count pids)}})))
    (catch Throwable t
      (log/log! {:level :warn :id ::notify-register-failed
                 :data {:error (ex-message t)}}))))

(declare notify-pref-level popup?)

(defn- strip+truncate [html n]
  (let [t (-> (str html)
              (str/replace #"<[^>]+>" " ")
              (str/replace #"\s+" " ")
              str/trim)]
    (if (> (count t) n) (str (subs t 0 n) "…") t)))

(defn notify-message!
  "For a just-persisted message, fan out a notification event to every HUMAN
   member of the room except the author. Each event carries `:mentioned?` (was
   this recipient @mentioned) so the client can badge every message but only
   pop for mentions (the `:mentions` default). `content` is the message HTML;
   `room-uuid`/`author-uuid` its room and sender. Best-effort — never breaks
   persistence. Called from the room-agents projector and the human persister."
  [{:keys [room-uuid author-uuid content]}]
  (try
    (when-let [peer @db/server-peer]
      (let [members (rooms/get-room-humans room-uuid)]
        (when (seq members)
          (let [handles        (refs/extract-user-mentions content)
                mentioned-pids  (into #{} (keep #(:party/id (parties/get-party-by-handle %))
                                                handles))
                author  (when author-uuid (parties/get-party author-uuid))
                room    (when room-uuid (rooms/get-room room-uuid))
                base    {:kind :message
                         :room-id room-uuid
                         :room-name (:room/name room)
                         :author-id author-uuid
                         :author-name (or (:party/display-name author)
                                          (:party/handle author) "Someone")
                         :preview (strip+truncate content 140)}]
            (doseq [m members
                    :let [pid (:party/id m)]
                    :when (and pid (not= pid author-uuid))]
              (let [mentioned? (contains? mentioned-pids pid)]
                (ensure-topic-registered! peer pid)
                (pubsub/publish! peer (notify-topic pid)
                                 (assoc base
                                        :mentioned? mentioned?
                                        :popup? (popup? (notify-pref-level pid room-uuid)
                                                        mentioned?)))))))))
    (catch Throwable t
      (log/log! {:level :debug :id ::notify-message-failed
                 :data {:room-uuid (str room-uuid) :error (ex-message t)}}))))

;; =============================================================================
;; Durable read cursors + unread counts (mentions-notifications design, step 3B)
;; =============================================================================

(defn- pair-key [party-id room-id] (str party-id ":" room-id))
(def ^:private cursor-key pair-key)

;; --- Per-room notification level (:all | :mentions | :none, default :mentions) ---

(defn notify-pref-level
  "The notification level for (party, room); :mentions when unset."
  [party-id room-id]
  (or (when-let [conn (system-db/get-conn)]
        (d/q '[:find ?lvl . :in $ ?k
               :where [?e :notify-pref/key ?k] [?e :notify-pref/level ?lvl]]
             @conn (pair-key party-id room-id)))
      :mentions))

(defn set-notify-pref!
  "Upsert the notification level for (party, room). `level` ∈ #{:all :mentions :none}."
  [party-id room-id level]
  (when (and (system-db/get-conn) (#{:all :mentions :none} level))
    (d/transact (system-db/get-conn)
      [{:notify-pref/key (pair-key party-id room-id) :notify-pref/level level}])
    level))

(defn notify-prefs-for-party
  "Explicit per-room levels for `party-id` as {room-id-str → level} (unset rooms
   are omitted; the client applies the :mentions default)."
  [party-id]
  (if-let [db (some-> (system-db/get-conn) deref)]
    (into {}
          (for [[k lvl] (d/q '[:find ?k ?lvl :in $ ?prefix
                               :where
                               [?e :notify-pref/key ?k]
                               [?e :notify-pref/level ?lvl]
                               [(clojure.string/starts-with? ?k ?prefix)]]
                             db (str party-id ":"))]
            [(subs k (inc (.indexOf ^String k ":"))) lvl]))
    {}))

(defn- popup?
  "Whether a message should raise a popup for a member, given their level."
  [level mentioned?]
  (case level
    :all  true
    :none false
    (boolean mentioned?)))   ; :mentions (default)

(defn read-cursor-at
  "The `:read-cursor/at` instant for (party, room), or nil if none."
  [party-id room-id]
  (when-let [conn (system-db/get-conn)]
    (d/q '[:find ?at . :in $ ?k
           :where [?e :read-cursor/key ?k] [?e :read-cursor/at ?at]]
         @conn (cursor-key party-id room-id))))

(defn set-read-cursor!
  "Upsert the read cursor for (party, room) to `inst`."
  [party-id room-id inst]
  (when-let [conn (system-db/get-conn)]
    (d/transact conn [{:read-cursor/key (cursor-key party-id room-id)
                       :read-cursor/at inst}])))

(defn- room-unread
  "Count messages in `room-uuid`'s content DB sent strictly after `since`
   (a Date). `since` nil ⇒ 0 (caught up)."
  [room-uuid since]
  (if (nil? since)
    0
    (if-let [conn (some-> (room-dbs/get-room-db-scope room-uuid)
                          room-dbs/connect-room-database)]
      (->> (d/q '[:find [?t ...] :where [?e :S.Message/sent-at ?t]] @conn)
           (filter #(pos? (compare % since)))
           count)
      0)))

(defn mark-read!
  "Advance (party, room)'s read cursor to now, clearing its unread. Returns nil."
  [party-id room-id]
  (set-read-cursor! party-id room-id (java.util.Date.))
  nil)

(defn- party-room-ids [sys-db party-id]
  (when (some? party-id)
    (d/q '[:find [?rid ...] :in $ ?pid
           :where [?p :party/id ?pid] [?r :room/parties ?p] [?r :room/id ?rid]]
         sys-db party-id)))

(defn unread-counts-for-party
  "Compute {room-id-str → unread} for every room `party-id` is in. A room with no
   cursor is initialised to now (the party starts caught-up — no giant backlog on
   first login) and reports 0; thereafter unread accrues durably from new
   messages until the party opens the room (mark-read!)."
  [party-id]
  (if-let [sys-db (some-> (system-db/get-conn) deref)]
    (into {}
          (for [rid (party-room-ids sys-db party-id)]
            (let [cursor (read-cursor-at party-id rid)]
              (when (nil? cursor)
                (set-read-cursor! party-id rid (java.util.Date.)))
              [(str rid) (room-unread rid cursor)])))
    {}))
