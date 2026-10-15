(ns is.simm.runtimes.telegram
  "Telegram channel for simmis (Stage 4 of doc/dvergr-integration-plan.md).

   Uses dvergr's telegram channel + medium adapter end to end. Each Telegram
   chat (the founders' group, or a DM with the bot) maps to one dvergr room
   with slug `tg-<chat-id>` and type :telegram-mirror. Inbound messages post
   into the room AS the sender's mapped party; simmis renders the room like
   any other (rich adapter), while the telegram side gets agent replies via
   dvergr's thin-adapter egress/mirror.

   Identity: link-on-first-contact. Unknown Telegram senders are logged
   prominently (::unlinked-telegram-user, with numeric id + username) and
   held as lightweight external parties. Link one to a real account with:

     (is.simm.runtimes.telegram/link-telegram! party-id tg-user-id username)

   after which their messages author as that party (existing external rows
   for the id are absorbed). Founders link once; ids are canonical, the
   username is informational.

   The bot token comes from ./config.local.edn {:telegram {:token ...}} or
   TELEGRAM_BOT_TOKEN (dvergr.substrate.config/telegram-token). The allowlist
   {:telegram {:allowed-users #{123456 \"@name\"}}} decides who may use the
   bot: dvergr authenticates a sender only when the allowlist is NOT empty and
   lists it (as the principal `telegram/<id>`), so an empty or missing
   allowlist refuses every sender.

   dvergr's `:telegram {:owners [id …]}` (numeric ids, acting as the process
   itself) is read from a dvergr daemon's config. simmis runs no daemon and
   passes none, so no Telegram sender is an owner here."
  (:require [dvergr.channels.telegram :as tg]
            [dvergr.channels.core :as channels]
            [dvergr.adapters.core :as adapters]
            [dvergr.security.allowlist :as allowlist]
            [dvergr.substrate.config :as dcfg]
            [is.simm.runtimes.instance :as instance]
            [dvergr.rooms :as drooms]
            [dvergr.room.registry :as rreg]
            [dvergr.room.store :as rstore]
            [dvergr.system.db :as sdb]
            [dvergr.discourse :as d]
            [is.simm.model.system-db :as system-db]
            [is.simm.model.rooms :as rooms]
            [is.simm.model.room-databases :as room-dbs]
            [is.simm.model.parties :as parties]
            [is.simm.model.blobs :as blobs]
            [is.simm.model.drives :as drives]
            [dvergr.audio.stt :as stt]
            [is.simm.agents.room-agents :as room-agents]
            [is.simm.runtimes.context :as ctx]
            [dvergr.system.rooms :as srooms]
            [org.replikativ.spindel.engine.core :as rtc]
            [datahike.api :as d-api]
            [clojure.string :as str]
            [taoensso.telemere :as log]))

;; =============================================================================
;; Identity: telegram user ↔ simmis party
;; =============================================================================

(defn party-by-telegram-id
  [tg-user-id]
  (when-let [conn (system-db/get-conn)]
    (d-api/q '[:find (pull ?e [:party/id :party/display-name :party/telegram-username]) .
               :in $ ?tid :where [?e :party/telegram-id ?tid]]
             @conn (long tg-user-id))))

(defn link-telegram!
  "Link a Telegram identity to an existing party. Absorbs any lightweight
   external party previously auto-created for this telegram id (retracts its
   telegram attrs so the unique id moves over). Returns the party id."
  [party-id tg-user-id username]
  (let [conn (system-db/get-conn)
        tid (long tg-user-id)]
    (when-let [existing (party-by-telegram-id tid)]
      (when (not= (:party/id existing) party-id)
        (d-api/transact conn [[:db/retract [:party/id (:party/id existing)]
                               :party/telegram-id tid]])))
    (d-api/transact conn [(cond-> {:party/id party-id :party/telegram-id tid}
                            username (assoc :party/telegram-username username))])
    (log/log! {:level :info :id ::telegram-linked
               :msg "Telegram identity linked to party"
               :data {:party-id party-id :telegram-id tid :username username}})
    party-id))

(defn- ensure-telegram-party!
  "Resolve a Telegram user-info map to a party id. Linked users map to their
   party; unknown users get a lightweight external party (carrying the actor
   core so dvergr participant routing works) and a prominent log line with
   the id + username for linking."
  [{:keys [id username first-name last-name] :as user-info}]
  (or (:party/id (party-by-telegram-id id))
      (let [conn (system-db/get-conn)
            pid (random-uuid)
            display (or (some->> [first-name last-name] (remove nil?) seq
                                 (str/join " "))
                        username
                        (str "telegram-" id))]
        (d-api/transact conn
          [(cond-> {:party/id pid
                    :party/type :human
                    :party/display-name display
                    :party/created (java.util.Date.)
                    :party/telegram-id (long id)
                    ;; actor core (A2 convention) so discourse routing works
                    :actor/id (keyword "party" (str pid))
                    :actor/kind :human
                    :actor/status :online
                    :actor/created-at (java.util.Date.)
                    :actor/name display}
             username (assoc :party/telegram-username username))])
        (log/log! {:level :warn :id ::unlinked-telegram-user
                   :msg (str "UNLINKED telegram user — link with "
                             "(is.simm.runtimes.telegram/link-telegram! <party-id> "
                             id " \"" username "\")")
                   :data {:telegram-id id :username username
                          :display-name display :auto-party pid}})
        pid)))

;; =============================================================================
;; Venue rooms (content-DB persistence = the shared room projector)
;; =============================================================================

(defn- telegram-chat-name
  "Resolve a human-readable name for a Telegram chat via getChat:
   group title, or DM partner's name/username. Nil on any failure
   (offline, unconfigured token) — callers fall back to the default."
  [chat-id]
  (try
    (when-let [token (dcfg/telegram-token)]
      (let [chat (tg/api-call token "getChat" {:chat_id chat-id})]
        (or (:title chat)
            (:first_name chat)
            (:username chat))))
    (catch Exception _ nil)))

(defn- maybe-name-room-from-chat!
  "If the room still carries the default 'Telegram <id>' name, rename it
   to the actual chat title (group name / DM partner). Also updates the
   content DB's chatroom entity so the client header follows."
  [room-row chat-id room-conn]
  (let [room-uuid (:room/id room-row)
        current (:room/name room-row)]
    (when (and room-uuid
               (or (nil? current) (= current (str "Telegram " chat-id))))
      (when-let [chat-name (telegram-chat-name chat-id)]
        (sdb/create-room! {:id room-uuid
                           :slug (:room/slug room-row)
                           :name chat-name
                           :type (:room/type room-row)})
        (when room-conn
          (d-api/transact room-conn
            [{:entity/uuid room-uuid
              :S.ChatRoom/name chat-name}]))
        (log/log! {:level :info :id ::room-named-from-chat
                   :data {:room room-uuid :name chat-name}})))))


(defn- file-into-room-drive!
  "Store bytes in the room drive under /telegram/ via dvergr's ONE
   channel-agnostic upload seam (integration/store-upload! — Telegram,
   web, mail, jibri all use it; the drive-conn resolver routes it onto
   simmis registry drives). Returns {:note :attachment} or nil."
  [chat-id bytes file-name mime source]
  (let [slug (str "tg-" chat-id)]
    (when-let [room ((requiring-resolve 'dvergr.room.registry/lookup)
                     ((requiring-resolve 'dvergr.room.store/slug->room-id) slug))]
      (let [name (or file-name
                     (str (clojure.core/name source) "-"
                          (.format (java.text.SimpleDateFormat. "yyyyMMdd-HHmmss")
                                   (java.util.Date.))
                          (cond (and mime (.contains ^String mime "ogg")) ".ogg"
                                (and mime (.contains ^String mime "jpeg")) ".jpg"
                                (and mime (.contains ^String mime "png")) ".png"
                                :else "")))]
        ((requiring-resolve 'dvergr.drive.integration/store-upload!)
         room "telegram" {:bytes bytes :file-name name :mime mime
                         :photo? (= source :photo) :source source})))))

(defn ensure-telegram-room!
  "Find-or-create the room for a Telegram chat-id. Deterministic slug
   `tg-<chat-id>`; provisions the full dvergr room (store + repo + ctx),
   the simmis content DB, and the content mirror. Adds `party-id` to the
   room roster (so it shows in that user's sidebar). Returns the live
   discourse Room."
  [chat-id party-id]
  (binding [rtc/*execution-context* ctx/server-context]
    (let [slug (str "tg-" chat-id)
          room-id (rstore/slug->room-id slug)
          live (or (rreg/lookup room-id)
                   (do (drooms/create-room! {:title (str "Telegram " chat-id)
                                             :slug slug
                                             :type :telegram-mirror
                                             :telegram-chat-id chat-id
                                             :parent-id false
                                             :ctx ctx/server-context})
                       (rreg/lookup room-id)))
          room-row (sdb/room-by-slug slug)
          room-uuid (:room/id room-row)]
      (when room-uuid
        ;; simmis extension: bind the room row to its store and make sure
        ;; simmis's schema is installed on it (dvergr provisioned the store
        ;; itself above, so the scope is looked up, never minted).
        (let [conn (system-db/get-conn)
              scope (or (:room/content-db-scope
                         (d-api/pull @conn [:room/content-db-scope] [:room/id room-uuid]))
                        (when-let [s (ctx/with-server-context
                                       (srooms/room-msgs-store-id room-uuid))]
                          (d-api/transact conn [{:room/id room-uuid
                                                 :room/content-db-scope s}])
                          (room-dbs/ensure-room-database! s)
                          s))
              room-conn (room-dbs/connect-room-database scope)]
          ;; The content DB needs the chatroom entity itself — message rows
          ;; ref it via :S.Message/room [:entity/uuid room-uuid].
          (when (and room-conn
                     (not (d-api/q '[:find ?e . :in $ ?uuid :where [?e :entity/uuid ?uuid]]
                                   @room-conn room-uuid)))
            (d-api/transact room-conn
              [{:entity/uuid room-uuid
                :entity/created-at (java.util.Date.)
                :entity/name (str "S.ChatRoom/" (:room/name room-row))
                :instance/of-role [:entity/uuid #uuid "00000000-0000-0000-0000-00000000002a"]
                :S.ChatRoom/name (or (:room/name room-row) slug)}]))
          (when (and party-id room-uuid)
            (rooms/add-party! room-uuid party-id))
          (maybe-name-room-from-chat! room-row chat-id room-conn)
          (when (and live room-conn)
            (room-agents/ensure-room-projector! live room-uuid room-conn)
            ;; Hydration does not rejoin agent participants — without this,
            ;; telegram-inbound messages after a JVM restart address an
            ;; empty roster and agents never reply (web dispatch does the
            ;; equivalent join on its own path).
            (doseq [agent (filter :party/auto-respond?
                                  (rooms/get-room-agents room-uuid))]
              (room-agents/ensure-room-party-entity! room-conn agent)
              (room-agents/ensure-agent-joined! live room-uuid agent room-conn)))))
      live)))

;; =============================================================================
;; Channel lifecycle
;; =============================================================================

(defonce ^:private tg-state (atom nil))

;; The adapter must survive stop!/start! cycles: its :mirrored/:injected
;; state is the only guard against re-registering a room's telegram mirror
;; (dvergr's mirror-room! has no unsubscribe — a fresh adapter after a
;; restart re-registers and every message relays twice). One adapter per
;; JVM; a stale listener from a previous adapter requires a JVM restart
;; (dvergr follow-up: track + remove mirror subs on disconnect!).
(defonce ^:private adapter-cache (atom nil))

(defn start-telegram!
  "Connect the Telegram bot when a token is configured; no-op otherwise.
   Idempotent.

   Never on an OFFSET instance (SIMMIS_PORT_OFFSET > 0). The token is usually
   the machine's — TELEGRAM_BOT_TOKEN in the shell, or the checkout's
   config.local.edn — so a second instance would find the same one, and
   Telegram serves a bot's updates to ONE poller: two of them answer each
   other with 409 Conflict and the first instance's chats stop arriving."
  []
  (cond
    @tg-state
    :already-running

    (pos? (instance/port-offset))
    (do (when (dcfg/telegram-token)
          (log/log! {:level :warn :id ::telegram-skipped-offset-instance
                     :msg (str "Telegram not started: this is an offset instance "
                               "(SIMMIS_PORT_OFFSET), and a bot token can have only one poller.")}))
        :offset-instance)

    :else
    (if-let [token (dcfg/telegram-token)]
      (let [tg-cfg (:telegram (dcfg/config))
            _ (when-let [users (:allowed-users tg-cfg)]
                (allowlist/set-users! users))
            _ (when (empty? (allowlist/list-users))
                (log/log! {:level :warn :id ::empty-allowlist
                           :msg (str "Telegram :allowed-users is empty: every sender is refused. "
                                     "List numeric ids or \"@handle\"s in config.local.edn.")}))
            send-fn (fn [chat-id text]
                      (tg/send-long-message! token chat-id text))
            caps {:ctx ctx/server-context
                  :send-fn send-fn
                  ;; Voice notes: the channel downloads the file (needs
                  ;; :token) and calls :transcribe-fn; the transcript then
                  ;; flows as a normal 🎤-prefixed text message — agents and
                  ;; mirrors see it like typed input.
                  ;; Original audio is kept content-addressed for later
                  ;; playback UI / re-processing.
                  :token token
                  :transcribe-fn (fn [{:keys [bytes mime chat-id] :as voice}]
                                   ;; one write path: the drive copy IS the CAS
                                   ;; store (same sha) — its blob-id serves playback
                                   (let [stored (when chat-id
                                                  (try (file-into-room-drive!
                                                        chat-id bytes nil (or mime "audio/ogg") :voice)
                                                       (catch Exception _ nil)))
                                         blob-id (or (get-in stored [:attachment :blob-id])
                                                     (:blob/id (blobs/store! bytes (or mime "audio/ogg"))))]
                                     (log/log! {:level :info :id ::voice-note
                                                :data {:blob blob-id :size (count bytes)
                                                       :duration (:duration voice)}})
                                     (when-let [t (stt/transcribe {:bytes bytes :mime mime})]
                                       {:text t
                                        :attachment {:blob-id blob-id
                                                     :mime (or mime "audio/ogg")}})))
                  ;; Documents/photos land in the room drive under
                  ;; /telegram/ — browsable in the Files panel, readable
                  ;; by agents at /drive/telegram/ in their shell.
                  :store-file-fn (fn [{:keys [bytes file-name mime chat-id photo?]}]
                                   (log/log! {:level :info :id ::telegram-file
                                              :data {:name file-name :size (count bytes)
                                                     :mime mime :photo? photo?}})
                                   (file-into-room-drive!
                                    chat-id bytes file-name
                                    (or mime (when photo? "image/jpeg"))
                                    (if photo? :photo :telegram)))
                  ;; no routed agent for now: messages land in the room
                  ;; (mirror-only bootstrap); agents join later explicitly.
                  :default-agent nil
                  :ensure-room (fn [chat-id _default-agent]
                                 ;; party added per-message in ensure-actor;
                                 ;; nil here (room creation must not depend on
                                 ;; a mapped sender).
                                 (ensure-telegram-room! chat-id nil))
                  :ensure-actor (fn [user-info]
                                  (let [pid (ensure-telegram-party! user-info)]
                                    (room-agents/party->actor-kw pid)))
                  :speaker-name (fn [actor-kw]
                                  (or (some-> actor-kw
                                              room-agents/actor-kw->party-uuid
                                              parties/get-party
                                              :party/display-name)
                                      (some-> actor-kw name)))}
            adapter (or @adapter-cache
                        (let [a (tg/make-daemon-adapter caps)]
                          (reset! adapter-cache a)
                          a))
            channel (tg/make-telegram {:token token :poll? true})
            ;; Defense-in-depth against re-delivered updates: telegram's
            ;; getUpdates offset is GLOBAL per bot token, so a second poller
            ;; (e.g. a dvergr daemon on the same token) thrashes the cursor
            ;; and causes sporadic re-deliveries. Only one poller should run;
            ;; this dedup keeps re-deliveries from double-posting regardless.
            seen-updates (java.util.Collections/newSetFromMap
                          (java.util.concurrent.ConcurrentHashMap.))
            connected (channels/connect!
                       channel
                       :on-message
                       (fn [msg]
                         (try
                           (let [k [(:chat-id msg) (:message-id msg)]]
                             (if (and (:message-id msg) (not (.add seen-updates k)))
                               (log/log! {:level :warn :id ::duplicate-update-dropped
                                          :data {:key k}})
                               (do
                                 ;; roster upkeep: a linked sender joins the room
                                 ;; row so it appears in their simmis sidebar.
                                 (when-let [{:keys [chat-id from]} msg]
                                   (when-let [party (party-by-telegram-id (:id from))]
                                     (when-let [room-row (sdb/room-by-slug (str "tg-" chat-id))]
                                       (rooms/add-party! (:room/id room-row) (:party/id party)))))
                                 (tg/handle-inbound! adapter msg caps))))
                           (catch Exception e
                             (log/log! {:level :error :id ::inbound-error
                                        :data {:error (.getMessage e)}})))))]
        (reset! tg-state {:channel connected :adapter adapter})
        ;; Re-arm known telegram rooms proactively: after a JVM restart the
        ;; outbound relay (adapters/mirror-room!) would otherwise only
        ;; re-register on the first INBOUND telegram message, so a web-send
        ;; before that reaches the store but never telegram.
        (try
          (doseq [{:room/keys [telegram-chat-id]}
                  (d-api/q '[:find [(pull ?r [:room/telegram-chat-id]) ...]
                             :where [?r :room/type :telegram-mirror]
                             [?r :room/telegram-chat-id _]]
                           @(system-db/get-conn))]
            (when telegram-chat-id
              (when-let [live (ensure-telegram-room! telegram-chat-id nil)]
                (adapters/mirror-room! adapter telegram-chat-id live))))
          (catch Exception e
            (log/log! {:level :warn :id ::rearm-failed
                       :data {:error (.getMessage e)}})))
        (log/log! {:level :info :id ::telegram-started
                   :msg "Telegram bot connected (mirror-only; no routed agent)"})
        :started)
      (do (log/log! {:level :info :id ::telegram-disabled
                     :msg "No telegram token configured — telegram channel disabled"})
          :no-token))))

(defn stop-telegram!
  []
  (when-let [{:keys [channel]} @tg-state]
    (try (channels/disconnect! channel)
         (catch Exception e
           (log/log! {:level :warn :id ::disconnect-failed
                      :data {:error (.getMessage e)}})))
    (reset! tg-state nil)
    :stopped))
