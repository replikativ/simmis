(ns is.simm.uis.web.desktop.message-notify-sync
  "Client-side: subscribe to my OWN private `:notify/<party-id>` topic and raise a
   browser Notification when I'm @mentioned in a room I'm not actively viewing.
   The server fans out per-recipient (message-notify-broadcast), so this stream
   only ever carries my notifications. Mention slice of
   doc/archive/mentions-notifications-contacts-design.md — unread badges come later."
  (:require [clojure.core.async :refer [go <!] :include-macros true]
            [kabel.pubsub :as pubsub]
            [kabel.pubsub.protocol :as proto]
            [org.replikativ.spindel.engine.core :as rtc]
            [is.simm.uis.web.desktop.runtime :refer [runtime]]
            [is.simm.uis.web.desktop.signals :as sig]
            [is.simm.uis.web.desktop.chat-remote :as cr]
            [is.simm.uis.web.desktop.remote :as rem]
            [is.simm.runtimes.web :as web]))

(defn notify-topic
  "Per-user notification topic keyword — must match the server's
   message-notify-broadcast/notify-topic."
  [party-id-str]
  (keyword "notify" party-id-str))

(defn supported? [] (exists? js/Notification))

(defn permission []
  (when (supported?) (.-permission js/Notification)))

(defn enabled? [] (= "granted" (permission)))

(defn request-permission!
  "Prompt for Notification permission. Returns a JS promise (or nil if
   unsupported). Call from a user gesture (the Settings toggle)."
  []
  (when (supported?) (.requestPermission js/Notification)))

(defn- active-chat-room-id
  "The room-id of the currently active chat tab, or nil."
  []
  (binding [rtc/*execution-context* runtime]
    (some (fn [col]
            (when-let [active (first (filter #(= (:id %) (:active-tab col)) (:tabs col)))]
              (when (= (:type active) :chat)
                (get-in active [:data :room-id]))))
          @sig/layout-columns)))

(defn- viewing-room?
  "True when `room-id` is the active chat tab AND the tab is focused — i.e. the
   user is already looking at this room, so neither badge nor popup is warranted."
  [room-id]
  (and (= (str room-id) (str (active-chat-room-id)))
       (not (.-hidden js/document))))

(defn- show-popup!
  [{:keys [room-id room-name author-name preview mentioned?]}]
  (when (enabled?)
    (let [title (if mentioned?
                  (str author-name " mentioned you"
                       (when (seq (str room-name)) (str " in " room-name)))
                  (str "New message" (when (seq (str room-name)) (str " in " room-name))))
          n (js/Notification. title
                              #js {:body (or preview "")
                                   :tag (str "room-" room-id)})]
      (set! (.-onclick n)
            (fn [_] (.focus js/window) (.close n))))))

(defn- handle-message-event!
  "A message landed in a room I belong to. Badge it (awareness) unless I'm
   already viewing that room; pop it (interruption) only when the server says so
   for my per-room level (`:popup?`) and I can't already see it."
  [{:keys [room-id popup?] :as payload}]
  (let [viewing? (viewing-room? room-id)]
    (when-not viewing?
      (sig/bump-room-unread! (str room-id))
      (when popup?
        (show-popup! payload)))))

(defonce ^:private unread-loaded? (atom false))

(defn load-unread!
  "Seed per-room unread badges from the server's durable read cursors on login.
   Idempotent per page load. Resets sig/unread-counts to the server truth (so a
   reload restores the same badges)."
  []
  (when-not @unread-loaded?
    (reset! unread-loaded? true)
    (binding [rtc/*execution-context* runtime]
      (let [s (cr/load-unread-counts! web/server-id)]
        (s (fn [counts]
             (binding [rtc/*execution-context* runtime]
               (reset! sig/unread-counts (or counts {}))))
           (fn [err]
             (reset! unread-loaded? false)
             (js/console.error "[notify-sync] load-unread error:" err)))))))

(defn mark-read!
  "Clear a room's badge locally AND advance the durable server cursor, so the
   read state survives a reload. Called when the user opens a room."
  [room-id-str]
  (sig/mark-room-read! room-id-str)
  (binding [rtc/*execution-context* runtime]
    (let [s (cr/mark-read! web/server-id (str room-id-str))]
      ;; The badge stays cleared — the person did read the room — but a
      ;; cursor that did not advance brings it back on reload, so say so.
      (s (rem/on-ok "Could not mark the room as read." nil)
         (rem/on-error "Could not mark the room as read.")))))

(defonce ^:private prefs-loaded? (atom false))

(defn load-notify-prefs!
  "Seed per-room notification levels from the server on login. Idempotent."
  []
  (when-not @prefs-loaded?
    (reset! prefs-loaded? true)
    (binding [rtc/*execution-context* runtime]
      (let [s (cr/load-notify-prefs! web/server-id)]
        (s (fn [prefs]
             (binding [rtc/*execution-context* runtime]
               ;; keys serialize as keywords over the wire — normalise to strings
               (reset! sig/notify-prefs (into {} (map (fn [[k v]] [(name k) v]) (or prefs {}))))))
           (fn [err]
             (reset! prefs-loaded? false)
             (js/console.error "[notify-sync] load-notify-prefs error:" err)))))))

(defn room-notify-level
  "The notification level for `room-id-str` — :mentions when unset."
  [room-id-str]
  (binding [rtc/*execution-context* runtime]
    (get @sig/notify-prefs (str room-id-str) :mentions)))

(defn set-notify-pref!
  "Set a room's notification level (:all/:mentions/:none) locally + on the server."
  [room-id-str level]
  (binding [rtc/*execution-context* runtime]
    (let [prev (get @sig/notify-prefs (str room-id-str))
          ;; shown at once; put back when the server does not take it
          undo! (fn [err]
                  (binding [rtc/*execution-context* runtime]
                    (swap! sig/notify-prefs
                           #(if prev (assoc % (str room-id-str) prev) (dissoc % (str room-id-str)))))
                  ((rem/on-error "Could not change the notification level.") err))]
      (swap! sig/notify-prefs assoc (str room-id-str) level)
      (let [s (cr/set-notify-pref! web/server-id (str room-id-str) level)]
        (s (fn [r] (when-let [e (rem/result-failure r)] (undo! e)))
           undo!)))))

(defonce ^:private subscribed? (atom false))

(defn subscribe!
  "Subscribe once to my private notify topic. `party-id-str` is my party uuid.
   Idempotent per page load. Auth-gated server-side to my own stream."
  [party-id-str]
  (when (and party-id-str (not @subscribed?))
    (reset! subscribed? true)
    (let [topic    (notify-topic party-id-str)
          strategy (proto/pub-sub-only-strategy
                     (fn [payload]
                       (when (= :message (:kind payload))
                         (handle-message-event! payload))))]
      (go
        (let [result (<! (pubsub/subscribe! @web/client #{topic}
                                            {:strategies {topic strategy}}))]
          (if (:error result)
            (do (reset! subscribed? false)
                (js/console.error "[notify-sync] subscribe failed:" (pr-str result)))
            (js/console.log "[notify-sync] subscribed to my notification stream")))))))
