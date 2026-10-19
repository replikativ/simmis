(ns is.simm.uis.web.desktop.message-notify-sync
  "Client-side: subscribe to my OWN private `:notify/<party-id>` topic and raise a
   browser Notification when I'm @mentioned in a room I'm not actively viewing.
   The server fans out per-recipient (message-notify-broadcast), so this stream
   only ever carries my notifications. Mention slice of
   doc/archive/mentions-notifications-contacts-design.md — unread badges come later."
  (:require [org.replikativ.spindel.engine.core :as rtc]
            [is.simm.uis.web.desktop.runtime :refer [runtime]]
            [is.simm.uis.web.desktop.signals :as sig]
            [is.simm.uis.web.desktop.chat-remote :as cr]
            [is.simm.uis.web.desktop.remote :as rem]
            [is.simm.uis.web.desktop.aggregate :as aggregate]
            [is.simm.uis.web.desktop.connection-epoch :as epoch]
            [is.simm.uis.web.desktop.topic-subscription :as subscription]
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

(declare mark-read!)

(defn- handle-message-event!
  "A message landed in a room I belong to. Badge it (awareness) unless I'm
   already viewing that room; pop it (interruption) only when the server says so
   for my per-room level (`:popup?`) and I can't already see it."
  [{:keys [room-id popup?] :as payload}]
  (let [viewing? (viewing-room? room-id)]
    (if viewing?
      (mark-read! (str room-id))
      (do (sig/bump-room-unread! (str room-id))
          (when popup? (show-popup! payload))))
    viewing?))

(def ^:private owner (subscription/owner))
(def ^:private unread-gate (aggregate/gate))
(def ^:private prefs-gate (aggregate/gate))
(def ^:private pref-requests (atom {}))

(defn load-unread!
  "Load durable unread truth. A live event during this snapshot queues another
   pass, so snapshot overwrite cannot permanently lose or double-count it."
  []
  (aggregate/run!
   unread-gate
   (fn [done current?]
     (binding [rtc/*execution-context* runtime]
       (let [s (cr/load-unread-counts! web/server-id)]
         (s (fn [counts]
              (try (when (current?)
                     (binding [rtc/*execution-context* runtime]
                       (let [active (active-chat-room-id)]
                         (reset! sig/unread-counts
                                 (cond-> (or counts {})
                                   (and active (viewing-room? active)) (dissoc (str active)))))))
                   (finally (done))))
            (fn [err]
              (try (when (current?) (js/console.error "[notify-sync] unread load failed:" err))
                   (finally (done))))))))))

(defn mark-read!
  "Clear a room's badge locally AND advance the durable server cursor, so the
   read state survives a reload. Called when the user opens a room."
  [room-id-str]
  (let [connection (epoch/snapshot)]
   (sig/mark-room-read! room-id-str)
   (binding [rtc/*execution-context* runtime]
    (let [s (cr/mark-read! web/server-id (str room-id-str))]
      ;; The badge stays cleared — the person did read the room — but a
      ;; cursor that did not advance brings it back on reload, so say so.
      (s (fn [result]
           (when (epoch/current? connection)
             ((rem/on-ok "Could not mark the room as read." (fn [_] (load-unread!))) result)))
         (fn [error]
           (when (epoch/current? connection)
             ((rem/on-error "Could not mark the room as read.") error))))))))

(defn load-notify-prefs!
  "Load per-room notification levels, fenced and coalesced by socket epoch."
  []
  (aggregate/run!
   prefs-gate
   (fn [done current?]
     (binding [rtc/*execution-context* runtime]
       (let [s (cr/load-notify-prefs! web/server-id)]
         (s (fn [prefs]
              (try (when (current?)
                     (binding [rtc/*execution-context* runtime]
                       (reset! sig/notify-prefs (into {} (map (fn [[k v]] [(name k) v]) (or prefs {}))))))
                   (finally (done))))
            (fn [err]
              (try (when (current?) (js/console.error "[notify-sync] preferences load failed:" err))
                   (finally (done))))))))))

(defn set-notify-pref!
  "Set a room's notification level (:all/:mentions/:none) locally + on the server."
  [room-id-str level]
  (binding [rtc/*execution-context* runtime]
    (let [connection (epoch/snapshot) request-id (random-uuid)
          current? #(and (epoch/current? connection) (= request-id (get @pref-requests room-id-str)))
          prev (get @sig/notify-prefs (str room-id-str))
          ;; shown at once; put back when the server does not take it
          undo! (fn [err]
                  (when (current?)
                    (binding [rtc/*execution-context* runtime]
                      (swap! sig/notify-prefs
                             #(if prev (assoc % (str room-id-str) prev) (dissoc % (str room-id-str)))))
                    ((rem/on-error "Could not change the notification level.") err)))]
      (swap! pref-requests assoc room-id-str request-id)
      (swap! sig/notify-prefs assoc (str room-id-str) level)
      (let [s (cr/set-notify-pref! web/server-id (str room-id-str) level)]
        (s (fn [r]
             (if-let [e (rem/result-failure r)] (undo! e)
                     (when (current?) (load-notify-prefs!))))
           undo!)))))

(defn subscribe!
  "Own my private stream for this authenticated socket. Catch up durable badges
   and preferences after handshake, without replaying old notification popups."
  [party-id-str]
  (let [connection (epoch/snapshot)]
    (when (= (str party-id-str) (str (:party connection)))
      (subscription/ensure!
       owner {:connection connection :peer (:client (web/current-session))
              :topic (notify-topic party-id-str)
              :on-ready #(do (load-unread!) (load-notify-prefs!))
              :on-message (fn [payload]
                            (when (= :message (:kind payload))
                              (when-not (handle-message-event! payload)
                                (load-unread!))))}))))
