(ns is.simm.uis.web.desktop.user-rooms-sync
  "Client-side: keep `sig/user-rooms` fresh.

   Two responsibilities:
   1. `refresh-user-rooms!` — call `load-rooms!`, reset the signal, and replace
      the personal-ai placeholder tab on first load. KB connections stay lazy:
      the room snapshot supplies navigation data and an opened wiki page connects
      its own store. Idempotent — safe to call repeatedly.
   2. `subscribe!` — subscribe once to the server's `:user-rooms/dirty` pubsub
      topic. When the server publishes a set of party-ids and the current
      user's id is in the set, refresh."
  (:require [clojure.core.async :refer [go <!] :include-macros true]
            [kabel.pubsub :as pubsub]
            [kabel.pubsub.protocol :as proto]
            [org.replikativ.spindel.engine.core :as rtc]
            [is.simm.uis.web.desktop.runtime :refer [runtime]]
            [is.simm.uis.web.desktop.signals :as sig]
            [is.simm.uis.web.desktop.chat-remote :as cr]
            [is.simm.runtimes.web :as web]))

(def ^:private dirty-topic :user-rooms/dirty)

(defn refresh-user-rooms!
  "Fetch load-rooms! for `party-id`, reset the signal, and (on first load)
   swap the personal-ai placeholder tab for the real room. KB stores connect
   only when a wiki view needs one, so chat startup is not queued behind every
   wiki handshake. Safe to call multiple times."
  [party-id]
  (let [s (cr/load-rooms! web/server-id party-id)]
    (s (fn [result]
         (binding [rtc/*execution-context* runtime]
           (reset! sig/user-rooms result)
           ;; Route restoration knows the room id, but deliberately does not
           ;; persist server-owned metadata such as its content DB scope. Fill
           ;; that metadata from this fresh snapshot for every chat tab (and
           ;; resolve the personal-AI placeholder at the same time). Without
           ;; this, a direct /room/:id load renders a permanent spinner because
           ;; render-tab-content has no scope with which to connect-room!.
           (let [rooms (:rooms result)
                 rooms-by-id (into {} (map (juxt (comp str :room/id) identity)) rooms)
                 personal-room (first (filter #(= (:room/type %) :personal-ai) rooms))]
             (swap! sig/layout-columns
                    (fn [cols]
                      (mapv (fn [col]
                              (update col :tabs
                                      (fn [tabs]
                                        (mapv (fn [t]
                                                (let [room-id (get-in t [:data :room-id])
                                                      room (if (= room-id "personal-ai-placeholder")
                                                             personal-room
                                                             (get rooms-by-id room-id))]
                                                  (if (and (= :chat (:type t)) room)
                                                    (-> t
                                                        (assoc :title (:room/name room))
                                                        (assoc-in [:data :room-id] (str (:room/id room)))
                                                        (assoc-in [:data :room-name] (:room/name room))
                                                        (assoc-in [:data :db-scope]
                                                                  (str (:room/content-db-scope room))))
                                                    t)))
                                              tabs))))
                            cols))))))
       (fn [err] (js/console.error "[user-rooms-sync] load-rooms error:" err)))))

(defonce ^:private subscribed? (atom false))

(defn subscribe!
  "Subscribe to the user-rooms invalidation topic. The server publishes
   `{:party-ids #{...}}` after any system-DB tx that affects a roster.
   If our party-id is in the set, refresh. Idempotent — only subscribes
   once per page load."
  [party-id-str]
  (when (and party-id-str (not @subscribed?))
    (reset! subscribed? true)
    (let [my-id (uuid party-id-str)
          strategy (proto/pub-sub-only-strategy
                     (fn [{:keys [party-ids]}]
                       (when (contains? party-ids my-id)
                         (refresh-user-rooms! party-id-str))))]
      (go
        (let [result (<! (pubsub/subscribe! @web/client #{dirty-topic}
                                            {:strategies {dirty-topic strategy}}))]
          (if (:error result)
            (do (reset! subscribed? false)
                (js/console.error "[user-rooms-sync] subscribe failed:" (pr-str result)))
            (js/console.log "[user-rooms-sync] subscribed to user-rooms invalidations")))))))
