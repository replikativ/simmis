(ns is.simm.uis.web.desktop.user-rooms-sync
  "Client-side: keep `sig/user-rooms` fresh.

   Two responsibilities:
   1. `refresh-user-rooms!` — call `load-rooms!`, reset the signal, and reconcile
      every open chat tab against the fresh roster. KB connections stay lazy:
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
            [is.simm.uis.web.desktop.tab-heal :as tab-heal]
            [is.simm.uis.web.desktop.chat-remote :as cr]
            [is.simm.runtimes.web :as web]))

(def ^:private dirty-topic :user-rooms/dirty)

(defn refresh-user-rooms!
  "Fetch load-rooms! for `party-id`, reset the signal, and reconcile every open
   chat tab against the fresh roster. KB stores connect only when a wiki view
   needs one, so chat startup is not queued behind every wiki handshake. Safe to
   call multiple times."
  [party-id]
  (let [s (cr/load-rooms! web/server-id party-id)]
    (s (fn [result]
         (binding [rtc/*execution-context* runtime]
           (reset! sig/user-rooms result)
           ;; Route restoration knows the room id, but deliberately does not
           ;; persist server-owned metadata such as its content DB scope. The
           ;; roster is where that metadata arrives, so the repair belongs here.
           ;; It subsumes the old placeholder-only swap: the boot layout's
           ;; "personal-ai-placeholder" tab is simply the tab whose room is the
           ;; personal-ai room.
           ;;
           ;; This is HALF the invariant — the half where the tab was already
           ;; open. `sig/open-tab!` holds the other half, for a tab opened
           ;; after the roster landed; both call into `tab-heal`, so a tab
           ;; ends up resolved-or-missing regardless of which came first.
           ;;
           ;; `reconcile-layout` returns the layout unchanged when it changed
           ;; nothing, so the refresh this subscription runs on every roster
           ;; invalidation does not repaint every column for no new fact.
           (swap! sig/layout-columns tab-heal/reconcile-layout result)))
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
