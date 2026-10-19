(ns is.simm.uis.web.desktop.user-rooms-sync
  "Epoch-owned roster invalidation, followed by an authoritative snapshot."
  (:require [org.replikativ.spindel.engine.core :as ec]
            [is.simm.uis.web.desktop.runtime :refer [runtime]]
            [is.simm.uis.web.desktop.signals :as sig]
            [is.simm.uis.web.desktop.tab-heal :as tab-heal]
            [is.simm.uis.web.desktop.chat-remote :as cr]
            [is.simm.uis.web.desktop.aggregate :as aggregate]
            [is.simm.uis.web.desktop.connection-epoch :as epoch]
            [is.simm.uis.web.desktop.topic-subscription :as subscription]
            [is.simm.runtimes.web :as web])
  (:require-macros [org.replikativ.spindel.signal :refer [signal]]))

(def ^:private owner (subscription/owner))
(def ^:private gate (aggregate/gate))
(def ^:private reconciler (signal runtime nil))

(defn refresh-user-rooms!
  "Reconcile tabs against fresh server truth. A nudge during this request causes
   another pass; only its current epoch may publish or reconcile replicas."
  ([party-id]
   (let [{:keys [connection on-roster]} @reconciler]
     (refresh-user-rooms! party-id (when (epoch/current? connection) on-roster))))
  ([party-id on-roster]
   (aggregate/run!
    gate
    (fn [done current?]
      (binding [ec/*execution-context* runtime]
        (let [s (cr/load-rooms! web/server-id party-id)]
          (s (fn [result]
               (try
                 (when (current?)
                   (binding [ec/*execution-context* runtime]
                     ;; Authorize/retire scopes before publishing the roster
                     ;; that can synchronously render and lazily open them.
                     (when on-roster (on-roster result))
                     (when (current?)
                       (reset! sig/user-rooms result))
                     (when (current?)
                       (swap! sig/layout-columns tab-heal/reconcile-layout result))))
                 (finally (done))))
             (fn [error]
               (try (when (current?) (js/console.error "[user-rooms-sync] load failed:" error))
                    (finally (done)))))))))))

(defn subscribe!
  "Subscribe before snapshot so disconnected changes and invalidations arriving
   during the snapshot cannot be lost. One owner per authenticated epoch."
  ([party-id] (subscribe! party-id nil))
  ([party-id on-roster]
   (let [connection (epoch/snapshot)]
     (when (= (str party-id) (str (:party connection)))
       (when on-roster (reset! reconciler {:connection connection :on-roster on-roster}))
       (subscription/ensure!
        owner {:connection connection :peer (:client (web/current-session))
               :topic :user-rooms/dirty
               :on-ready #(refresh-user-rooms! party-id)
               :on-message (fn [{:keys [party-ids]}]
                             (when (contains? party-ids (uuid (str party-id)))
                               (refresh-user-rooms! party-id)))})))))
