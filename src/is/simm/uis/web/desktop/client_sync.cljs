(ns is.simm.uis.web.desktop.client-sync
  "Reconnect coordination above UI signals and Datahike replicas. Authentication
   starts an epoch; roster authorization and each replica's fresh head are
   distinct readiness boundaries. No timer or transaction replay."
  (:require [org.replikativ.spindel.engine.core :as ec]
            [is.simm.uis.web.desktop.runtime :refer [runtime]]
            [is.simm.uis.web.desktop.connection-epoch :as epoch]
            [is.simm.uis.web.desktop.db-signal :as db]
            [is.simm.uis.web.desktop.user-rooms-sync :as roster]
            [is.simm.uis.web.desktop.message-notify-sync :as notify]
            [is.simm.uis.web.desktop.branching-sync :as branches]
            [is.simm.uis.web.desktop.perspectives-sync :as perspectives]
            [is.simm.uis.web.desktop.views.leaderboard :as leaderboard]
            [is.simm.runtimes.web :as web])
  (:require-macros [org.replikativ.spindel.signal :refer [signal]]))

(def state (signal runtime nil))

(defn- catch-up! []
  (let [{:keys [connection roster branch-ready?]} @state]
    (when (and (epoch/current? connection) roster branch-ready?)
      (doseq [kb (:knowledge-bases roster) :when (epoch/current? connection)]
        (branches/refresh-kb-branches! (:kb/db-scope kb)))
      (when (epoch/current? connection) (perspectives/catch-up!))
      (when (epoch/current? connection) (leaderboard/refresh-loaded!))
      (when (epoch/current? connection) (swap! state assoc :status :streams-ready)))))

(defn ensure!
  "Called by the shell tracking the authenticated epoch. Native resources may
   only be installed in the canonical world, never in speculative UI forks."
  [connection user]
  (when (and (identical? (ec/home-context runtime) (ec/home-context ec/*execution-context*))
             (epoch/current? connection) (= (str (:id user)) (str (:party connection))))
    (when (not= connection (:connection @state))
      (reset! state {:connection connection :status :pending})
      (perspectives/install!)
      (branches/add-listener!
       ::reconnect
       (fn [{:keys [type]}]
         (when (and (= :transport/ready type) (epoch/current? connection))
           (swap! state assoc :branch-ready? true)
           (catch-up!)))))
    (roster/subscribe!
     (:id user)
     (fn [fresh]
       (when (epoch/current? connection)
         (swap! state assoc :roster fresh)
         (db/resync-open! connection fresh (:client (web/current-session)))
         (catch-up!))))
    (notify/subscribe! (:id user))
    (branches/subscribe!)))
