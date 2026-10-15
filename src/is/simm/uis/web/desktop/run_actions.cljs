(ns is.simm.uis.web.desktop.run-actions
  "What a person does to a Run from the UI: cancel it, promote its world to a
   proposal. The Runs themselves are read from the room replica
   (`run-detail/query-room-runs`); what only the server can say — whether it
   still holds a reviewed world's capability, whether this viewer may stop a
   Run — is asked here."
  (:require [org.replikativ.spindel.engine.core :as rtc]
            [is.simm.uis.web.desktop.chat-remote :as remote]
            [is.simm.uis.web.desktop.runtime :refer [runtime]]
            [is.simm.uis.web.desktop.signals :as sig]
            [is.simm.runtimes.web :as web]))

(defonce ^:private asked
  ;; [run-id fact] pairs already asked (or being asked) of the server
  (atom #{}))

(defn- ask-once!
  "Ask the server `fact` of Run `run-id` once, through `remote-call` (a fn of
   room-id and run-id strings returning the spin); `(answer result)` is the
   fact's value, kept in `sig/run-server-facts`."
  [fact remote-call answer room-id run-id]
  (let [run-id (str run-id)
        k [run-id fact]]
    (when-not (contains? @asked k)
      (swap! asked conj k)
      (binding [rtc/*execution-context* runtime]
        (let [s (remote-call (str room-id) run-id)]
          (s (fn [result]
               (binding [rtc/*execution-context* runtime]
                 (swap! sig/run-server-facts assoc-in [run-id fact] (boolean (answer result)))))
             (fn [error]
               (swap! asked disj k)
               (js/console.warn "[run-actions] run query failed" (name fact) run-id error))))))))

(defn ensure-world-live!
  "Ask once whether the server still holds Run `run-id`'s world; the answer
   lands in `sig/run-server-facts` as `:world-live?`."
  [room-id run-id]
  (ask-once! :world-live? #(remote/run-world-live! web/server-id %1 %2) :live? room-id run-id))

(defn ensure-can-cancel!
  "Ask once whether the viewer may stop the live Run `run-id`; the answer lands
   in `sig/run-server-facts` as `:can-cancel?`, and the stop controls show only
   when it is true."
  [room-id run-id]
  (ask-once! :can-cancel? #(remote/can-cancel-room-run! web/server-id %1 %2) :can-cancel?
             room-id run-id))

(defn cancel!
  "Request cooperative cancellation of one active Run in its room. The Run's
   cancelled status reaches the view through the replica."
  [room-id run-id]
  (binding [rtc/*execution-context* runtime]
    (let [s (remote/cancel-room-run! web/server-id (str room-id) (str run-id))]
      (s (fn [_])
         (fn [error]
           (js/console.warn "[run-actions] cancellation failed" run-id error))))))

(defn promote!
  "Promote one retained Run world and pass its durable Proposal result onward."
  [room-id run-id title on-complete]
  (binding [rtc/*execution-context* runtime]
    (let [s (remote/promote-room-run-world! web/server-id (str room-id)
                                            (str run-id) (str title))]
      (s (fn [result]
           (swap! asked disj [(str run-id) :world-live?])
           (when on-complete (on-complete result)))
         (fn [error]
           (js/console.warn "[run-actions] promotion failed" run-id error))))))
