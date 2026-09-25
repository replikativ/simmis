(ns is.simm.uis.web.desktop.run-actions
  "What a person does to a Run from the UI: cancel it, promote its world to a
   proposal. The Runs themselves are read from the room replica
   (`run-detail/query-room-runs`); the one process-local fact, whether this
   server still holds a reviewed world's capability, is asked here."
  (:require [org.replikativ.spindel.engine.core :as rtc]
            [is.simm.uis.web.desktop.chat-remote :as remote]
            [is.simm.uis.web.desktop.runtime :refer [runtime]]
            [is.simm.uis.web.desktop.signals :as sig]
            [is.simm.runtimes.web :as web]))

(defonce ^:private asked (atom #{}))

(defn ensure-world-live!
  "Ask once whether the server still holds Run `run-id`'s world; the answer
   lands in `sig/run-world-live`."
  [room-id run-id]
  (let [run-id (str run-id)]
    (when-not (contains? @asked run-id)
      (swap! asked conj run-id)
      (binding [rtc/*execution-context* runtime]
        (let [s (remote/run-world-live! web/server-id (str room-id) run-id)]
          (s (fn [{:keys [live?]}]
               (binding [rtc/*execution-context* runtime]
                 (swap! sig/run-world-live assoc run-id (boolean live?))))
             (fn [error]
               (swap! asked disj run-id)
               (js/console.warn "[run-actions] world capability query failed" run-id error))))))))

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
           (swap! asked disj (str run-id))
           (when on-complete (on-complete result)))
         (fn [error]
           (js/console.warn "[run-actions] promotion failed" run-id error))))))
