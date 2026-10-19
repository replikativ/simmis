(ns is.simm.uis.web.desktop.memory-sync
  "One private hint subscription per authenticated socket. Hints never contain
   files; owning views reconcile authorized metadata after readiness/reconnect."
  (:require [clojure.core.async :refer [go <!] :include-macros true]
            [kabel.pubsub :as pubsub]
            [kabel.pubsub.protocol :as proto]
            [org.replikativ.spindel.engine.core :as ec]
            [is.simm.uis.web.desktop.runtime :refer [runtime]]
            [is.simm.uis.web.desktop.signals :as sig]
            [is.simm.uis.web.desktop.remote :as remote]
            [is.simm.runtimes.web :as web])
  (:require-macros [org.replikativ.spindel.signal :refer [signal]]))

(def state (signal runtime nil))

(defn ensure!
  "Call only after prepare-memory-stream! registered the party's private topic.
   Readiness means the handshake callback, not merely subscribe request sent."
  [connection]
  (let [ctx (or ec/*execution-context* runtime) session (web/current-session)
        peer (:client session)
        current? #(and (web/current-session? session)
                       (= connection (binding [ec/*execution-context* ctx] @sig/memory-connection)))
        update! (fn [f & args]
                  (binding [ec/*execution-context* ctx]
                    (when (current?) (apply swap! state f args))))]
    (when (and connection (current?)
               (or (not= connection (:connection @state)) (= :error (:status @state))))
      (reset! state {:connection connection :status :pending :dirty {}})
      (let [topic (keyword "memory" (str (:party connection)))
            strategy (proto/pub-sub-only-strategy
                       (fn [{:keys [kind room-id]}]
                         (when (= kind :memory-changed)
                           (update! update-in [:dirty (str room-id)] (fnil inc 0)))))]
        (go
          (let [result (<! (pubsub/subscribe! peer #{topic}
                                            {:strategies {topic strategy}
                                             :on-handshake-complete
                                             (fn [_] (update! assoc :status :ready))}))]
            (when-let [error (:error result)]
              (update! assoc :status :error :error (remote/error-text error)))))))))

(defn problem [stream]
  (when (= :error (:status stream))
    (str "Live Memory updates unavailable: " (:error stream))))
