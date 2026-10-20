(ns is.simm.uis.web.desktop.topic-subscription
  "A canonical-runtime owner for one physical pubsub strategy. Projection state
   stays in a Spindel signal; the gate and retained strategy own native resources."
  (:require [clojure.core.async :refer [chan go <! put!] :include-macros true]
            [kabel.pubsub :as pubsub]
            [kabel.pubsub.protocol :as protocol]
            [org.replikativ.spindel.engine.core :as ec]
            [is.simm.uis.web.desktop.runtime :refer [runtime]]
            [is.simm.uis.web.desktop.connection-epoch :as epoch])
  (:require-macros [org.replikativ.spindel.signal :refer [signal]]))

(defn owner []
  (let [gate (chan 1)]
    (put! gate true)
    {:state (signal runtime nil) :active (atom nil) :requested (atom nil) :gate gate}))

(defn ensure!
  "Install one topic for an authenticated epoch. Replacement joins guarded
   disposal, and callbacks cannot publish into a replacement epoch. Only the
   canonical runtime may create physical subscriptions, not a forked UI render.
   on-ready runs after handshake drain; use it for authoritative catch-up.
   Failures remain visible until a new epoch or an explicit retry."
  [{:keys [state active requested gate]} {:keys [connection peer topic on-message on-ready initial retry?]}]
  (when (and (identical? (ec/home-context runtime)
                         (ec/home-context ec/*execution-context*))
             peer (epoch/current? connection)
             (or (not= connection (:connection @state))
                 (and retry? (= :error (:status @state)))))
    (let [token (random-uuid)
          current? #(and (= token @requested) (epoch/current? connection))
          update! (fn [f]
                    (binding [ec/*execution-context* runtime]
                      (when (current?) (swap! state f))))
          fail! #(update! (fn [s] (assoc s :status :error :error %)))
          strategy (protocol/pub-sub-only-strategy
                    (fn [message]
                      (binding [ec/*execution-context* runtime]
                        (when (current?) (when on-message (on-message message))))))]
      (reset! requested token)
      (reset! state (merge initial {:connection connection :status :pending}))
      (go
        (<! gate)
        (try
          (when (current?)
            (when-let [{old-peer :peer old-topic :topic old-strategy :strategy} @active]
              (let [result (<! (pubsub/unsubscribe! old-peer #{old-topic}
                                                    {:expected-strategies {old-topic old-strategy}}))]
                (when-let [error (:error result)] (throw (ex-info "Subscription cleanup failed" {:error error})))
                (when (instance? js/Error result) (throw result))
                (when-not (true? (:ok result))
                  (throw (ex-info "Subscription cleanup did not confirm completion" {})))
                (reset! active nil)))
            (when (current?)
              (reset! active {:peer peer :topic topic :strategy strategy})
              (let [result (<! (pubsub/subscribe!
                                peer #{topic}
                                {:strategies {topic strategy}
                                 :on-handshake-complete
                                 (fn [_]
                                   (binding [ec/*execution-context* runtime]
                                     (when (current?)
                                       (update! #(assoc % :status :ready))
                                       (when (and (current?) on-ready) (on-ready)))))}))]
                (when-let [error (:error result)] (fail! error))
                (when (instance? js/Error result) (fail! result)))))
          (catch :default e (fail! e))
          (finally (put! gate true)))))))

(defn retry!
  "Explicit same-epoch error recovery through this SAME owner and disposal
   gate. Ordinary ensure! never retries an error or replaces a pending strategy."
  [owner opts]
  (ensure! owner (assoc opts :retry? true)))
