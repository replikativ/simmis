(ns is.simm.uis.web.desktop.topic-subscription-test
  (:require [cljs.test :refer-macros [deftest is async]]
            [clojure.core.async :refer [go <! put! promise-chan] :include-macros true]
            [kabel.pubsub :as pubsub]
            [org.replikativ.spindel.core :as sp]
            [org.replikativ.spindel.engine.context :as context]
            [org.replikativ.spindel.engine.core :as ec]
            [is.simm.uis.web.desktop.runtime :refer [runtime]]
            [is.simm.uis.web.desktop.connection-epoch :as epoch]
            [is.simm.uis.web.desktop.topic-subscription :as topic]))

(defn- delivered [value] (doto (promise-chan) (put! value)))

(deftest explicit-error-retry-joins-exact-old-strategy-and-never-opens-a-second-lane
  (async done
         (go
           (let [owner (topic/owner) connection {:epoch (random-uuid)}
                 subscriptions (atom []) messages (atom []) ready (atom [])
                 failed (promise-chan) second-sub (promise-chan)
                 cleanup-start (promise-chan) cleanup-finish (promise-chan)
                 watch-key (random-uuid)
                 opts {:connection connection :peer :peer :topic :test
                       :on-message #(swap! messages conj %) :on-ready #(swap! ready conj :ready)}]
             (try
               (with-redefs [pubsub/subscribe!
                             (fn ([_ _] (delivered {}))
                               ([_ _ options]
                                (let [n (count (swap! subscriptions conj options))]
                                  (when (= 2 n) (put! second-sub true))
                                  (delivered (if (= 1 n) {:error (js/Error. "First handshake failed")} {})))))
                             pubsub/unsubscribe!
                             (fn ([_ _] (delivered {:ok true}))
                               ([_ topics options]
                                (is (= #{:test} topics))
                                (is (identical? (get-in (first @subscriptions) [:strategies :test])
                                                (get-in options [:expected-strategies :test])))
                                (put! cleanup-start true) cleanup-finish))]
                 (try
                   (binding [ec/*execution-context* runtime]
                     (reset! epoch/connection connection)
                     (add-watch (:state owner) watch-key
                                (fn [_ _ _ s] (when (= :error (:status s)) (put! failed true))))
                     (topic/ensure! owner opts))
                   (<! failed)
                   (binding [ec/*execution-context* runtime]
                     (topic/ensure! owner opts)
                     (is (= :error (:status @(:state owner))) "Ordinary rendering never retries")
                     (topic/retry! owner opts)
                     (topic/retry! owner opts))
                   (<! cleanup-start)
                   (is (= 1 (count @subscriptions)) "Replacement waits for disposal ACK")
                   ((:on-publish-fn (get-in (first @subscriptions) [:strategies :test])) :stale)
                   ((:on-handshake-complete (first @subscriptions)) nil)
                   (is (empty? @messages)) (is (empty? @ready))
                   (put! cleanup-finish {:ok true})
                   (<! second-sub)
                   ((:on-handshake-complete (second @subscriptions)) nil)
                   ((:on-publish-fn (get-in (second @subscriptions) [:strategies :test])) :fresh)
                   ((:on-publish-fn (get-in (first @subscriptions) [:strategies :test])) :still-stale)
                   (is (= 2 (count @subscriptions)))
                   (is (= [:ready] @ready)) (is (= [:fresh] @messages))
                   (finally
              ;; Join before WITH-REDEFS restores physical transport functions.
                     (put! cleanup-finish {:ok true})
                     (<! (:gate owner)) (put! (:gate owner) true))))
               (catch :default error (is false (str error)))
               (finally
                 (binding [ec/*execution-context* runtime] (remove-watch (:state owner) watch-key))
                 (done)))))))

(deftest real-spawned-context-can-subscribe-but-forks-cannot
  (async done
         (go
           (let [owner (topic/owner) connection {:epoch (random-uuid)}
                 subscribed (promise-chan) spawned (promise-chan) calls (atom 0)
                 opts {:connection connection :peer :peer :topic :test}]
             (try
               (with-redefs [pubsub/subscribe! (fn ([_ _] (delivered {}))
                                                 ([_ _ _] (swap! calls inc) (put! subscribed true) (delivered {})))]
                 (binding [ec/*execution-context* runtime]
                   (reset! epoch/connection connection)
                   (let [fork (context/fork-context runtime)]
                     (try
                       (binding [ec/*execution-context* fork] (topic/ensure! owner opts))
                       (binding [ec/*execution-context* (assoc fork :home (:home runtime))]
                         (topic/ensure! owner opts))
                       (is (nil? @(:requested owner)))
                       (finally (context/close-context! fork))))
                   (sp/spawn! (sp/spin (topic/ensure! owner opts) (put! spawned true))))
                 (<! spawned)
                 (<! subscribed)
                 (is (= 1 @calls)))
               (catch :default error (is false (str error)))
               (finally (done)))))))

(deftest replacement-joins-exact-strategy-cleanup-and-fences-old-callbacks
  (async done
         (go
           (let [owner (topic/owner) a {:epoch (random-uuid)} b {:epoch (random-uuid)}
                 subscriptions (atom []) messages (atom []) ready (atom [])
                 first-sub (promise-chan) second-sub (promise-chan)
                 cleanup-start (promise-chan) cleanup-finish (promise-chan)
                 make-opts (fn [connection] {:connection connection :peer :peer :topic :test
                                             :on-message #(swap! messages conj [connection %])
                                             :on-ready #(swap! ready conj connection)})]
             (try
               (with-redefs [pubsub/subscribe!
                             (fn ([_ _] (delivered {}))
                               ([_ _ opts]
                                (let [n (count (swap! subscriptions conj opts))]
                                  (put! (if (= 1 n) first-sub second-sub) true))
                                (delivered {})))
                             pubsub/unsubscribe!
                             (fn ([_ _] (delivered {}))
                               ([_ topics opts]
                                (is (= #{:test} topics))
                                (is (identical? (get-in (first @subscriptions) [:strategies :test])
                                                (get-in opts [:expected-strategies :test])))
                                (put! cleanup-start true) cleanup-finish))]
                 (binding [ec/*execution-context* runtime]
                   (reset! epoch/connection a) (topic/ensure! owner (make-opts a)))
                 (<! first-sub)
                 (binding [ec/*execution-context* runtime]
                   (reset! epoch/connection b) (topic/ensure! owner (make-opts b)))
                 (<! cleanup-start)
                 (is (= 1 (count @subscriptions)))
                 ((:on-publish-fn (get-in (first @subscriptions) [:strategies :test])) :stale)
                 ((:on-handshake-complete (first @subscriptions)) nil)
                 (is (empty? @messages))
                 (is (empty? @ready))
                 (put! cleanup-finish {:ok true})
                 (<! second-sub)
                 ((:on-handshake-complete (second @subscriptions)) nil)
                 ((:on-publish-fn (get-in (second @subscriptions) [:strategies :test])) :fresh)
                 (is (= [b] @ready))
                 (is (= [[b :fresh]] @messages)))
               (catch :default error (is false (str error)))
               (finally (done)))))))

(deftest reentrant-ready-publication-cannot-run-an-obsolete-ready-hook
  (async done
         (go
           (let [owner (topic/owner) a {:epoch (random-uuid)} b {:epoch (random-uuid)}
                 first-sub (promise-chan) second-sub (promise-chan) subscriptions (atom []) ready (atom [])
                 make-opts (fn [connection] {:connection connection :peer :peer :topic :test
                                             :on-ready #(swap! ready conj connection)})
                 key (random-uuid)]
             (try
               (with-redefs [pubsub/subscribe!
                             (fn ([_ _] (delivered {}))
                               ([_ _ opts]
                                (put! (if (= 1 (count (swap! subscriptions conj opts))) first-sub second-sub) true)
                                (delivered {})))
                             pubsub/unsubscribe! (fn ([_ _] (delivered {:ok true})) ([_ _ _] (delivered {:ok true})))]
                 (binding [ec/*execution-context* runtime]
                   (reset! epoch/connection a)
                   (add-watch (:state owner) key
                              (fn [_ _ _ value]
                                (when (and (= a (:connection value)) (= :ready (:status value)))
                                  (reset! epoch/connection b)
                                  (topic/ensure! owner (make-opts b)))))
                   (topic/ensure! owner (make-opts a)))
                 (<! first-sub)
                 ((:on-handshake-complete (first @subscriptions)) nil)
                 (<! second-sub)
                 (is (empty? @ready))
                 ((:on-handshake-complete (second @subscriptions)) nil)
                 (is (= [b] @ready)))
               (catch :default error (is false (str error)))
               (finally
                 (binding [ec/*execution-context* runtime] (remove-watch (:state owner) key))
                 (done)))))))
