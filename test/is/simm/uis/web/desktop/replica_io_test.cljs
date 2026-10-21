(ns is.simm.uis.web.desktop.replica-io-test
  (:require [cljs.test :refer-macros [deftest is async]]
            [clojure.core.async :refer [go <! put! promise-chan poll!] :include-macros true]
            [datahike.api :as d]
            [datahike.kabel.connector :as connector]
            [is.simm.uis.web.desktop.replica-io :as io]))

(defn- delivered [value] (doto (promise-chan) (put! value)))

(defn- config []
  (let [id (random-uuid)]
    {:store {:backend :tiered :id id
             :frontend-config {:backend :memory :id id}
             :backend-config {:backend :indexeddb :id id :name (str id)}}}))

(defn- options [generation published]
  {:generation generation :valid? (constantly true)
   :publish! (fn [handle current?] (when (current?) (swap! published conj (:conn handle))))
   :unpublish! (fn [_])})

(deftest lost-preparation-does-not-own-the-native-lease
  (async done
         (go
           (let [cfg (config) lost (promise-chan) started (promise-chan)
                 published (atom []) connected (atom 0) conn (atom {:writer :writer})]
             (try
               (with-redefs [connector/connect-kabel (fn [_ _] (swap! connected inc) (delivered conn))
                             connector/await-release! (fn [_] (delivered true))
                             d/release (fn ([_]) ([_ _]))]
                 (let [a (io/open! cfg (assoc (options :a published) :prepare! #(do (put! started true) lost)))]
                   (<! started)
                   (is (= :idle (:phase (io/status cfg))) "Preparation has not entered native construction")
                   (is (zero? @connected))
                   (let [b (io/open! cfg (options :b published))]
                     (is (= :ready (:status (<! b))))
                     (is (instance? js/Error (<! a)) "Superseded preparation settles without its RPC callback")
                     (put! lost {:ready? true})
                     (is (= [conn] @published))
                     (is (= 1 @connected))
                     (is (true? (<! (io/retire! cfg)))))))
               (catch :default error (is false (str error)))
               (finally (done)))))))

(deftest replacement-waits-for-construction-and-owned-cleanup
  (async done
         (go
           (let [cfg (config) opened-a (promise-chan) finish-a (promise-chan)
                 shutting-down (promise-chan) drained (promise-chan)
                 events (atom []) published (atom [])
                 conn-a (atom {:writer :a}) conn-b (atom {:writer :b})]
             (try
               (with-redefs [connector/connect-kabel
                             (fn [configuration _]
                               (if (= :a (:peer configuration))
                                 (do (swap! events conj :open-a) (put! opened-a true) finish-a)
                                 (do (swap! events conj :open-b) (delivered conn-b))))
                             connector/await-release!
                             (fn [conn]
                               (let [id (if (identical? conn conn-a) :a :b)]
                                 (swap! events conj [:native-close id])
                                 (if (= :a id) (do (put! shutting-down true) drained) (delivered true))))
                             d/release (fn ([conn] (swap! events conj [:release (if (identical? conn conn-a) :a :b)]))
                                         ([_ _]))]
                 (let [a (io/open! (assoc cfg :peer :a) (options :a published))]
                   (<! opened-a)
                   (let [b (io/open! (assoc cfg :peer :b) (options :b published))]
                     (is (instance? js/Error (<! a)) "Logical cancellation does not wait for native construction")
                     (is (= [:open-a] @events))
                     (put! finish-a conn-a)
                     (<! shutting-down)
                     (is (= [:open-a [:release :a] [:native-close :a]] @events))
                     (is (empty? @published) "Old constructor must never publish")
                     (is (nil? (poll! b)) "Replacement waits for post-subscription native close too")
                     (put! drained true)
                     (is (= :ready (:status (<! b))))
                     (is (instance? js/Error (<! a)))
                     (is (= [:open-a [:release :a] [:native-close :a] :open-b] @events))
                     (is (= [conn-b] @published))
                     (is (true? (<! (io/retire! cfg)))))))
               (catch :default error (is false (str error)))
               (finally (done)))))))

(deftest uncertain-native-close-holds-the-slot-until-explicit-recovery
  (async done
         (go
           (let [cfg (config) conn (atom {:writer :fixture})
                 published (atom []) opens (atom 0) closes (atom 0)
                 failure (ex-info "Native close not confirmed" {})]
             (try
               (with-redefs [connector/connect-kabel (fn [& _] (swap! opens inc) (delivered conn))
                             connector/await-release! (fn [_]
                                                        (delivered (if (= 1 (swap! closes inc)) failure true)))
                             d/release (fn ([_]) ([_ _]))]
                 (is (= :ready (:status (<! (io/open! cfg (options :a published))))))
                 (is (identical? failure (<! (io/retire! cfg))))
                 (is (= :held (:phase (io/status cfg))))
                 (is (identical? failure (<! (io/open! cfg (options :b published)))))
                 (is (= 1 @opens) "No constructor after unconfirmed native close")
                 (is (true? (<! (io/recover! cfg))))
                 (is (= 2 @closes))
                 (is (= :ready (:status (<! (io/open! cfg (options :b published))))))
                 (is (= 2 @opens))
                 (is (true? (<! (io/retire! cfg)))))
               (catch :default error (is false (str error)))
               (finally (done)))))))

(deftest explicit-recovery-allows-the-same-generation-to-open-again
  (async done
         (go
           (let [cfg (config) published (atom []) calls (atom 0) recovered (atom 0)
                 failure (ex-info "Native construction left cleanup pending" {})
                 conn (atom {:writer :writer})]
             (try
               (with-redefs [connector/connect-kabel (fn [_ _]
                                                       (delivered (if (= 1 (swap! calls inc)) failure conn)))
                             connector/cleanup-confirmed? (constantly false)
                             connector/recover-connect! (fn [error]
                                                          (is (identical? failure error))
                                                          (swap! recovered inc)
                                                          (delivered true))
                             connector/await-release! (fn [_] (delivered true))
                             d/release (fn ([_]) ([_ _]))]
                 (is (identical? failure (<! (io/open! cfg (options :same published)))))
                 (is (= :held (:phase (io/status cfg))))
                 (is (true? (<! (io/recover! cfg))))
                 (is (= 1 @recovered))
                 (is (= :ready (:status (<! (io/open! cfg (options :same published))))))
                 (is (= 2 @calls))
                 (is (= [conn] @published))
                 (is (true? (<! (io/retire! cfg)))))
               (catch :default error (is false (str error)))
               (finally (done)))))))

(deftest same-uuid-cannot-open-an-incompatible-store
  (async done
         (go
           (let [cfg (config) published (atom []) conn (atom {:writer :writer}) calls (atom 0)]
             (try
               (with-redefs [connector/connect-kabel (fn [_ _] (swap! calls inc) (delivered conn))
                             connector/await-release! (fn [_] (delivered true))
                             d/release (fn ([_]) ([_ _]))]
                 (is (= :ready (:status (<! (io/open! cfg (options :a published))))))
                 (let [error (<! (io/open! (assoc-in cfg [:store :backend-config :name] "different")
                                           (options :b published)))]
                   (is (instance? js/Error error))
                   (is (re-find #"different store configuration" (.-message error))))
                 (is (= 1 @calls))
                 (is (= :ready (:phase (io/status cfg))))
                 (is (true? (<! (io/retire! cfg)))))
               (catch :default error (is false (str error)))
               (finally (done)))))))

(deftest retirement-cancels-preparation-before-a-native-resource-exists
  (async done
         (go
           (let [cfg (config) preparation (promise-chan) started (promise-chan) calls (atom 0)]
             (try
               (with-redefs [connector/connect-kabel (fn [_ _] (swap! calls inc) (delivered nil))]
                 (let [result (io/open! cfg (assoc (options :a (atom []))
                                                   :prepare! #(do (put! started true) preparation)))]
                   (<! started)
                   (is (true? (<! (io/retire! cfg))))
                   (is (instance? js/Error (<! result)))
                   (put! preparation {:ready? true})
                   (is (zero? @calls))))
               (catch :default error (is false (str error)))
               (finally (done)))))))
