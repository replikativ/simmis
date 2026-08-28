(ns is.simm.model.store-sync-registration-test
  (:require [clojure.test :refer [deftest is testing]]
            [is.simm.model.knowledge-bases :as kbs]
            [is.simm.model.room-databases :as room-dbs]))

(defn- exercise-concurrent-registration!
  [register! connect-var]
  (let [scope (random-uuid)
        server-peer (atom {:pubsub {:topics {}}})
        opens (atom 0)
        register-store! (fn [registered-scope _conn peer _opts]
                          (swap! peer assoc-in
                                 [:pubsub :topics registered-scope]
                                 {:registered? true}))]
    (with-redefs-fn
      {connect-var (fn [_]
                     (swap! opens inc)
                     ;; Keep the first caller inside the critical section long
                     ;; enough for its peers to reach the same absent-topic check.
                     (Thread/sleep 25)
                     :conn)
       #'clojure.core/require (fn [& _])
       #'clojure.core/resolve (fn [_] register-store!)}
      (fn []
        (let [calls (doall (repeatedly 12
                                       #(future (register! scope server-peer))))]
          (doseq [call calls]
            (is (not= ::timeout (deref call 2000 ::timeout)))))))
    (is (= 1 @opens))
    (is (contains? (get-in @server-peer [:pubsub :topics]) scope))))

(deftest room-sync-registration-is-single-flight
  (testing "concurrent room subscribers open and register a scope once"
    (exercise-concurrent-registration!
     room-dbs/register-room-for-sync!
     #'room-dbs/connect-room-database)))

(deftest kb-sync-registration-is-single-flight
  (testing "concurrent KB subscribers open and register a scope once"
    (exercise-concurrent-registration!
     kbs/register-kb-for-sync!
     #'kbs/connect-kb-database)))
