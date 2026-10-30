(ns is.simm.model.github-pins-test
  (:require [clojure.test :refer [deftest is]]
            [datahike.api :as d]
            [is.simm.model.github-connections :as connections]
            [is.simm.model.github-integrations :as integrations]
            [is.simm.model.github-integrations-test :as fixtures]
            [is.simm.model.github-pins :as pins]))

(defn- fixture [f]
  (#'fixtures/with-store
   (fn [control]
     (d/transact control pins/schema)
     (let [connection (#'fixtures/connection)
           target (:target connection) head (random-uuid) oid (apply str (repeat 40 "a"))]
       (d/transact control [[:db.fn/call integrations/initialize-tx
                             (:room connection) (random-uuid) (connections/connection-id connection)
                             head oid (random-uuid) (random-uuid)]])
       (f control
          {:version 1 :purpose :upstream-base :connection connection
           :cursor (integrations/current-in @control (:room connection))
           :selection {:head head :git-oid oid
                       :basis (merge (select-keys target [:repository :store :backend :branch :ref])
                                     {:configuration (random-uuid) :commit (random-uuid) :max-tx 1 :datoms 1})}
           :root (random-uuid) :claim (random-uuid) :by (random-uuid)})))))

(deftest an-intent-has-one-root-and-replay-never-grants-creation
  (fixture
   (fn [control body]
     (let [key (pins/intent-key body) ref (random-uuid)
           first (d/transact control [[:db.fn/call pins/prepare-tx body ref]])
           row (pins/by-key @control key)
           other (assoc body :root (random-uuid) :claim (random-uuid) :by (random-uuid))
           second (d/transact control [[:db.fn/call pins/prepare-tx other (random-uuid)]])]
       (is (nil? (pins/by-key (:db-before first) key)))
       (is (= row (pins/by-key (:db-before second) key)))
       (is (= row (pins/by-key @control key)))
       (is (= (:root body) (:github.pin/id row)))
       (is (= :creating (:github.pin/state row)))
       (d/transact control (pins/confirm-tx @control row body))
       (let [retained (pins/by-key @control key)]
         (is (= :retained (:github.pin/state retained)))
         (is (= [] (pins/confirm-tx @control retained body)))
         (is (thrown? clojure.lang.ExceptionInfo (pins/confirm-tx @control row body))))))))

(deftest stale-cursor-and-altered-selection-cannot-be-certified
  (fixture
   (fn [control body]
     (d/transact control [[:db.fn/call pins/prepare-tx body (random-uuid)]])
     (let [row (pins/by-key @control (pins/intent-key body))
           changed (assoc-in body [:selection :basis :store] (random-uuid))]
       (is (thrown? clojure.lang.ExceptionInfo (pins/body-id changed)))
       (is (thrown? clojure.lang.ExceptionInfo (pins/body-id (assoc body :extra true))))
       (d/transact control [{:db/id (get-in body [:cursor :db/id]) :github.integration/head (random-uuid)}])
       (is (thrown? clojure.lang.ExceptionInfo (pins/confirm-tx @control row body)))
       (is (thrown? clojure.lang.ExceptionInfo (pins/prepare-tx @control body (random-uuid))))
       (is (= :creating (:github.pin/state (pins/by-key @control (pins/intent-key body)))))))))
