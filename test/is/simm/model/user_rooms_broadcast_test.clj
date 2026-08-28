(ns is.simm.model.user-rooms-broadcast-test
  (:require [clojure.test :refer [deftest is testing]]
            [is.simm.model.user-rooms-broadcast :as sut]
            [kabel.pubsub :as pubsub]))

(deftest explicit-invalidation-test
  (testing "state outside the system DB can invalidate the same thin roster topic"
    (let [peer (atom {})
          published (atom nil)
          alice (random-uuid)
          bob (random-uuid)]
      (with-redefs [pubsub/topic-registered? (constantly true)
                    pubsub/publish! (fn [actual-peer topic payload]
                                      (reset! published [actual-peer topic payload]))]
        (sut/ensure-topic-registered! peer)
        (sut/notify-parties! [alice bob alice])
        (is (= [peer sut/topic {:party-ids #{alice bob}}]
               @published))))))
