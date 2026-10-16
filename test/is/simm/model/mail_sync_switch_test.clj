(ns is.simm.model.mail-sync-switch-test
  "Mail sync at boot is ON by default (production) and switchable off for an
   isolated or test instance."
  (:require [clojure.test :refer [deftest is testing]]
            [is.simm.model.mail-accounts :as mail]))

(deftest sync-switch
  (testing "default on"
    (is (true? (mail/sync-enabled? nil {})))
    (is (true? (mail/sync-enabled? nil {:mail {}}))))
  (is (false? (mail/sync-enabled? nil {:mail {:sync? false}})))
  (testing "the environment decides when set"
    (is (false? (mail/sync-enabled? "false" {})))
    (is (false? (mail/sync-enabled? " FALSE " {})))
    (is (true? (mail/sync-enabled? "true" {:mail {:sync? false}})))))

(deftest boot-does-not-sync-when-off
  (let [starts (atom 0)]
    (with-redefs [mail/start-all! (fn [] (swap! starts inc) true)]
      (is (= :disabled (mail/boot! (mail/sync-enabled? "false" {}))))
      (is (zero? @starts))
      (is (= :started (mail/boot! (mail/sync-enabled? nil {}))))
      (is (= 1 @starts)))))
