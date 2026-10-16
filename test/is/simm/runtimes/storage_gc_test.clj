(ns is.simm.runtimes.storage-gc-test
  "The storage GC loop is OPT-IN: on datahike without #1117 a failed mark
   sweeps the whole store, which is how dev.simm.is lost its system DB. These
   tests never sweep — `start!` is replaced, so no store is touched."
  (:require [clojure.test :refer [deftest is testing]]
            [is.simm.runtimes.storage-gc :as gc]))

(deftest off-unless-asked
  (testing "no env, no config key: off"
    (is (false? (gc/enabled? nil {})))
    (is (false? (gc/enabled? nil {:storage-gc {}})))
    (is (false? (gc/enabled? "" {}))))
  (testing "config turns it on"
    (is (true? (gc/enabled? nil {:storage-gc {:enabled? true}}))))
  (testing "the environment decides when set"
    (is (true? (gc/enabled? "true" {})))
    (is (true? (gc/enabled? " TRUE " {})))
    (is (false? (gc/enabled? "false" {:storage-gc {:enabled? true}})))
    (testing "anything but true is off — a typo must not turn a deleter on"
      (is (false? (gc/enabled? "yes" {})))
      (is (false? (gc/enabled? "1" {}))))))

(deftest boot-starts-the-loop-only-when-enabled
  (let [starts (atom 0)]
    (with-redefs [gc/start! (fn [& _] (swap! starts inc) :started)]
      (is (= :disabled (gc/boot! (gc/enabled? nil {}))))
      (is (zero? @starts) "the default boot does not start the loop")
      (is (= :started (gc/boot! (gc/enabled? nil {:storage-gc {:enabled? true}}))))
      (is (= 1 @starts))
      (is (= :started (gc/boot! (gc/enabled? "true" {}))))
      (is (= 2 @starts)))))

(deftest datahike-version-floor
  (is (false? (gc/version-at-least? "0.8.1904" "0.8.1905")))
  (is (true? (gc/version-at-least? "0.8.1905" "0.8.1905")))
  (is (true? (gc/version-at-least? "0.8.2000" "0.8.1905")))
  (is (true? (gc/version-at-least? "0.9.1" "0.8.1905")))
  (is (false? (gc/version-at-least? "0.7.9999" "0.8.1905")))
  (testing "an unknown version (a source checkout) is not assumed safe"
    (is (false? (gc/version-at-least? nil "0.8.1905")))
    (is (false? (gc/version-at-least? "0.8.1905-SNAPSHOT" "0.8.1905"))))
  (testing "the pinned datahike predates the fix, so an enabled loop warns"
    (is (false? (gc/version-at-least? (gc/datahike-version)
                                      gc/datahike-fail-closed-gc-version)))))

(deftest a-failed-sweep-is-not-an-empty-sweep
  (is (= [] (gc/report-problems {:rooms 2 :system-db {:reclaimed 0}} 2)))
  (is (= [] (gc/report-problems {:rooms 2 :system-db {:reclaimed 17}} 2)))
  (is (= [:no-report] (gc/report-problems nil 2)))
  (is (= [:system-db-error] (gc/report-problems {:rooms 2 :system-db {:error "boom"}} 2)))
  (is (= [:system-db-timeout] (gc/report-problems {:rooms 2 :system-db {:reclaimed :timeout}} 2)))
  (is (= [:rooms-failed] (gc/report-problems {:rooms 1 :system-db {:reclaimed 0}} 2))
      "dvergr swallows a room's GC failure; the count is the only trace of it"))
