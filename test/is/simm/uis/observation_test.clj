(ns is.simm.uis.observation-test
  (:require [clojure.test :refer [deftest is]]
            [is.simm.uis.web.desktop.observation :as observation]))

(deftest invalidations-during-loading-coalesce-without-losing-the-last-update
  (let [[first-state first-request] (observation/offer nil :socket-a [:source 0] :first)
        [pending ignored] (observation/offer first-state :socket-a [:source 1] :ignored)
        [latest ignored-again] (observation/offer pending :socket-a [:source 2] :also-ignored)
        done (observation/finish latest first-request)
        [second-state second-request] (observation/offer done :socket-a [:source 2] :second)]
    (is (observation/current? first-state first-request))
    (is (nil? ignored))
    (is (nil? ignored-again))
    (is (observation/owns? latest first-request))
    (is (not (observation/current? latest first-request)))
    (is (= [:source 2] (:wanted done)))
    (is (= {:epoch :socket-a :generation [:source 2] :token :second} second-request))
    (is (observation/current? second-state second-request))
    (is (= second-state (observation/finish second-state first-request)))
    (is (nil? (second (observation/offer (observation/finish second-state second-request)
                                        :socket-a [:source 2] :unused))))))

(deftest socket-replacement-supersedes-a-hung-fetch-without-timers
  (let [[old request] (observation/offer nil :socket-a 0 :first)
        [fresh replacement] (observation/offer old :socket-b 0 :second)]
    (is (observation/current? fresh replacement))
    (is (not (observation/owns? fresh request)))
    (is (= fresh (observation/finish fresh request)))
    (is (= :second (get-in fresh [:active :token])))))

(deftest disconnected-and-failed-observations-do-not-spin-or-retry-implicitly
  (let [[waiting none] (observation/offer nil nil 0 :unused)
        [started request] (observation/offer waiting :socket 0 :first)
        failed (observation/finish started request)
        [unchanged retry] (observation/offer failed :socket 0 :unused)
        [explicit explicit-request] (observation/offer failed :socket 1 :retry)
        [offline offline-request] (observation/offer explicit nil 1 :unused)]
    (is (nil? none))
    (is (nil? retry))
    (is (= failed unchanged))
    (is (observation/current? explicit explicit-request))
    (is (nil? offline-request))
    (is (not (observation/owns? offline explicit-request)))))

(deftest copied-observation-data-has-no-shared-mutable-gate
  (let [[base request] (observation/offer nil :socket 0 :first)
        [left _] (observation/offer base :socket 1 :left)
        right (observation/finish base request)]
    (is (observation/current? base request))
    (is (not (observation/current? left request)))
    (is (nil? (:active right)))
    (is (= request (:active base)))
    (is (not= left right))))

(deftest a-copied-world-must-not-wait-for-the-original-worlds-native-reply
  (let [[base original] (observation/offer nil [:socket :original-world] nil :first)
        [copied request] (observation/offer base [:socket :copied-world] nil :copy)]
    (is (observation/current? base original))
    (is (observation/current? copied request))
    (is (not (observation/owns? copied original)))
    (is (= :copy (:token request)))
    (is (= copied (observation/finish copied original)))))
