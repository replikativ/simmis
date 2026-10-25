(ns is.simm.uis.screen-sampling-test
  (:require #?(:clj [clojure.test :refer [deftest is testing]]
               :cljs [cljs.test :refer-macros [deftest is testing]])
            [is.simm.uis.web.desktop.screen-sampling :as sampling]))

(deftest quiet-screens-never-trigger-heartbeat-inference
  (let [signature (vec (repeat 1000 100))]
    (is (sampling/due? 0 nil nil signature))
    (is (not (sampling/due? 120000 0 signature signature)))
    (is (not (sampling/due? 86400000 0 signature signature)))
    (is (not (sampling/changed? signature (assoc signature 42 255))))))

(deftest text-changes-are-bounded-and-compared-with-the-submitted-frame
  (let [before (vec (repeat 1000 100))
        ;; Enough sparse text changes to matter, though the mean is only 0.6.
        after (into (vec (repeat 20 130)) (repeat 980 100))]
    (is (sampling/changed? before after))
    (is (not (sampling/due? 19999 0 before after)))
    (is (sampling/due? 20000 0 before after))
    (is (sampling/due? 24000 0 before after))
    (is (not (sampling/due? 40000 20000 after after)))
    (testing "new grant requests a fresh observation without bypassing the shared cap"
      (is (not (sampling/due? 25000 20000 nil after)))
      (is (sampling/due? 40000 20000 nil after)))))

(deftest broad-scene-changes-and-capture-resizing-are-visible
  (is (sampling/changed? [0 0 0] [7 7 7]))
  (is (sampling/changed? [0 0] [0 0 0])))

(deftest busy-server-does-not-consume-a-quiet-new-grant-observation
  (let [scene [100 100 100]
        first-request (sampling/request (sampling/initial :capture) 0 scene)
        accepted (sampling/acknowledge first-request (:pending first-request) true)
        granted (sampling/refresh accepted)
        busy (sampling/request granted 20000 scene)
        declined (sampling/acknowledge busy (:pending busy) false)
        retry (sampling/request declined 40000 scene)
        completed (sampling/acknowledge retry (:pending retry) true)]
    (is (= scene (:signature accepted)))
    (is (identical? busy (sampling/request busy 30000 scene)) "One upload at a time")
    (is (nil? (:signature declined)) "HTTP 202 does not acknowledge the scene")
    (is (identical? declined (sampling/request declined 39999 scene)))
    (is (:pending retry))
    (is (= scene (:signature completed)))
    (is (identical? completed (sampling/request completed 86400000 scene)))))

(deftest acknowledgements-cannot-consume-a-new-grant-or-a-new-capture
  (let [scene [10 10]
        old (sampling/request (sampling/initial :old) 0 scene)
        attempt (:pending old)
        granted (sampling/refresh old)
        acknowledged (sampling/acknowledge granted attempt true)
        fresh (sampling/request (sampling/initial :new) 0 scene)]
    (is (nil? (:signature acknowledged)))
    (is (nil? (:pending acknowledged)))
    (is (:pending (sampling/request acknowledged 20000 scene)))
    (is (= fresh (sampling/acknowledge fresh attempt true)))))
