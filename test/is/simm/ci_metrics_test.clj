(ns is.simm.ci-metrics-test
  (:require [clojure.test :refer [deftest is]]
            [is.simm.ci-metrics :as metrics]
            [kaocha.plugin.profiling :as profiling]))

(deftest namespace-residual-keeps-fixture-overhead-distinct-from-test-work
  (let [m (metrics/measurement
            {:kaocha.testable/id :example
             :kaocha.testable/type :kaocha.type/ns
             ::profiling/duration 100
             :kaocha.result/tests [{::profiling/duration 30}
                                   {::profiling/duration 50}
                                   {:kaocha.testable/skip true}]})]
    (is (= 100 (:elapsed-ns m)))
    (is (= 80 (:children-elapsed-ns m)))
    (is (= 20 (:outside-children-ns m)))))
