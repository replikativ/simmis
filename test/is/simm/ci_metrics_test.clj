(ns is.simm.ci-metrics-test
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]
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

(deftest completed-progress-records-can-be-read-back-independently
  (let [file (java.io.File/createTempFile "simmis-ci-metrics-" ".edn")
        records [{:phase :start} {:phase :loaded} {:phase :namespace :id :example}]]
    (try
      (with-redefs-fn {#'metrics/output-file (constantly file)}
        (fn []
          (doseq [[i record] (map-indexed vector records)]
            (#'metrics/write! "-progress.edn" record (pos? i)))))
      (with-open [reader (java.io.PushbackReader. (io/reader file))]
        (is (= records (vec (take-while #(not= ::eof %)
                                       (repeatedly #(edn/read {:eof ::eof} reader)))))))
      (finally (.delete file)))))
