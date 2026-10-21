(ns is.simm.ops.market-discovery-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.set :as set]
            [dvergr.catalog.room :as catalog]
            [is.simm.ops.benchmark-dataset :as dataset]
            [is.simm.ops.checkers :as checkers]
            [is.simm.ops.workflows :as workflows]))

(defn example-files []
  (let [root (.toPath (io/file "examples/workflows/market-discovery-eval"))]
    (into (sorted-map)
          (for [file (file-seq (.toFile root)) :when (.isFile file)]
            [(str (.relativize root (.toPath file))) (slurp file)]))))

(deftest example-is-valid-calibrated-and-explicitly-split
  (let [files (example-files)
        bundle (catalog/bundle "market-discovery-eval" files)
        inspection (workflows/inspect-snapshot "market-discovery-eval"
                                               {:files files :manifest []})]
    (is (:ok inspection))
    (is (= 4 (count (:cases bundle))))
    (is (= #{"dev-empty" "dev-notes"}
           (set (:cases (dataset/select-cases files (keys (:cases bundle)) {:split "development"})))))
    (is (thrown? clojure.lang.ExceptionInfo (dataset/select-cases files (keys (:cases bundle)) {})))
    (is (:ok? (checkers/calibrate bundle)))
    (is (= :ad-hoc (:tier (checkers/evaluator bundle))))))

(deftest held-out-documents-are-disjoint-and-example-evidence-is-explicit
  (let [files (example-files)
        evidence (fn [case] (edn/read-string (get files (str "cases/" case "/fixtures/inputs/evidence.edn"))))
        urls #(set (map :url (:sources (evidence %))))
        development (urls "dev-notes")
        held-out (urls "holdout-automation")]
    (is (empty? (set/intersection development held-out)))
    (is (= 2 (count development)))
    (is (= 2 (count held-out)))
    (is (false? (:synthetic? (evidence "dev-notes"))))
    (is (= [] (:sources (evidence "dev-empty"))))
    (is (= "Evaluation-authored adversarial text; NOT quoted from either vendor."
           (:adversarial-text-origin (evidence "holdout-source-instructions"))))))

(deftest evidence-checks-reject-fabricated-facts-but-do-not-claim-market-validation
  (let [bundle (catalog/bundle "market-discovery-eval" (example-files))
        good (get-in bundle [:calibration :reference])
        gold (get-in bundle [:cases "dev-notes" :gold])
        verdict #(checkers/run-checker (:checker bundle) {:files % :gold gold})]
    (is (= 1.0 (:reward (verdict good))))
    (doseq [[_ variant] (get-in bundle [:calibration :damaged])]
      (is (= 0.0 (:reward (verdict (:files variant))))))
    (is (= #{:output-contract? :known-source-ids? :fixture-facts-grounded?
             :required-fixture-facts? :honest-status?}
           (set (keys (:checks (verdict good))))))))
