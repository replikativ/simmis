(ns is.simm.ops.wiki-named-calibration-test
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [dvergr.catalog.room :as catalog]
            [is.simm.ops.checkers :as checkers]
            [is.simm.ops.knowledge-inputs :as inputs]
            [is.simm.ops.workflow-runs :as runs]
            [is.simm.ops.workflows :as workflows]
            [is.simm.starter.wiki-maintenance :as starter]))

(defn- example []
  (let [root (io/file "examples/workflows/wiki-maintenance-named-eval-v2")]
    (into {} (for [file (file-seq root) :when (.isFile file)]
               [(str (.relativize (.toPath root) (.toPath file))) (slurp file)]))))

(deftest named-development-example-calibrates-the-unchanged-shipped-program
  (let [source (example) shipped (starter/source 2)
        name "wiki-maintenance-named-development"
        bundle (catalog/bundle name source)
        report (checkers/calibrate bundle checkers/calibration-policy (inputs/declaration source))]
    (is (= shipped (select-keys source (keys shipped))))
    (is (:ok (workflows/inspect-snapshot name {:files source})))
    (is (:ok? report))
    (is (= #{"supported" "no-change"} (set (keys (:cases bundle)))))
    (is (= #{:update :new-page :no-op} (set (keys (:references (:calibration bundle))))))
    (is (= 8 (count (:damaged (:calibration bundle)))))
    (doseq [case ["supported" "no-change"]]
      (let [prepared (runs/prepare-source {:files source}
                                          {:name name :expected-bundle (:id bundle) :case case})
            frozen (:inputs prepared)
            company (get frozen "/in/knowledge/sources/wiki/Company.md")
            evidence (get frozen "/in/knowledge/sources/inbox/founder-note.md")]
        (is (= (into {} (filter (fn [[path _]] (str/starts-with? path "/in/knowledge/"))) frozen)
               (:inputs (inputs/from-inputs (inputs/declaration source) frozen))))
        (is (= (get shipped "fixtures/workflow-context/wiki-maintenance.txt")
               (get frozen "/workflow-context/wiki-maintenance.txt")))
        (is (str/includes? company "Bright Harbor Labs is developing LedgerNest"))
        (is (str/includes? company "revenue: 0"))
        (is (str/includes? company "active: false"))
        (is (str/includes? company "c9c344b7-83c7-41d8-b3d1-f2e684ae50e6"))
        (is (str/includes? company "2a54e1a1-146e-44c8-b6b9-5c185149e6eb"))
        (is (= (= "no-change" case) (str/includes? company "USD19")))
        (is (str/includes? evidence "Bright Harbor Labs"))
        (is (str/includes? evidence "LedgerNest"))
        (is (str/includes? evidence "unvalidated hypothesis"))
        (is (not-any? #(re-find #"(?:^|/)(?:gold\.edn|calibration\.edn|checker\.clj)$" %) (keys frozen)))
        (is (every? #(or (= "/workflow-context/wiki-maintenance.txt" %)
                         (= "/in/knowledge/manifest.edn" %)
                         (str/starts-with? % "/in/knowledge/sources/")) (keys frozen)))))))
