(ns is.simm.benchmarks.matter-judgement-test
  (:require [clojure.test :refer [deftest is]] [is.simm.ops.checkers :as checkers]))
(def checker (slurp "examples/workflows/legal-matter-normalized/judge-checker.clj"))
(def gold {:packet-id "packet-1" :subject-id "attempt-1" :criterion-ids ["C-001" "C-002"]
           :allowed-files {"/subject/memo.md" "EBITDA is inconsistent. Reconcile before valuation."}})
(def answer {:version 1 :packet-id "packet-1" :subject-id "attempt-1"
             :criteria [{:id "C-001" :verdict :pass :reason "Identifies discrepancy." :basis :excerpt
                         :evidence [{:path "/subject/memo.md" :quote "EBITDA is inconsistent."}]}
                        {:id "C-002" :verdict :fail :reason "No dollar amount anywhere." :basis :whole-output :evidence []}]})
(defn score [answer]
  (checkers/run-checker checker {:gold gold :files {"/out/assessment.edn" (pr-str answer)}}))
(deftest complete-assessment-is-not-a-successful-legal-subject
  (is (= 1.0 (:reward (score answer))))
  (is (= 1.0 (:reward (score (update answer :criteria #(mapv (fn [row] (assoc row :verdict :fail)) %))))))
  (is (= 1.0 (:reward (score (assoc-in answer [:criteria 0 :verdict] :unassessable))))))
(deftest subject-coverage-and-evidence-are-independently-bound
  (doseq [[k bad] [[:subject-bound? (assoc answer :subject-id "another-attempt")]
                  [:subject-bound? (assoc answer :packet-id "another-packet")]
                  [:criterion-coverage? (update answer :criteria pop)]
                  [:criterion-coverage? (assoc-in answer [:criteria 1 :id] "C-001")]
                  [:allowed-quotes? (assoc-in answer [:criteria 0 :evidence 0 :path] "/subject/tracker.edn")]
                  [:allowed-quotes? (assoc-in answer [:criteria 0 :evidence 0 :quote] "$100 million")]
                  [:assessment-shape? (assoc-in answer [:criteria 0 :verdict] :maybe)]
                  [:assessment-shape? (assoc-in answer [:criteria 0 :evidence] [])]]]
    (is (false? (get-in (score bad) [:checks k])))
    (is (zero? (:reward (score bad))))))
(deftest malformed-trusted-packets-cannot-certify-an-assessment
  (doseq [bad [nil (dissoc gold :packet-id) (assoc gold :subject-id nil)
               (assoc gold :criterion-ids []) (assoc gold :criterion-ids ["C-001" "C-001"])
               (assoc gold :allowed-files {})]]
    (let [result (checkers/run-checker checker {:gold bad :files {"/out/assessment.edn" (pr-str answer)}})]
      (is (false? (get-in result [:checks :packet-valid?])))
      (is (zero? (:reward result))))))
(deftest multi-conflict-criteria-can-cite-more-than-four-excerpts
  (let [c (get-in answer [:criteria 0 :evidence 0])]
    (is (= 1.0 (:reward (score (assoc-in answer [:criteria 0 :evidence] (vec (repeat 5 c)))))))
    (is (= 1.0 (:reward (score (assoc-in answer [:criteria 0 :evidence] (vec (repeat 8 c)))))))
    (is (false? (get-in (score (assoc-in answer [:criteria 0 :evidence] (vec (repeat 9 c)))) [:checks :assessment-shape?])))))
