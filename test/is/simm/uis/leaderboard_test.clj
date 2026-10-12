(ns is.simm.uis.leaderboard-test
  (:require [clojure.test :refer [deftest is testing]]
            [is.simm.uis.web.desktop.leaderboard :as lb]))

(deftest verdicts-read-as-plain-words
  (is (= "haiku-0 better" (lb/verdict-label {:a "workflow/haiku-0" :b "workflow/luna-1" :verdict "a-better"})))
  (is (= "luna-1 better" (lb/verdict-label {:a "workflow/haiku-0" :b "workflow/luna-1" :verdict "b-better"})))
  (is (= "no detectable difference at these cases"
         (lb/verdict-label {:a "a" :b "b" :verdict "no-difference" :decidable? true})))
  (is (= "no detectable difference: too few disagreements to decide"
         (lb/verdict-label {:a "a" :b "b" :verdict "no-difference" :decidable? false})))
  (is (= [:better :none :undecidable]
         (map lb/verdict-kind [{:verdict "b-better" :decidable? true}
                               {:verdict "no-difference" :decidable? true}
                               {:verdict "no-difference" :decidable? false}]))))

(deftest p-values-print-as-the-report-prints-them
  (is (= "0.013" (lb/p-label 0.012939453125)))
  (is (= "1.0" (lb/p-label 1.0)))
  (is (= "< 0.0001" (lb/p-label 1.0e-6)))
  (is (= "—" (lb/p-label nil))))

(deftest the-policy-names-the-family-holm-covers
  (testing "every pair, one comparison: no correction to mention"
    (is (= "Every pair of candidates. A difference is a p below 0.05."
           (lb/policy-note {:policy "all-pairs" :alpha 0.05 :family-size 1 :comparisons [{}]}))))
  (testing "against the best: Holm over every pair, the others not shown"
    (is (= (str "Each candidate against the one with the highest pass rate, sol (the other 10 pairs are "
                "not shown). A difference is a p below 0.05, after Holm's correction for all 15 pairs "
                "(choosing the best looked at all of them).")
           (lb/policy-note {:policy "against-best" :baseline "workflow/sol" :pairs-omitted 10
                            :alpha 0.05 :family-size 15 :comparisons (repeat 5 {})})))))

(deftest exclusions-and-repetitions
  (let [paired {:comparisons [{:a "a" :b "b" :excluded-cases 1 :excluded-cells 0 :repetitions 1}
                              {:a "a" :b "c" :excluded-cases 0 :excluded-cells 0 :repetitions 3}]}]
    (is (= [{:a "a" :b "b" :cases 1 :cells 0}] (lb/exclusions paired)))
    (is (true? (lb/repetitions? paired)))
    (is (false? (lb/repetitions? {:comparisons [{:repetitions 1}]})))))

(deftest figures
  (is (= "42,236" (lb/tokens-label 42236)))
  (is (= "236" (lb/tokens-label 236)))
  (is (= "1,000,000" (lb/tokens-label 1000000)))
  (is (= "12 s" (lb/seconds-label 12.4)))
  (is (= "3.5 min" (lb/seconds-label 210)))
  (is (= "haiku-0" (lb/candidate-label "workflow/haiku-0")))
  (is (= "plain" (lb/candidate-label "plain"))))
