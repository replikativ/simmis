(ns is.simm.benchmarks.discovery-artifacts-test
  (:require [clojure.test :refer [deftest is]] [is.simm.ops.checkers :as checkers]))
(def checker (slurp "examples/workflows/legal-matter-normalized/discovery-checker.clj"))
(def gold {:matter-id "synthetic" :question-ids ["Q1" "Q2"]
           :documents {"D1" {:lines ["Account assigned to Mina." "Physical operator is not identified."]}
                       "D2" {:lines ["The office is in Paris."]}}})
(def citation {:document "D1" :line-start 1 :line-end 1 :quote "Account assigned to Mina."})
(def answer {:version 1 :matter-id "synthetic" :phase :initial
             :answers [{:id "Q1" :status :answered :answer "The account is assigned to Mina."
                        :basis :evidence :evidence [citation]}
                       {:id "Q2" :status :unanswerable :answer "No evidence identifies the physical operator."
                        :basis :whole-packet :evidence []}]})
(defn score [a]
  (checkers/run-checker checker {:gold gold :files {"/out/answers.edn" (pr-str a)
                                                  "/out/discovery.md" (apply str (repeat 4 "Format witness; not semantic certification. "))}}))
(deftest unknowns-do-not-force-irrelevant-quotes
  (is (= 1.0 (:reward (score answer))))
  (is (= 1.0 (:reward (score (update answer :answers #(mapv (fn [a] (dissoc a :basis)) %)))))
      "Evidence/status determine basis without redundant required labels")
  (is (= 1.0 (:reward (score (assoc-in answer [:answers 0 :answer] "Mina physically used it."))))
      "An unsupported inference needs semantic rejection, not invented lexical matching")
  (is (= 1.0 (:reward (score (assoc-in answer [:answers 0 :evidence 0]
                                                     {:document "D2" :line-start 1 :line-end 1 :quote "office is in Paris"}))))
      "A real irrelevant quotation passes provenance, not semantic grounding"))
(deftest duplicate-questions-and-forged-or-mislocated-quotes-refuse
  (doseq [[k bad] [[:question-coverage? (assoc-in answer [:answers 1 :id] "Q1")]
                  [:question-coverage? (update answer :answers pop)]
                  [:answer-contract? (assoc-in answer [:answers 0 :evidence] [])]
                  [:answer-contract? (assoc-in answer [:answers 0 :basis] :whole-packet)]
                  [:answer-contract? (assoc-in answer [:answers 1 :basis] :evidence)]
                  [:source-quotations-valid? (assoc-in answer [:answers 0 :evidence 0 :line-start] 2)]
                  [:source-quotations-valid? (assoc-in (assoc-in answer [:answers 0 :evidence 0 :line-start] 2)
                                                       [:answers 0 :evidence 0 :line-end] 2)]
                  [:source-quotations-valid? (assoc-in answer [:answers 0 :evidence 0 :document] "D99")]
                  [:source-quotations-valid? (assoc-in answer [:answers 0 :evidence 0 :quote] "Account assigned to Alex.")]]]
    (is (false? (get-in (score bad) [:checks k])))
    (is (zero? (:reward (score bad))))))
