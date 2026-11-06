(ns is.simm.benchmarks.question-judgement-test
  (:require [clojure.test :refer [deftest is]] [is.simm.ops.checkers :as checkers]
            [is.simm.benchmarks.matter-grading :as grading] [hasch.core :as hasch]))
(def checker (slurp "examples/workflows/legal-matter-normalized/question-judge-checker.clj"))
(def gold {:packet-id "p" :subject-id "s"
           :targets {"Q1" {:question "Did consent precede transfer?" :answer-text "Yes. Consent was at 12:00; transfer was at 14:00." :allow-source-absence? false}
                     "Q2" {:question "What losses resulted?" :answer-text "Loss cannot be established from the packet." :allow-source-absence? true}}
           :documents {"D1" {:lines ["Consent at 12:00." "Transfer at 14:00."]}}})
(def source {:document "D1" :line-start 1 :line-end 1 :quote "Consent at 12:00."})
(def answer {:version 1 :packet-id "p" :subject-id "s"
             :criteria [{:id "Q1" :verdict :pass :finding :supported :reason "Correct times and ordering."
                         :candidate-quotes ["Yes. Consent was at 12:00; transfer was at 14:00."] :source-quotes [source]}
                        {:id "Q2" :verdict :pass :finding :supported :reason "Retains the absence of loss evidence."
                         :candidate-quotes ["Loss cannot be established"] :source-quotes []}]})
(defn score [a] (checkers/run-checker checker {:gold gold :files {"/out/assessment.edn" (pr-str a)}}))
(deftest matching-answer-and-source-have-separate-provenance
  (is (= 1.0 (:reward (score answer))))
  (let [compact (update answer :criteria #(mapv (fn [r] (dissoc r :verdict)) %))]
    (is (= 1.0 (:reward (score compact))))
    (is (= answer (grading/with-derived-verdicts compact))))
  (doseq [[k bad] [[:assessment-shape? (assoc-in answer [:criteria 0 :candidate-quotes] [])]
                  [:allowed-quotes? (assoc-in answer [:criteria 0 :candidate-quotes] ["Loss cannot be established"])]
                  [:allowed-quotes? (assoc-in answer [:criteria 0 :source-quotes] [])]
                  [:allowed-quotes? (assoc-in answer [:criteria 0 :source-quotes 0 :quote] "Consent at 10:00.")]
                  [:criterion-coverage? (assoc-in answer [:criteria 1 :id] "Q1")]
                  [:subject-bound? (assoc answer :subject-id "other")]]]
    (is (false? (get-in (score bad) [:checks k])))
    (is (zero? (:reward (score bad))))))
(deftest omissions-and-internal-contradictions-do-not-force-invented-quotes
  (is (= 1.0 (:reward (score (assoc-in answer [:criteria 0]
                                                {:id "Q1" :verdict :fail :finding :omission :reason "Missing required chronology."
                                                 :candidate-quotes [] :source-quotes [source]})))))
  (is (= 1.0 (:reward (score (assoc-in answer [:criteria 0]
                                                {:id "Q1" :verdict :fail :finding :contradiction :reason "Illustrative finding, not a semantic witness."
                                                 :candidate-quotes ["Yes."] :source-quotes []})))))
  (is (= 1.0 (:reward (score (assoc-in answer [:criteria 0 :candidate-quotes] ["Consent was at 12:00"]))))
      "A true fragment can still conceal a contradiction: semantic controls must detect that")
  (is (zero? (:reward (score (assoc-in answer [:criteria 0 :finding] :omission))))))

(deftest trusted-targets-and-finding-normalization-are-explicit
  (doseq [g [(assoc-in gold [:targets "Q1" :question] "")
             (assoc-in gold [:targets "Q1" :submitted-answer] {:answer "A different answer"})
             (assoc-in gold [:targets "Q1" :submitted-answer] {:id "Q2" :answer (get-in gold [:targets "Q1" :answer-text])})
             (assoc-in gold [:documents "D1" :lines] [42])]]
    (is (false? (get-in (checkers/run-checker checker {:gold g :files {"/out/assessment.edn" (pr-str answer)}})
                       [:checks :packet-valid?]))))
  (doseq [[finding verdict] grading/finding-verdict]
    (is (= verdict (get-in (grading/with-derived-verdicts {:criteria [{:finding finding}]}) [:criteria 0 :verdict]))))
  (is (thrown? clojure.lang.ExceptionInfo
               (grading/with-derived-verdicts {:criteria [{:finding :unsupported :verdict :pass}]}))))

(deftest host-subject-projection-requires-full-packet-and-valid-checks
  (let [body (dissoc gold :packet-id) packet (assoc body :packet-id (str (hasch/uuid body)))
        raw (-> answer (assoc :packet-id (:packet-id packet)) (dissoc :subject-id))
        result (checkers/run-checker checker {:gold packet :files {"/out/assessment.edn" (pr-str raw)}})
        projected (grading/bind-question-assessment packet raw (:checks result))]
    (is (= 1.0 (:reward result)))
    (is (= "s" (:subject-id projected)))
    (is (not (contains? raw :subject-id)) "Raw model output remains untouched")
    (is (= (grading/with-derived-verdicts (assoc raw :subject-id "s"))
           (grading/bind-question-assessment packet (assoc raw :subject-id "s") (:checks result))))
    (is (thrown? clojure.lang.ExceptionInfo
                 (grading/bind-question-assessment packet (assoc raw :packet-id "foreign-packet") (:checks result))))
    (doseq [bad [nil "foreign" 42]]
      (let [a (assoc raw :subject-id bad)
            checked (checkers/run-checker checker {:gold packet :files {"/out/assessment.edn" (pr-str a)}})]
        (is (false? (get-in checked [:checks :subject-bound?])))
        (is (thrown? clojure.lang.ExceptionInfo (grading/bind-question-assessment packet a (:checks checked))))))
    (is (thrown? clojure.lang.ExceptionInfo (grading/bind-question-assessment (assoc packet :extra "changed") raw (:checks result))))
    (is (thrown? clojure.lang.ExceptionInfo (grading/bind-question-assessment packet raw (assoc (:checks result) :allowed-quotes? false)))))
  (is (false? (get-in (score (dissoc answer :packet-id)) [:checks :subject-bound?]))))
