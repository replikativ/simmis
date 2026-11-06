(ns is.simm.benchmarks.matter-artifacts-test
  (:require [clojure.test :refer [deftest is]]
            [is.simm.ops.checkers :as checkers]))
(def checker (slurp "examples/workflows/legal-matter-normalized/checker.clj"))
(def gold {:documents {"source.docx" {"word/document.xml" {"p1" "Payment is due in 30 days." "p2" "The office is in Paris."}}}})
(def citation {:document "source.docx" :part "word/document.xml" :id "p1" :quote "30 days"})
(def issue {:id "issue-1" :title "Payment" :priority :high :recommendation "Inspect payment terms." :evidence [citation]})
(defn score [tracker]
  (checkers/run-checker checker {:gold gold :files {"/out/red-flag-memo.md" (apply str (repeat 5 "A structural witness, not a legal assessment. "))
                                                  "/out/red-flag-tracker.edn" (if (string? tracker) tracker (pr-str tracker))}}))
(deftest artifact-validity-does-not-certify-legal-issue-coverage
  (is (= 1.0 (:reward (score {:version 1 :issues [issue]}))))
  (is (= 1.0 (:reward (score {:version 1 :issues []}))) "Empty analysis is format-valid, never semantic success")
  (let [irrelevant (assoc-in issue [:evidence 0] (assoc citation :id "p2" :quote "office is in Paris"))]
    (is (true? (get-in (score {:version 1 :issues [irrelevant]}) [:checks :tracker-quotations-valid?]))
        "A real irrelevant quote must be rejected by semantic grading, not misclassified as fabricated")))
(deftest malformed-duplicate-and-substituted-outputs-fail-without-verifier-exceptions
  (doseq [bad ["{} nil" "nil" "[]" "{:version 1 :issues 42}" {:version 1 :issues [issue issue]}]]
    (is (false? (get-in (score bad) [:checks :tracker-structure?]))))
  (doseq [bad [(assoc citation :quote "90 days") (assoc citation :id "p2")
               (assoc citation :part "word/footer1.xml") (assoc citation :document "foreign.docx")]]
    (let [verdict (score {:version 1 :issues [(assoc issue :evidence [bad])]})]
      (is (false? (get-in verdict [:checks :tracker-quotations-valid?])))
      (is (zero? (:reward verdict))))))
