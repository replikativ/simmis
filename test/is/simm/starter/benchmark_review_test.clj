(ns is.simm.starter.benchmark-review-test
  (:require [clojure.edn :as edn]
            [clojure.test :refer [deftest is]]
            [dvergr.catalog.room :as catalog]
            [is.simm.ops.checkers :as checkers]
            [is.simm.starter.benchmark-review :as review]))

(defn source-bundle [] (catalog/bundle "benchmark-review" (review/source)))
(defn reference [bundle]
  (edn/read-string (get-in bundle [:calibration :reference "/out/review.edn"])))
(defn grade [bundle value]
  (checkers/run-checker (:checker bundle)
                        {:gold (:gold bundle) :files {"/out/review.edn" (pr-str value)}}))

(deftest starter-is-inert-and-calibrated
  (let [files (review/source)
        bundle (source-bundle)]
    (is (= 1 review/version))
    (is (empty? (catalog/check-files files :program)))
    (is (:ok? (checkers/calibrate bundle)))
    (is (= 1.0 (:reward (grade bundle (reference bundle)))))
    (is (= #{"/inputs/review-packet.edn"} (set (keys (:fixtures bundle)))))
    (is (not (contains? (:definition bundle) :coordinator-benchmark)))))

(deftest exact-packet-and-case-coverage-are-required
  (let [bundle (source-bundle)
        value (reference bundle)]
    (doseq [bad [(assoc value :packet-id "another-packet")
                 (update value :cases #(conj % (first %)))
                 (assoc-in value [:cases 0 :id] "foreign-case")
                 (assoc value :cases [])]]
      (is (false? (get-in (grade bundle bad) [:checks :review-case-binding?]))))))

(deftest faithful-quotations-require-the-named-case-local-source
  (let [bundle (source-bundle)
        value (reference bundle)]
    (doseq [bad [(assoc-in value [:cases 0 :findings 0 :citations 0 :quote] "Tuesday")
                 (assoc-in value [:cases 0 :findings 0 :citations 0 :source-id] "foreign-source")]]
      (is (false? (get-in (grade bundle bad) [:checks :review-citations?]))))
    (is (false? (get-in (grade bundle (assoc-in value [:cases 0 :findings 0 :citations] []))
                       [:checks :review-structure?])))))

(deftest malformed-packet-identities-cannot-collapse-into-a-passing-reference
  (let [bundle (source-bundle)
        value (reference bundle)
        gold (:gold bundle)]
    (doseq [bad [(update gold :cases #(conj % (first %)))
                 (update-in gold [:cases 0 :sources] #(conj % (first %)))
                 (assoc-in gold [:cases 0 :id] " ")
                 (assoc-in gold [:cases 0 :sources 0 :text] 42)]]
      (let [result (checkers/run-checker (:checker bundle) {:gold bad :files {"/out/review.edn" (pr-str value)}})]
        (is (false? (get-in result [:checks :review-case-binding?])))
        (is (false? (get-in result [:checks :review-citations?])))))))

(deftest a-report-cannot-claim-applied-grading-or-skip-human-adjudication
  (let [bundle (source-bundle)
        value (reference bundle)]
    (is (false? (get-in (grade bundle (assoc value :grading-changes-applied true))
                       [:checks :advisory-only?])))
    (doseq [target [:reference :grading-policy]]
      (let [changed (assoc-in value [:cases 0 :proposed-change :target] target)]
        (is (false? (get-in (grade bundle changed) [:checks :advisory-only?])))
        (is (true? (get-in (grade bundle (assoc-in changed [:cases 0 :next-action] :human-adjudication))
                          [:checks :advisory-only?])))))))

(deftest malformed-or-unbounded-review-output-is-refused
  (let [bundle (source-bundle)
        value (reference bundle)]
    (doseq [raw ["{} nil" "#unknown/tag {}" "[" "42" (apply str (repeat 65537 "x"))]]
      (is (false? (get-in (checkers/run-checker (:checker bundle)
                                              {:gold (:gold bundle) :files {"/out/review.edn" raw}})
                          [:checks :review-structure?]))))
    (doseq [bad [(assoc value :approved true)
                 (assoc-in value [:cases 0 :findings 0 :confidence] :certain)
                 (assoc-in value [:cases 0 :findings 0 :summary] (apply str (repeat 2001 "x")))]]
      (is (false? (get-in (grade bundle bad) [:checks :review-structure?]))))))

(deftest structurally-valid-abstention-is-not-semantic-certification
  (let [bundle (source-bundle)
        value (-> (reference bundle)
                  (assoc-in [:cases 0 :findings] [{:kind :insufficient-context :confidence :low
                                                 :summary "Request independent adjudication." :citations []}])
                  (assoc-in [:cases 0 :next-action] :collect-evidence)
                  (assoc-in [:cases 0 :proposed-change] nil))
        result (grade bundle value)]
    (is (= 1.0 (:reward result)))
    (is (= #{:review-structure? :review-case-binding? :review-citations? :advisory-only?}
           (set (keys (:checks result)))))
    (is (not (contains? (:checks result) :semantically-correct?)))))
