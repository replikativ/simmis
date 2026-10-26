(ns is.simm.ops.wiki-checkpoint-scoring-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [is.simm.ops.checkers :as checkers]
            [is.simm.ops.knowledge-inputs :as inputs]
            [is.simm.ops.wiki-scoring :as scoring]
            [is.simm.ops.wiki-scoring-test :as fixture]))

(defn- bundle []
  (-> (fixture/bundle)
      (assoc-in [:definition :params :simmis/checker-profile] (:id scoring/checkpoint-profile))
      (update :checker str/replace ":wiki-structure-v1" ":wiki-checkpoint-structure-v1")
      (assoc :fixtures (assoc (fixture/frozen {"inbox/note.md" fixture/source})
                              "/wiki/Company.md" fixture/original))
      (assoc-in [:cases "no-change" :fixtures] {"/wiki/Company.md" fixture/changed})
      (assoc-in [:cases "unselected"]
                {:fixtures {"/wiki/Private.md" fixture/fresh}
                 :gold (get-in (fixture/bundle) [:cases "supported" :gold])})))

(defn- observed [b case]
  (scoring/prepare-input b inputs/default-policy
                         {:environment/metadata {:case case}} (fn [] nil) {:files {}}))

(deftest checkpoint-context-is-a-case-specific-baseline-and-not-new-evidence
  (let [b (bundle) supported (:structure (observed b "supported"))
        no-change (:structure (observed b "no-change"))]
    (is (= scoring/checkpoint-profile (:profile supported)))
    (is (= fixture/original (get-in supported [:baseline-texts "wiki/Company.md"])))
    (is (= fixture/changed (get-in no-change [:baseline-texts "wiki/Company.md"])))
    (is (not (contains? (:baseline-texts supported) "wiki/Private.md")))
    (is (= [fixture/source-path] (:source-paths supported)))
    (is (= {fixture/source-path fixture/source} (:source-texts supported)))
    (is (not (contains? (:source-texts supported) "/wiki/Company.md")))))

(deftest actual-checker-preserves-the-checkpoint-wiki-and-old-profile-is-unchanged
  (let [b (bundle) output {"/out/wiki/Company.md" fixture/changed}]
    (is (every? true? (vals (:checks (fixture/verdict b "supported" output)))))
    (is (false? (get-in (fixture/verdict b "supported"
                                         {"/out/wiki/Company.md" (str/replace fixture/changed "revenue: 0" "revenue: 1")})
                        [:checks :preserved-structure?])))
    (is (every? true? (vals (:checks (fixture/verdict b "no-change" {})))))
    (let [old (assoc-in b [:definition :params :simmis/checker-profile] :wiki-structure-v1)]
      (is (not (contains? (get-in (observed old "supported") [:structure :baseline-texts]) "wiki/Company.md"))))
    (let [old (assoc-in b [:definition :params :simmis/checker-profile] :wiki-structure-v1)
          actual (get-in (checkers/evaluator b inputs/default-policy) [:ref :verifier/basis])
          prior (get-in (checkers/evaluator old inputs/default-policy) [:ref :verifier/basis])]
      (is (= scoring/checkpoint-profile (:observation-profile actual)))
      (is (= scoring/profile (:observation-profile prior)))
      (is (not= actual prior)))
    (is (every? true? (vals (:checks (fixture/verdict b "supported" {"/out/wiki/Product.md" fixture/fresh})))))
    (is (false? (get-in (fixture/verdict b "supported"
                                         {"/out/wiki/Product.md" (str/replace fixture/fresh fixture/other-page fixture/page)})
                        [:checks :unique-identities?])))))

(deftest malformed-conflicting-and-oversized-wiki-baselines-refuse
  (let [b (bundle)]
    (doseq [fixtures [(assoc (:fixtures b) "/wiki/../Company.md" fixture/original)
                      (assoc (:fixtures b) "/wiki/Company.md" nil)
                      (assoc (:fixtures b) "/wiki/Duplicate.md" fixture/original)
                      (assoc (:fixtures b) "/wiki/Company.md" (apply str (repeat 524289 "a")))
                      (merge (:fixtures b) (fixture/frozen {"inbox/note.md" fixture/source
                                                            "wiki/Company.md" fixture/changed}))]]
      (is (thrown? clojure.lang.ExceptionInfo (observed (assoc b :fixtures fixtures) "supported"))))))

(deftest calibration-and-scoring-use-the-same-checkpoint-observation
  (let [b (assoc (bundle) :calibration
                 {:version 2
                  :references {:update {:case "supported" :files {"/out/wiki/Company.md" fixture/changed}}}
                  :damaged {:metadata {:case "supported"
                                       :files {"/out/wiki/Company.md" (str/replace fixture/changed "active: false" "active: true")}
                                       :loses [:preserved-structure?]}}})]
    (is (:ok? (checkers/calibrate b checkers/calibration-policy inputs/default-policy)))))

(deftest wiki-context-never-substitutes-for-trigger-quote-evidence
  (let [b (update (bundle) :fixtures merge
                  (fixture/frozen {"inbox/note.md" fixture/source "wiki/Company.md" fixture/original}))
        structure (:structure (observed b "supported"))
        quoted (assoc-in b [:cases "supported" :gold :quotes]
                         [{:source "/in/knowledge/sources/wiki/Company.md" :text "# Company"}])]
    (is (= [fixture/source-path] (:source-paths structure)))
    (is (= {fixture/source-path fixture/source} (:source-texts structure)))
    (is (false? (get-in (fixture/verdict quoted "supported" {"/out/wiki/Company.md" fixture/changed}) [:checks :quotes?])))))
