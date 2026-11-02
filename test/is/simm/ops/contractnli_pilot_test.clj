(ns is.simm.ops.contractnli-pilot-test
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.test :refer [deftest is]]
            [dvergr.catalog.room :as catalog]
            [is.simm.agents.workflows :as workflows]
            [is.simm.ops.checkers :as checkers])
  (:import [java.security MessageDigest]))

(def root (io/file "examples/workflows/contractnli-eval"))

(defn source []
  (into {} (for [file (file-seq root) :when (.isFile file)]
             [(.toString (.relativize (.toPath root) (.toPath file))) (slurp file)])))

(defn reference [gold]
  {:choice (:choice gold)
   :evidence (mapv (fn [id] {:span-id id :quote (get (:spans gold) id)})
                   (:annotated-span-ids gold))})

(defn evaluate [bundle case-id prediction]
  (checkers/run-checker
   (:checker bundle)
   {:files {"/out/answer.edn" (if (string? prediction) prediction (pr-str prediction))
            "/out/review.md" "Calibration example; inspect retained source evidence."}
    :gold (get-in bundle [:cases case-id :gold])}))

(defn sha256 [text]
  (apply str (map #(format "%02x" (bit-and 255 %))
                  (.digest (MessageDigest/getInstance "SHA-256") (.getBytes ^String text "UTF-8")))))

(deftest grounded-pack-is-ordinary-runnable-and-calibrated
  (let [files (source) bundle (catalog/bundle "contractnli-eval" files)
        inspection (workflows/inspect-files files)]
    (is (:ok inspection))
    (is (:benchmark-ready? inspection))
    (is (false? (:checker-executed? inspection)))
    (is (:ok? (checkers/calibrate bundle)))
    (is (= 4 (count (:cases bundle))))
    (is (empty? (catalog/check-files (select-keys files ["workflow.edn" "diagram.mmd"]) :program)))
    (doseq [[case-id {:keys [gold]}] (:cases bundle)]
      (let [verdict (evaluate bundle case-id (reference gold))]
        (is (every? true? (vals (:checks verdict))) case-id)
        (is (= 1.0 (:reward verdict)) case-id)))))

(deftest scorer-separates-label-citations-and-annotation-coverage
  (let [bundle (catalog/bundle "contractnli-eval" (source))
        case-id "dev-482-nda-7" gold (get-in bundle [:cases case-id :gold])
        correct (reference gold)
        partial (evaluate bundle case-id (update correct :evidence #(subvec % 0 1)))
        invented (evaluate bundle case-id (assoc-in correct [:evidence 0 :quote] "Invented quotation"))
        wrong (evaluate bundle case-id (assoc correct :choice "Entailment"))]
    (is (true? (get-in partial [:checks :label-correct?])))
    (is (true? (get-in partial [:checks :citations-valid?])))
    (is (false? (get-in partial [:checks :annotation-evidence-complete?])))
    (is (< 0.75 (:reward partial) 1.0))
    (is (false? (get-in invented [:checks :citations-valid?])))
    (is (< (:reward invented) 1.0))
    (is (false? (get-in wrong [:checks :label-correct?])))
    (is (zero? (:reward wrong)))))

(deftest malformed-and-unfaithful-output-is-not-a-checker-crash
  (let [bundle (catalog/bundle "contractnli-eval" (source)) case-id "dev-71-nda-7"
        correct (reference (get-in bundle [:cases case-id :gold]))]
    (doseq [bad ["" "nil" "42" "[1 2]" "{" "#=(System/exit 0)"
                 (str (pr-str correct) " nil")
                 (str (pr-str correct) " {:second :form}")
                 (str (pr-str correct) " trailing")
                 (assoc correct :choice "Unlisted")
                 (assoc correct :evidence "not a vector")
                 (update correct :evidence #(conj % (first %)))]]
      (let [result (evaluate bundle case-id bad)]
        (is (false? (get-in result [:checks :output-contract?])) (pr-str bad))
        (is (zero? (:reward result)))))
    (is (= 1.0 (:reward (evaluate bundle case-id (str (pr-str correct) "\n ; trailing comment\n")))))))

(defn codepoint-slice [^String text start end]
  (subs text (.offsetByCodePoints text 0 start) (.offsetByCodePoints text 0 end)))

(deftest frozen-provenance-spans-and-splits-agree
  (let [files (source) provenance (edn/read-string (get files "source-provenance.edn"))
        split (edn/read-string (get files "benchmark.edn"))
        cases (:cases provenance)
        ids (into {} (map (juxt :case-id :document-id) cases))
        dev (set (map ids (get-in split [:splits :development])))
        held (set (map ids (get-in split [:splits :held-out])))]
    (is (empty? (set/intersection dev held)))
    (is (= #{} (set/difference (set (keys ids))
                             (set (mapcat val (:splits split))))))
    (doseq [{:keys [case-id document-sha256 fixture-sha256 gold-sha256]} cases]
      (let [fixture-source (get files (str "cases/" case-id "/fixtures/inputs/case.edn"))
            gold-source (get files (str "cases/" case-id "/gold.edn"))
            fixture (edn/read-string fixture-source) gold (edn/read-string gold-source)]
        (is (= fixture-sha256 (sha256 fixture-source)))
        (is (= gold-sha256 (sha256 gold-source)))
        (is (= document-sha256 (sha256 (:document fixture))))
        (is (not (contains? fixture :choice)))
        (is (not (contains? fixture :annotated-span-ids)))
        (doseq [{:keys [id text start-codepoint end-codepoint]} (:spans fixture)]
          (is (= text (codepoint-slice (:document fixture) start-codepoint end-codepoint)))
          (is (= text (get (:spans gold) id))))))
    ;; Non-BMP characters must not be split using Python offsets as UTF-16.
    (is (= "😀" (codepoint-slice "A😀B" 1 2)))))

(deftest fabricated-citations-do-not-disappear-from-evidence-precision
  (let [bundle (catalog/bundle "contractnli-eval" (source))
        case-id "dev-482-nda-7"
        correct (reference (get-in bundle [:cases case-id :gold]))
        extra {:span-id 999999 :quote "Invented quotation"}
        result (evaluate bundle case-id (update correct :evidence conj extra))
        unknown-id "dev-456-nda-7"
        unknown (reference (get-in bundle [:cases unknown-id :gold]))
        fabricated (evaluate bundle unknown-id (assoc unknown :evidence [extra]))]
    (is (true? (get-in result [:checks :output-contract?])))
    (is (false? (get-in result [:checks :citations-valid?])))
    (is (false? (get-in result [:checks :annotation-evidence-only?])))
    (is (< (:reward result) 0.75))
    (is (= "NotMentioned" (:choice unknown)))
    (is (false? (get-in fabricated [:checks :citations-valid?])))
    (is (false? (get-in fabricated [:checks :annotation-evidence-only?])))
    (is (= 0.5 (:reward fabricated)))))
