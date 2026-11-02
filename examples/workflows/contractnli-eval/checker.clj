(ns contractnli-checker
  (:require [clojure.edn :as edn]
            [clojure.set :as set]
            [clojure.string :as str]))

(def labels #{"Entailment" "Contradiction" "NotMentioned"})

(defn answer [raw]
  (try
    (when (and (string? raw) (<= (count raw) 65536))
      (with-in-str raw
        (let [eof (volatile! nil)
              value (edn/read {:eof eof} *in*)
              trailing (edn/read {:eof eof} *in*)]
          (when (identical? eof trailing) value))))
    (catch Exception _ nil)))

(defn f1 [faithful submitted expected]
  (let [overlap (count (set/intersection faithful expected))]
    (cond
      (and (empty? submitted) (empty? expected)) 1.0
      (or (empty? submitted) (empty? expected) (zero? overlap)) 0.0
      :else (/ (* 2.0 overlap) (+ (count submitted) (count expected))))))

(defn check [{:keys [files gold]}]
  (let [value (answer (get files "/out/answer.edn"))
        evidence (:evidence value)
        report (get files "/out/review.md")
        shape? (and (map? value) (= #{:choice :evidence} (set (keys value)))
                    (contains? labels (:choice value))
                    (string? report) (<= 1 (count report) 32768) (not (str/blank? report))
                    (vector? evidence) (<= (count evidence) 32)
                    (every? (fn [e]
                              (and (map? e) (= #{:span-id :quote} (set (keys e)))
                                   (integer? (:span-id e)) (<= 0 (:span-id e))
                                   (string? (:quote e)) (<= 1 (count (:quote e)) 8192))) evidence)
                    (= (count evidence) (count (set (map :span-id evidence)))))
        valid-ids (if shape?
                    (into #{} (keep (fn [{:keys [span-id quote]}]
                                     (when (= quote (get (:spans gold) span-id)) span-id))) evidence)
                    #{})
        submitted-ids (if shape? (set (map :span-id evidence)) #{})
        expected (set (:annotated-span-ids gold))
        label? (and shape? (= (:choice gold) (:choice value)))
        citations? (and shape?
                        (= (count evidence) (count valid-ids))
                        (if (= "NotMentioned" (:choice value))
                          (empty? evidence)
                          (seq evidence)))
        overlap? (and shape? (if (empty? expected) (empty? valid-ids)
                                              (seq (set/intersection valid-ids expected))))
        complete? (and shape? (set/subset? expected valid-ids))
        only? (and shape? (set/subset? submitted-ids expected))
        checks {:output-contract? (boolean shape?)
                :label-correct? (boolean label?)
                :citations-valid? (boolean citations?)
                :annotation-evidence-overlap? (boolean overlap?)
                :annotation-evidence-complete? (boolean complete?)
                :annotation-evidence-only? (boolean only?)}]
    ;; A wrong/invalid label never earns task reward. For a correct label,
    ;; preserve partial evidence credit rather than require one exact quote set.
    ;; This is agreement with dataset annotations, not independent entailment.
    {:checks checks
     :reward (if label?
               (+ 0.5 (if citations? 0.25 0.0) (* 0.25 (f1 valid-ids submitted-ids expected)))
               0.0)}))
