(ns is.simm.benchmarks.matter-grading
  "Pure aggregation of independently scoped assessment batches.
   No inference, accounting, authority or rewriting of original Scorecards."
  (:require [hasch.core :as hasch]))

(def required-checks
  #{:packet-valid? :assessment-shape? :subject-bound? :criterion-coverage? :allowed-quotes?})

(def finding-verdict
  {:supported :pass :contradiction :fail :unsupported :fail :omission :fail :uncertain :unassessable})

(defn with-derived-verdicts
  "Normalize a structurally validated question assessment for aggregation.
   A supplied verdict must agree with its finding; raw receipts stay intact."
  [assessment]
  (update assessment :criteria
          (fn [rows]
            (mapv (fn [row]
                    (let [verdict (get finding-verdict (:finding row))]
                      (when-not (and verdict (or (not (contains? row :verdict)) (= verdict (:verdict row))))
                        (throw (ex-info "Inconsistent assessment finding/verdict" {:type ::invalid-finding})))
                      (assoc row :verdict verdict))) rows))))

(defn- nonblank? [x] (and (string? x) (boolean (re-find #"\S" x))))

(defn bind-question-assessment
  "Project a question assessment after trusted structural/provenance checks.
   Verify the FULL packet-body hash, then fill only absent host-owned metadata.
   This does not certify a new native Attempt or change a historical Scorecard."
  [packet assessment checks]
  (when-not (and (= required-checks (set (keys checks))) (every? true? (vals checks))
                 (= (:packet-id packet) (str (hasch/uuid (dissoc packet :packet-id))))
                 (= (:packet-id packet) (:packet-id assessment)) (nonblank? (:subject-id packet))
                 (or (not (contains? assessment :subject-id)) (= (:subject-id packet) (:subject-id assessment))))
    (throw (ex-info "Question assessment is not bound to its verified packet" {:type ::invalid-packet-binding})))
  (with-derived-verdicts (assoc assessment :subject-id (:subject-id packet))))

(defn aggregate
  "Packets must partition the frozen criterion universe exactly. A missing,
   unsettled, foreign or structurally invalid batch contributes no verdicts.
   :unassessable remains unresolved in the fixed denominator. Checks must come
   from the trusted deterministic checker, not the judge's own declaration."
  [expected-ids packets results]
  (let [ids (mapcat :criterion-ids (vals packets))]
    (when-not (and (vector? expected-ids) (seq expected-ids) (every? nonblank? expected-ids)
                   (= (count expected-ids) (count (set expected-ids)))
                   (map? packets) (seq packets)
                   (every? (fn [p] (and (nonblank? (:packet-id p)) (nonblank? (:subject-id p))
                                        (vector? (:criterion-ids p)) (seq (:criterion-ids p)))) (vals packets))
                   (= 1 (count (set (map :subject-id (vals packets)))))
                   (= (count packets) (count (set (map :packet-id (vals packets)))))
                   (= (count expected-ids) (count ids) (count (set ids)))
                   (= (set expected-ids) (set ids)))
      (throw (ex-info "Assessment packets must partition the frozen criterion universe" {:type ::invalid-partition})))
    (let [batches
          (into (sorted-map)
                (for [[id packet] packets
                      :let [{:keys [completed? settled? checks assessment]} (get results id)
                            rows (:criteria assessment)
                            valid? (and (true? completed?) (true? settled?) (= required-checks (set (keys checks)))
                                        (every? true? (vals checks))
                                        (= (:packet-id packet) (:packet-id assessment))
                                        (= (:subject-id packet) (:subject-id assessment))
                                        (vector? rows)
                                        (= (count rows) (count (:criterion-ids packet)))
                                        (= (set (map :id rows)) (set (:criterion-ids packet)))
                                        (every? #(contains? #{:pass :fail :unassessable} (:verdict %)) rows))]]
                  [id {:usable? (boolean valid?)
                       :criteria (if valid? (into {} (map (juxt :id :verdict) rows))
                                     (zipmap (:criterion-ids packet) (repeat :unresolved)))}]))
          verdicts (into (sorted-map) (mapcat (comp seq :criteria val) batches))
          passed (count (filter #{:pass} (vals verdicts)))
          failed (count (filter #{:fail} (vals verdicts)))
          denominator (count expected-ids)]
      {:denominator denominator :passed passed :failed failed :unresolved (- denominator passed failed)
       :assessed (+ passed failed) :usable-batches (count (filter :usable? (vals batches)))
       :batch-count (count packets) :assessment-complete? (= (+ passed failed) denominator)
       :all-pass? (= passed denominator)
       :criteria verdicts :batches batches})))
