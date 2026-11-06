(ns matter-judgement (:require [clojure.edn :as edn] [clojure.string :as str]))

;; Contract v2 permits up to eight quotations for multi-conflict criteria.
;; This checks assessment provenance and completeness, never whether a legal
;; verdict is right. In particular, a complete all-fail assessment can pass.
(defn text? [x n] (and (string? x) (<= 1 (count x) n) (not (str/blank? x))))
(defn read-one [raw]
  (try
    (when (and (string? raw) (<= (count raw) 131072))
      (with-in-str raw
        (let [eof (volatile! nil)
              opts {:eof eof :readers {} :default (fn [& _] (throw (ex-info "tag" {})))}
              value (edn/read opts *in*)]
          (when (identical? eof (edn/read opts *in*)) value))))
    (catch Exception _ nil)))
(defn row? [row]
  (and (map? row) (= #{:id :verdict :reason :basis :evidence} (set (keys row)))
       (text? (:id row) 100) (contains? #{:pass :fail :unassessable} (:verdict row))
       (text? (:reason row) 2000) (contains? #{:excerpt :whole-output} (:basis row))
       (vector? (:evidence row)) (<= (count (:evidence row)) 8)
       (if (= :excerpt (:basis row)) (seq (:evidence row)) (empty? (:evidence row)))))
(defn quotation? [allowed c]
  (and (map? c) (= #{:path :quote} (set (keys c)))
       (text? (:path c) 512) (text? (:quote c) 2000)
       (let [source (get allowed (:path c))]
         (and (string? source) (str/includes? source (:quote c))))))
(defn packet? [gold]
  (and (map? gold) (text? (:packet-id gold) 100) (text? (:subject-id gold) 100)
       (vector? (:criterion-ids gold)) (<= 1 (count (:criterion-ids gold)) 50)
       (every? #(text? % 100) (:criterion-ids gold))
       (= (count (:criterion-ids gold)) (count (set (:criterion-ids gold))))
       (map? (:allowed-files gold)) (<= 1 (count (:allowed-files gold)) 2)
       (every? (fn [[path content]] (and (text? path 512) (text? content 131072))) (:allowed-files gold))))
(defn check [{:keys [files gold]}]
  (let [answer (read-one (get files "/out/assessment.edn"))
        packet? (packet? gold)
        shape? (and (map? answer)
                    (= #{:version :packet-id :subject-id :criteria} (set (keys answer)))
                    (= 1 (:version answer)) (vector? (:criteria answer))
                    (<= 1 (count (:criteria answer)) 50) (every? row? (:criteria answer)))
        bound? (and packet? shape? (= (:packet-id gold) (:packet-id answer))
                    (= (:subject-id gold) (:subject-id answer)))
        coverage? (and packet? shape? (= (count (:criteria answer)) (count (:criterion-ids gold)))
                       (= (set (:criterion-ids gold)) (set (map :id (:criteria answer)))))
        quotes? (and packet? shape? (every? (partial quotation? (:allowed-files gold))
                                   (mapcat :evidence (:criteria answer))))
        checks {:packet-valid? (boolean packet?) :assessment-shape? (boolean shape?) :subject-bound? (boolean bound?)
                :criterion-coverage? (boolean coverage?) :allowed-quotes? (boolean quotes?)}]
    {:checks checks :reward (if (every? true? (vals checks)) 1.0 0.0)}))
