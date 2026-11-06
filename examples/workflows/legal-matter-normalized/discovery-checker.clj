(ns discovery-artifacts (:require [clojure.edn :as edn] [clojure.string :as str]))
(defn text? [x n] (and (string? x) (<= 1 (count x) n) (not (str/blank? x))))
(defn read-one [raw]
  (try
    (when (and (string? raw) (<= (count raw) 131072))
      (with-in-str raw
        (let [eof (volatile! nil) opts {:eof eof :readers {} :default (fn [& _] (throw (ex-info "tag" {})))}
              value (edn/read opts *in*)]
          (when (identical? eof (edn/read opts *in*)) value))))
    (catch Exception _ nil)))
;; Contract v2 derives basis from citations/status. A supplied basis must agree;
;; its omission adds no information and must not turn a valid answer into failure.
(defn answer? [a]
  (and (map? a) (every? #{:id :status :answer :basis :evidence} (keys a))
       (every? #(contains? a %) [:id :status :answer :evidence])
       (text? (:id a) 100) (text? (:answer a) 8000)
       (contains? #{:answered :partly-answered :unanswerable} (:status a))
       (vector? (:evidence a)) (<= (count (:evidence a)) 8)
       (if (seq (:evidence a))
         (or (not (contains? a :basis)) (= :evidence (:basis a)))
         (and (= :unanswerable (:status a))
              (or (not (contains? a :basis)) (= :whole-packet (:basis a)))))))
(defn citation? [documents c]
  (and (map? c) (= #{:document :line-start :line-end :quote} (set (keys c)))
       (text? (:document c) 100) (text? (:quote c) 4000)
       (let [lines (get-in documents [(:document c) :lines]) start (:line-start c) end (:line-end c)]
         (and (vector? lines) (integer? start) (integer? end) (<= 1 start end (count lines))
              (<= (- end start) 20)
              (str/includes? (str/join "\n" (subvec lines (dec start) end)) (:quote c))))))
(defn check [{:keys [files gold]}]
  (let [a (read-one (get files "/out/answers.edn"))
        gold? (and (text? (:matter-id gold) 100) (vector? (:question-ids gold))
                   (<= 1 (count (:question-ids gold)) 20) (every? #(text? % 100) (:question-ids gold))
                   (= (count (:question-ids gold)) (count (set (:question-ids gold))))
                   (map? (:documents gold)) (seq (:documents gold)))
        shape? (and gold? (map? a) (= #{:version :matter-id :phase :answers} (set (keys a)))
                    (= 1 (:version a)) (= (:matter-id gold) (:matter-id a)) (= :initial (:phase a))
                    (vector? (:answers a)) (<= (count (:answers a)) 20) (every? answer? (:answers a)))
        complete? (and shape? (= (count (:question-ids gold)) (count (:answers a)))
                       (= (set (:question-ids gold)) (set (map :id (:answers a)))))
        quotes? (and shape? (every? (partial citation? (:documents gold)) (mapcat :evidence (:answers a))))
        report? (and (text? (get files "/out/discovery.md") 131072) (>= (count (get files "/out/discovery.md")) 80))
        checks {:answer-contract? (boolean shape?) :question-coverage? (boolean complete?)
                :source-quotations-valid? (boolean quotes?) :report-present? (boolean report?)}]
    {:checks checks :reward (if (every? true? (vals checks)) 1.0 0.0)}))
