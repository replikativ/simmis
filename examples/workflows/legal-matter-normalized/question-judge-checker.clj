(ns question-judgement (:require [clojure.edn :as edn] [clojure.string :as str]))
;; Contract v3: subject-id may be omitted; the host binds it only after checking
;; the full trusted packet hash. Any supplied subject-id must agree, even nil.
;; Provenance and contract checks only. Genuine excerpts can still be irrelevant.
(defn text? [x n] (and (string? x) (<= 1 (count x) n) (not (str/blank? x))))
(defn read-one [raw]
  (try
    (when (and (string? raw) (<= (count raw) 131072))
      (with-in-str raw
        (let [eof (volatile! nil) opts {:eof eof :readers {} :default (fn [& _] (throw (ex-info "tag" {})))}
              x (edn/read opts *in*)]
          (when (identical? eof (edn/read opts *in*)) x))))
    (catch Exception _ nil)))
(defn packet? [gold]
  (and (text? (:packet-id gold) 100) (text? (:subject-id gold) 100)
       (map? (:targets gold)) (<= 1 (count (:targets gold)) 50)
       (every? (fn [[id t]] (and (text? id 100) (text? (:question t) 8000) (text? (:answer-text t) 16000)
                                 (contains? #{true false} (:allow-source-absence? t))
                                 (or (not (contains? t :submitted-answer))
                                     (and (map? (:submitted-answer t)) (= id (get-in t [:submitted-answer :id]))
                                          (= (:answer-text t) (get-in t [:submitted-answer :answer])))))) (:targets gold))
       (map? (:documents gold)) (seq (:documents gold))
       (every? (fn [[id d]] (and (text? id 100) (vector? (:lines d)) (seq (:lines d))
                                 (every? string? (:lines d)))) (:documents gold))))
(def finding-verdict {:supported :pass :contradiction :fail :unsupported :fail :omission :fail :uncertain :unassessable})
(defn row? [r]
  (and (map? r) (every? #{:id :verdict :finding :reason :candidate-quotes :source-quotes} (keys r))
       (every? #(contains? r %) [:id :finding :reason :candidate-quotes :source-quotes])
       (text? (:id r) 100) (text? (:reason r) 4000)
       (contains? finding-verdict (:finding r))
       (or (not (contains? r :verdict)) (= (:verdict r) (get finding-verdict (:finding r))))
       (vector? (:candidate-quotes r)) (<= (count (:candidate-quotes r)) 4)
       (every? #(text? % 4000) (:candidate-quotes r))
       (vector? (:source-quotes r)) (<= (count (:source-quotes r)) 8)
       (case (:finding r)
         :supported (boolean (seq (:candidate-quotes r)))
         :contradiction (boolean (seq (:candidate-quotes r)))
         :unsupported (boolean (seq (:candidate-quotes r)))
         :omission (boolean (seq (:source-quotes r)))
         :uncertain true false)))
(defn source-quote? [documents c]
  (and (map? c) (= #{:document :line-start :line-end :quote} (set (keys c)))
       (text? (:document c) 100) (text? (:quote c) 4000)
       (let [lines (get-in documents [(:document c) :lines]) start (:line-start c) end (:line-end c)]
         (and (vector? lines) (integer? start) (integer? end) (<= 1 start end (count lines))
              (<= (- end start) 20)
              (str/includes? (str/join "\n" (subvec lines (dec start) end)) (:quote c))))))
(defn evidence? [gold row]
  (let [target (get-in gold [:targets (:id row)])]
    (and target
         (every? #(str/includes? (:answer-text target) %) (:candidate-quotes row))
         (every? (partial source-quote? (:documents gold)) (:source-quotes row))
         (or (not= :supported (:finding row)) (seq (:source-quotes row)) (:allow-source-absence? target)))))
(defn check [{:keys [files gold]}]
  (let [a (read-one (get files "/out/assessment.edn"))
        packet? (boolean (packet? gold))
        shape? (and (map? a) (every? #{:version :packet-id :subject-id :criteria} (keys a))
                    (every? #(contains? a %) [:version :packet-id :criteria])
                    (= 1 (:version a)) (vector? (:criteria a)) (<= 1 (count (:criteria a)) 50)
                    (every? row? (:criteria a)))
        bound? (and packet? shape? (= (:packet-id gold) (:packet-id a))
                    (or (not (contains? a :subject-id)) (= (:subject-id gold) (:subject-id a))))
        coverage? (and packet? shape? (= (count (:targets gold)) (count (:criteria a)))
                       (= (set (keys (:targets gold))) (set (map :id (:criteria a)))))
        quotes? (and packet? shape? (every? (partial evidence? gold) (:criteria a)))
        checks {:packet-valid? packet? :assessment-shape? (boolean shape?) :subject-bound? (boolean bound?)
                :criterion-coverage? (boolean coverage?) :allowed-quotes? (boolean quotes?)}]
    {:checks checks :reward (if (every? true? (vals checks)) 1.0 0.0)}))
