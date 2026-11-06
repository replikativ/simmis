(ns benchmark-review-checker
  (:require [clojure.edn :as edn]
            [clojure.string :as str]))

(def kinds #{:execution-fault :format-fault :annotation-disagreement :unsupported-answer
             :evidence-selection :no-issue :insufficient-context})
(def semantic-kinds #{:annotation-disagreement :unsupported-answer :evidence-selection :no-issue})
(def actions #{:retain :repair-workflow :inspect-harness :human-adjudication :collect-evidence})
(defn text? [s maximum] (and (string? s) (<= 1 (count s) maximum) (not (str/blank? s))))
(defn packet? [gold]
  (and (map? gold) (text? (:packet-id gold) 100)
       (vector? (:cases gold)) (<= 1 (count (:cases gold)) 16)
       (= (count (:cases gold)) (count (set (map :id (:cases gold)))))
       (every? (fn [c]
                 (and (map? c) (text? (:id c) 100)
                      (vector? (:sources c)) (<= (count (:sources c)) 100)
                      (= (count (:sources c)) (count (set (map :id (:sources c)))))
                      (every? #(and (map? %) (text? (:id %) 100)
                                    (string? (:text %)) (<= (count (:text %)) 524288)) (:sources c))))
               (:cases gold))))
(defn answer [raw]
  (try
    (when (and (string? raw) (<= (count raw) 65536))
      (with-in-str raw
        (let [eof (volatile! nil)
              options {:eof eof :readers {} :default (fn [& _] (throw (ex-info "Unsupported tag" {})))}
              value (edn/read options *in*)]
          (when (identical? eof (edn/read options *in*)) value))))
    (catch Exception _ nil)))
(defn finding? [f]
  (and (map? f) (= #{:kind :confidence :summary :citations} (set (keys f)))
       (contains? kinds (:kind f)) (contains? #{:low :medium :high} (:confidence f))
       (text? (:summary f) 2000) (vector? (:citations f)) (<= (count (:citations f)) 4)
       (or (not (contains? semantic-kinds (:kind f))) (seq (:citations f)))
       (every? #(and (map? %) (= #{:source-id :quote} (set (keys %)))
                     (text? (:source-id %) 100) (text? (:quote %) 4096)) (:citations f))))
(defn case? [c]
  (and (map? c) (= #{:id :findings :next-action :proposed-change} (set (keys c)))
       (text? (:id c) 100) (vector? (:findings c)) (<= 1 (count (:findings c)) 8)
       (every? finding? (:findings c)) (contains? actions (:next-action c))
       (or (nil? (:proposed-change c))
           (let [p (:proposed-change c)]
             (and (map? p) (= #{:target :description} (set (keys p)))
                  (contains? #{:reference :grading-policy :workflow} (:target p))
                  (text? (:description p) 4000))))))
(defn check [{:keys [files gold]}]
  (let [value (answer (get files "/out/review.edn"))
        shape? (and (map? value) (= #{:version :packet-id :cases :grading-changes-applied} (set (keys value)))
                    (= 1 (:version value)) (text? (:packet-id value) 100)
                    (vector? (:cases value)) (<= 1 (count (:cases value)) 16)
                    (every? case? (:cases value)))
        expected (into {} (map (juxt :id identity)) (:cases gold))
        binding? (and shape? (packet? gold) (= (:packet-id gold) (:packet-id value))
                      (= (count (:cases value)) (count (set (map :id (:cases value)))))
                      (= (set (keys expected)) (set (map :id (:cases value)))))
        quotations? (and binding?
                         (every? (fn [c]
                                   (let [sources (into {} (map (juxt :id :text)) (:sources (get expected (:id c))))]
                                     (every? (fn [{:keys [source-id quote]}]
                                               (let [source (get sources source-id)]
                                                 (and (string? source) (str/includes? source quote))))
                                             (mapcat :citations (:findings c))))) (:cases value)))
        advisory? (and shape? (false? (:grading-changes-applied value))
                       (every? (fn [c]
                                 (or (not (contains? #{:reference :grading-policy} (get-in c [:proposed-change :target])))
                                     (= :human-adjudication (:next-action c)))) (:cases value)))
        checks {:review-structure? (boolean shape?) :review-case-binding? (boolean binding?)
                :review-citations? (boolean quotations?) :advisory-only? (boolean advisory?)}]
    {:checks checks :reward (if (every? true? (vals checks)) 1.0 0.0)}))
