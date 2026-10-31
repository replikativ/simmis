(ns simmis.workflows.maintain-wiki-v5
  (:require [clojure.string :as str]))

;; Mechanical fixture checks, not factual truth or publication authority.
;; Citations use parsed inline or defined reference links, not code or comments.
(defn wiki-path? [path]
  (and (string? path) (<= (count path) 512) (str/starts-with? path "/out/wiki/")
       (boolean (re-find #"(?i)\.(md|markdown)$" path))
       (not (re-find #"[\\\p{Cntrl}]" path))
       (every? #(not (contains? #{"" "." ".."} %)) (str/split (subs path 10) #"/" -1))))

(defn preserves? [before after]
  (let [old-ids (set (map :id (:blocks before)))]
    (and (= (:page-id before) (:page-id after))
         (= (:metadata-digest before) (:metadata-digest after))
         (= (:blocks before) (filterv #(contains? old-ids (:id %)) (:blocks after))))))

(defn check [{:keys [files gold structure]}]
  (let [fixture? (and (= 2 (:version gold)) (boolean? (:no-change? gold))
                      (= :wiki-checkpoint-structure-v1 (get-in structure [:profile :id])))
        baseline (get-in structure [:baseline :files])
        proposed (get-in structure [:proposed :files])
        paths? (and (map? files) (<= (count files) 100)
                    (every? (fn [[path text]] (and (wiki-path? path) (string? text))) files)
                    (= (set (map #(subs % 5) (keys files))) (set (keys proposed))))
        texts (vals files)
        effective-texts (merge (:baseline-texts structure)
                               (into {} (map (fn [[path text]] [(subs path 5) text])) files))
        includes? (fn [fragment] (boolean (some #(str/includes? % fragment) texts)))
        old-ids (into #{} (mapcat :ids) (vals baseline))
        new-ids (mapcat :ids (vals proposed))
        citations (mapcat :links (vals proposed))
        frozen (set (:source-paths structure))
        checks
        {:fixture? (boolean fixture?)
         :wiki-paths? (boolean paths?)
         :expected-change? (boolean (and fixture? (if (:no-change? gold) (empty? files) (seq files))))
         :structure? (boolean (and paths? (every? :valid? (vals proposed))))
         :preserved-structure? (boolean (every? (fn [[path after]]
                                                (if-let [before (get baseline path)]
                                                  (preserves? before after)
                                                  (some? (:page-id after)))) proposed))
         :unique-identities? (boolean (and (= (count new-ids) (count (set new-ids)))
                                           (every? (fn [[path after]]
                                                     (let [own (set (:ids (get baseline path)))]
                                                       (every? #(or (not (contains? old-ids %)) (contains? own %))
                                                               (:ids after)))) proposed)))
         :required-content? (boolean (and fixture? (every? includes? (:required gold))))
         :preserved-content? (boolean
                              (and fixture?
                                   (every? (fn [[path fragments]]
                                             (and (contains? baseline path)
                                                  (string? (get effective-texts path))
                                                  (every? #(str/includes? (get effective-texts path) %) fragments)))
                                           (:preserved gold))))
         :forbidden-content? (boolean (and fixture? (not-any? includes? (:forbidden gold))))
         :citations? (boolean (and fixture? (every? #(or (not (str/starts-with? % "/in/knowledge/"))
                                                        (contains? frozen %)) citations)
                                   (every? (set citations) (:citations gold))))
         :quotes? (boolean (and fixture? (every? (fn [{:keys [source text]}]
                                                  (and (contains? frozen source)
                                                       (string? (get (:source-texts structure) source))
                                                       (str/includes? (get (:source-texts structure) source) text)
                                                       (includes? text))) (:quotes gold))))}]
    {:checks checks :reward (/ (double (count (filter true? (vals checks)))) (count checks))}))
