(ns simmis.workflows.maintain-wiki
  (:require [clojure.string :as str]))

;; Benchmark-only mechanical checks. Gold is evaluator-held fixture data, never
;; candidate input or an authority to publish. This does not prove semantic truth.
(defn wiki-path? [path]
  (and (string? path) (<= (count path) 512)
       (str/starts-with? path "/out/wiki/")
       (boolean (re-find #"(?i)\.(md|markdown)$" path))
       (not (re-find #"[\\\p{Cntrl}]" path))
       (every? #(not (contains? #{"" "." ".."} %))
               (str/split (subs path (count "/out/wiki/")) #"/" -1))))

(defn citations [text]
  (let [matches (re-seq #"\]\((?:<(/in/knowledge/sources/[^<>\r\n]+)>|(/in/knowledge/sources/[^\s()<>]+))\)" text)
        starts (re-seq #"\]\(<?/in/knowledge/sources/" text)]
    {:valid? (= (count matches) (count starts))
     :paths (mapv (fn [[_ angled bare]] (or angled bare)) matches)}))

(defn check [{:keys [files gold]}]
  (let [fixture? (and (= 1 (:version gold)) (map? (:expected gold)))
        expected (:expected gold)
        paths? (and (map? files) (<= (count files) 100)
                    (every? (fn [[path text]]
                              (and (wiki-path? path) (string? text)
                                   (<= (count text) 524288)))
                            files))
        shape? (and fixture? (= (set (keys expected)) (set (keys files))))
        includes? (fn [path fragments]
                    (let [text (get files path)]
                      (and (string? text) (every? #(str/includes? text %) fragments))))
        cited (when paths? (into {} (map (fn [[p text]] [p (citations text)])) files))
        checks {:fixture? (boolean fixture?)
                :wiki-paths? (boolean paths?)
                :expected-pages? (boolean shape?)
                :required-content? (boolean (and fixture? (every? (fn [[p spec]] (includes? p (:required spec))) expected)))
                :preserved-content? (boolean (and fixture? (every? (fn [[p spec]] (includes? p (:preserved spec))) expected)))
                :forbidden-content? (boolean (and fixture? (every? (fn [[p spec]]
                                                                     (let [text (get files p)]
                                                                       (and (string? text)
                                                                            (not-any? #(str/includes? text %) (:forbidden spec))))) expected)))
                :citations? (boolean (and fixture? paths?
                                          (every? :valid? (vals cited))
                                          (every? #(contains? (:sources gold) %) (mapcat :paths (vals cited)))
                                          (every? (fn [[p spec]]
                                                    (every? (set (:paths (get cited p))) (:citations spec))) expected)))
                :quotes? (boolean (and fixture?
                                       (every? (fn [[p spec]]
                                                 (every? (fn [{:keys [source text]}]
                                                           (and (string? (get (:sources gold) source))
                                                                (str/includes? (get (:sources gold) source) text)
                                                                (includes? p [text]))) (:quotes spec))) expected)))}]
    {:checks checks
     :reward (/ (double (count (filter true? (vals checks)))) (count checks))}))
