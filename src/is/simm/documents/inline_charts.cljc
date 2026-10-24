(ns is.simm.documents.inline-charts
  "Inert chart-fence previews. No identities, interpreter, queries or effects."
  (:require [is.simm.documents.chart-contract :as contract]
            [is.simm.documents.code-cells :as cells]
            #?(:clj [is.simm.documents.chart-spec :as spec])
            #?(:clj [is.simm.documents.rich-text :as rich])))

(defn fence? [node]
  (and (= :FencedCodeBlock (:kind node)) (= "simmis-chart" (cells/language (:language node)))))

#?(:clj
   (defn analyze
     "At most four bounded previews of actual parsed fences. Match the projected
      code text, including nested-prefix/newline handling, not raw source spans."
     [nodes]
     (mapv (fn [node]
             (let [projected (first (:content (rich/model node)))
                   base {:language (get-in projected [:attrs :language])
                         :text (apply str (map :text (:content projected)))
                         :from (:from node) :to (:to node)}]
               (try (assoc base :status :ready :data (spec/decode (:literal node)))
                    (catch clojure.lang.ExceptionInfo error
                      (if (= :is.simm.documents.chart-spec/refused (:type (ex-data error)))
                        (assoc base :status :invalid)
                        (throw error))))))
           (take contract/max-mounted
                 (filter fence? (mapcat #(tree-seq (comp seq :children) :children %) nodes))))))

(defn normalize-preview [preview]
  ;; JSON attributes carry enum values as strings; normalize only closed enums.
  (cond-> (update preview :status #(case % "ready" :ready "invalid" :invalid %))
    (:data preview) (update-in [:data :kind] #(case % "line" :line "bar" :bar "point" :point %))))

(defn match-preview [previews node]
  (when (= "codeBlock" (:type node))
    (let [language (get-in node [:attrs :language]) text (apply str (map :text (:content node)))]
      (some #(when (and (= language (:language %)) (= text (:text %))) %) previews))))

(defn positions
  "Sidecar by model :content path. Limit actual occurrences, including copied
   identical fences, without adding fields to the editor's semantic model."
  [models previews limit]
  (letfn [(walk [node path [found remaining :as result]]
            (cond
              (zero? remaining) result
              (= "codeBlock" (:type node))
              (if-let [preview (match-preview previews node)]
                [(assoc found path preview) (dec remaining)] result)
              :else (reduce (fn [result [index child]] (walk child (conj path index) result))
                            result (map-indexed vector (:content node)))))]
    (first (reduce (fn [result [index model]] (walk model [index] result))
                   [{} (max 0 (min contract/max-mounted (or limit 0)))]
                   (map-indexed vector models)))))
