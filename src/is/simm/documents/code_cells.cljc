(ns is.simm.documents.code-cells
  "Inert correspondence between saved fenced syntax and supported rich nodes.
   These UI descriptors are never execution authority."
  (:require [clojure.string :as str]
            [is.simm.documents.rich-text :as rich]))

(def max-code-characters 16384)
(def max-visible-cells 128)

(defn equivalent-projection?
  "Cell correspondence only, never a source-write/provenance gate. A long-lived
   native document retains derived region/opaque keys across autosaves. Ignore
   only those offset keys after validating their shape; retain explicit block
   identities, opaque bytes, ordering and all ordinary content. Source-backed
   math/footnote keys remain strict: a moved token temporarily disables Run."
  [saved native]
  (letfn [(attrs [node] (merge {:key nil :blockId nil :label nil} (:attrs node)))
          (walk [node]
            (case (:type node)
              "doc" ["doc" (mapv walk (:content node))]
              "memoryBlock" ["memoryBlock" (attrs node) (mapv walk (:content node))]
              ("memoryRegion" "memoryOpaque")
              (let [prefix (if (= "memoryRegion" (:type node)) "region:" "opaque:")
                    key (get-in node [:attrs :key])]
                (when-not (and (string? key) (re-matches (re-pattern (str prefix "[0-9]+")) key))
                  (throw (ex-info "Invalid source projection key" {})))
                [(:type node) (dissoc (attrs node) :key)
                 (if (= "memoryRegion" (:type node))
                   (rich/normalize {:type "doc" :content (:content node)})
                   (vec (:content node)))])
              (throw (ex-info "Invalid cell projection boundary" {}))))]
    (try (rich/validate-model! saved) (rich/validate-model! native)
         (= (walk saved) (walk native))
         (catch #?(:clj Exception :cljs :default) _ false))))

(defn language [info]
  (some-> info str/trim (str/split #"\s+") first str/lower-case))

(defn clojure-fence? [node]
  (and (= :FencedCodeBlock (:kind node))
       (contains? #{"clj" "clojure"} (language (:language node)))))

(defn- region-paths [model]
  (letfn [(walk [node path]
            (cond
              (= "memoryRegion" (:type node))
              [[(get-in node [:attrs :key]) {:path path :node node}]]
              (contains? #{"doc" "memoryBlock"} (:type node))
              (mapcat (fn [[i child]] (walk child (conj path i))) (map-indexed vector (:content node)))
              :else []))]
    (into {} (walk model []))))

(defn- paired-cells [syntax model path owner region]
  (cond
    (and (clojure-fence? syntax) (= "codeBlock" (:type model)))
    (let [code (:literal syntax) size (count code)]
      [{:key (str "cell:" (:from syntax) ":" (:to syntax))
        :from (:from syntax) :to (:to syntax) :block-id owner :region region
        :node-path path :language (language (:language syntax)) :characters size
        :runnable? (and (pos? size) (<= size max-code-characters) (not (str/blank? code)))}])

    ;; Indented code participates in the model correspondence but is inert.
    ;; Never zip all AST fences against all native codeBlock nodes: opaque
    ;; regions may contain fences that do not occur in the rich document.
    (and (contains? #{:BulletList :OrderedList :BulletListItem :OrderedListItem} (:kind syntax))
         (= ({:BulletList "bulletList" :OrderedList "orderedList"
              :BulletListItem "listItem" :OrderedListItem "listItem"} (:kind syntax)) (:type model))
         (= (count (:children syntax)) (count (:content model))))
    (mapcat (fn [i child rendered]
              (paired-cells child rendered (conj path i) owner region))
            (range) (:children syntax) (:content model))

    :else []))

(defn descriptors
  "Map only exact supported slots/regions in this frozen projection. Paths
   index model :content vectors, including semantic wrappers and Markdown lists.
   The native editor must still verify its complete baseline before offering Run."
  [analysis {:keys [model regions]}]
  (if-not (= :ready (get-in analysis [:semantic :status])) []
    (let [syntax (into {} (keep (fn [node]
                                  (when (some clojure-fence? (tree-seq (comp seq :children) :children node))
                                    [[(:from node) (:to node)] node]))) (:nodes analysis))
        paths (region-paths model)]
    (vec
     (take max-visible-cells
           (mapcat
            (fn [[key region]]
              (let [{:keys [path node]} (get paths key)]
                (when (and path (some #(contains? syntax [(:from %) (:to %)]) (:slots region))
                           (= (rich/normalize (:model region))
                              (rich/normalize {:type "doc" :content (:content node)})))
                  (loop [slots (:slots region) offset 0 result []]
                    (if-let [slot (first slots)]
                      (let [parsed (get syntax [(:from slot) (:to slot)])
                            rendered (:model slot)
                            verified? (and parsed rendered
                                           (= (rich/normalize rendered) (some-> (rich/model parsed) rich/normalize)))
                            cells (when (and verified? (= 1 (count (:content rendered))))
                                    (paired-cells parsed (first (:content rendered)) (conj path offset)
                                                  (get-in node [:attrs :blockId]) key))]
                        (recur (next slots) (+ offset (count (:content rendered))) (into result cells)))
                      result)))))
            (sort-by (comp :from val) regions)))))))
