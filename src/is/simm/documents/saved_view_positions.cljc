(ns is.simm.documents.saved-view-positions
  "Exact, inert saved-fence correspondence for document presentation.
   Paths select specific syntax/model occurrences, never matching code text
   across a document. They are not read or write authority."
  (:require [is.simm.documents.code-cells :as cells]
            [is.simm.documents.rich-text :as rich]
            [is.simm.documents.saved-view-contract :as contract]))

(defn- paired [syntax node path entries]
  (cond
    (and (= :FencedCodeBlock (:kind syntax))
         (= "simmis-view" (cells/language (:language syntax)))
         (= "codeBlock" (:type node)))
    (if-let [entry (get entries [(:from syntax) (:to syntax)])]
      {path entry} {})

    (and (= ({:BulletList "bulletList" :OrderedList "orderedList"
              :BulletListItem "listItem" :OrderedListItem "listItem"} (:kind syntax)) (:type node))
         (= (count (:children syntax)) (count (:content node))))
    (reduce merge {} (map-indexed (fn [index child]
                                   (paired child (nth (:content node) index) (conj path index) entries))
                                 (:children syntax)))
    :else {}))

(defn positions
  "Map source-backed slot model :content paths to eligible saved entries.
   A slot is accepted only when its entire model still matches its exact AST
   occurrence. Unsupported/opaque structures stay source-only."
  [analysis slots]
  (let [entries (vec (take contract/max-mounted (:saved-views analysis)))
        ids (frequencies (map :view entries))
        spans (frequencies (map (juxt :from :to) entries))
        entries (into {} (keep (fn [entry]
                                (when (and (= :ready (:status entry)) (uuid? (:view entry))
                                           (= 1 (get ids (:view entry)))
                                           (= 1 (get spans [(:from entry) (:to entry)])))
                                  [[(:from entry) (:to entry)] entry]))) entries)
        syntax (into {} (map (juxt (juxt :from :to :kind) identity))
                     (mapcat #(tree-seq (comp seq :children) :children %) (:nodes analysis)))]
    (reduce merge {}
            (map-indexed
             (fn [index slot]
               (let [node (get syntax [(:from slot) (:to slot) (:kind slot)]) model (:model slot)]
                 (if (and node model (= 1 (count (:content model)))
                          (= (rich/normalize model) (some-> node rich/model rich/normalize)))
                   (paired node (first (:content model)) [index 0] entries) {}))) slots))))
