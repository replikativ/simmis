(ns is.simm.documents.rich-document
  "Source-backed continuous editing. Semantic containers and opaque nodes are
   immutable boundaries; ordinary paragraphs may split/join within a region."
  (:require [clojure.string :as str]
            [is.simm.documents.rich-text :as rich]))

(def boundary-types #{"memoryBlock" "memoryRegion" "memoryOpaque"})

(defn- refuse! []
  (throw (ex-info "This operation changes a protected knowledge boundary; use Source for structural changes."
                  {:type ::boundary})))

(defn project [source analysis]
  (let [slots (into {} (map (juxt :from identity)) (:slots analysis))]
    (letfn [(parts [blocks owner]
              (let [entries (sort-by :from
                                    (mapcat (fn [b]
                                              (if (= :explicit (:identity-status b))
                                                [(assoc b :container? true)] (:syntax b))) blocks))]
                (render-entries entries owner)))
            (render-entries [entries owner]
              (loop [remaining entries out [] regions {}]
                (if-let [entry (first remaining)]
                  (cond
                    (:container? entry)
                    (let [entries (sort-by :from (concat (:syntax entry) (map #(assoc % :container? true) (:children entry))))
                          nested (if (seq entries) (render-entries entries (str (:id entry)))
                                     (empty-region (:to (get slots (:from entry))) (str (:id entry))))]
                      (recur (next remaining)
                             (conj out {:type "memoryBlock" :attrs {:key (str "block:" (:id entry)) :blockId (str (:id entry))}
                                        :content (:content nested)})
                             (merge regions (:regions nested))))

                    (:model (get slots (:from entry)))
                    (let [run (loop [xs remaining taken []]
                                (let [e (first xs) previous (peek taken)]
                                  (if (and e (not (:container? e)) (:model (get slots (:from e)))
                                           (or (nil? previous)
                                               (str/blank? (subs source (:to previous) (:from e)))))
                                    (recur (next xs) (conj taken (get slots (:from e)))) taken)))
                          key (str "region:" (:from (first run)))
                          model {:type "doc" :content (vec (mapcat #(get-in % [:model :content]) run))}]
                      (recur (drop (count run) remaining)
                             (conj out {:type "memoryRegion" :attrs {:key key :blockId owner} :content (:content model)})
                             (assoc regions key {:from (:from (first run)) :to (:to (peek run)) :slots run :model model})))

                    :else
                    (recur (next remaining)
                           (conj out {:type "memoryOpaque"
                                      :attrs {:key (str "opaque:" (:from entry))
                                              :label (subs source (:from entry) (:to entry))}}) regions))
                  {:content out :regions regions})))
            (empty-region [pos owner]
              (let [key (str "region:" pos) model {:type "doc" :content []}]
                {:content [{:type "memoryRegion" :attrs {:key key :blockId owner}
                            :content [{:type "paragraph" :content []}]}]
                 :regions {key {:from pos :to pos :model model :slots [{:from pos :to pos :model model}]}}}))]
      (let [result (if (seq (get-in analysis [:semantic :blocks]))
                     (parts (get-in analysis [:semantic :blocks]) nil)
                     (empty-region (or (:body-from analysis) 0) nil))]
        (when (seq (:content result))
          {:model {:type "doc" :content (:content result)} :regions (:regions result)})))))

(defn topology [model]
  (rich/validate-model! model)
  (letfn [(walk [n]
            (case (:type n)
              "doc" (mapv walk (:content n))
              "memoryBlock" [(:type n) (select-keys (:attrs n) [:key :blockId]) (mapv walk (:content n))]
              "memoryRegion" (do (rich/markdown {:type "doc" :content (:content n)})
                                  [(:type n) {:key (get-in n [:attrs :key]) :blockId (get-in n [:attrs :blockId])}])
              "memoryOpaque" (do (when (seq (:content n)) (refuse!)) [(:type n) (select-keys (:attrs n) [:key :label])])
              (refuse!)))]
    (walk model)))

(defn region-models [model]
  (into {} (keep (fn [n] (when (= "memoryRegion" (:type n))
                          [(get-in n [:attrs :key]) {:type "doc" :content (:content n)}])))
        (tree-seq #(contains? #{"doc" "memoryBlock"} (:type %)) :content model)))

(defn normalized [model]
  (if (some #(contains? boundary-types (:type %)) (:content model))
    [(topology model) (into {} (map (fn [[k v]] [k (rich/normalize v)])) (region-models model))]
    (rich/normalize model)))

(defn edits [projection model]
  (when-not (= (topology (:model projection)) (topology model)) (refuse!))
  (let [models (region-models model)]
    (vec (keep (fn [[key region]]
                 (let [edited (get models key)]
                   (when-not (= (rich/normalize (:model region)) (rich/normalize edited))
                     (assoc (select-keys region [:from :to]) :model edited)))) (:regions projection)))))

(defn- changed-hunks
  "LCS anchors retain original spelling even between separate structural edits.
   This is syntax reuse within ONE semantic owner, never identity inference."
  [old new]
  (let [n (count old) m (count new)]
    (when (> (* n m) 160000)
      (throw (ex-info "Structural edit exceeds the interactive source-map limit; use Source." {:type ::limit})))
    (let [table (loop [i (dec n) table (vec (repeat (inc n) (vec (repeat (inc m) 0))))]
                  (if (neg? i) table
                      (recur (dec i)
                             (assoc table i
                                    (loop [j (dec m) row (vec (repeat (inc m) 0))]
                                      (if (neg? j) row
                                          (recur (dec j)
                                                 (assoc row j
                                                        (if (= (nth old i) (nth new j))
                                                          (inc (get-in table [(inc i) (inc j)]))
                                                          (max (get-in table [(inc i) j]) (get row (inc j))))))))))))
          anchors (loop [i 0 j 0 out []]
                    (cond
                      (or (= i n) (= j m)) (conj out [n m])
                      (= (nth old i) (nth new j)) (recur (inc i) (inc j) (conj out [i j]))
                      (>= (get-in table [(inc i) j]) (get-in table [i (inc j)])) (recur (inc i) j out)
                      :else (recur i (inc j) out)))]
      (:hunks (reduce (fn [{:keys [i j hunks]} [a b]]
                        {:i (inc a) :j (inc b)
                         :hunks (cond-> hunks (or (< i a) (< j b)) (conj [i a j b]))})
                      {:i 0 :j 0 :hunks []} anchors)))))

(defn apply-run
  "Preserve unchanged syntax bytes while editing inside one semantic owner.
   Source offsets are UTF-16 and stay relative to the frozen draft baseline."
  [source slots model]
  (let [new (vec (:content (rich/normalize model)))
        newline (or (re-find #"\r\n|\n|\r" source) "\n")]
    (if (= (:from (first slots)) (:to (first slots)))
      (let [text (rich/markdown {:type "doc" :content new}) pos (:from (first slots))]
        (if (str/blank? text) source
            (str (subs source 0 pos) (when (pos? pos) newline)
                 (str/replace text "\n" newline) newline newline (subs source pos))))
      (let [old (mapv #(first (get-in % [:model :content])) slots)
            normalized-old (mapv #(rich/normalize {:type "doc" :content [%]}) old)
            normalized-new (mapv #(rich/normalize {:type "doc" :content [%]}) new)
            hunks (if (= (count old) (count new))
                    (vec (keep-indexed (fn [i n] (when (not= n (nth normalized-new i)) [i (inc i) i (inc i)])) normalized-old))
                    (changed-hunks normalized-old normalized-new))]
        (reduce
         (fn [text [a b c d]]
           (let [replacement {:type "doc" :content (subvec new c d)}]
             (if (< a b)
               (rich/apply-region text {:from (:from (nth slots a)) :to (:to (nth slots (dec b)))
                                         :model {:type "doc" :content (subvec old a b)}} replacement)
               (let [next-slot (get slots a) previous (peek slots)
                     pos (if next-slot (:from next-slot) (:to previous))
                     ending (or (re-find #"[\r\n]+$" (subs source (:from previous) (:to previous))) "")
                     leading (when-not next-slot
                               (apply str (repeat (max 0 (- 2 (count (re-seq #"\r\n|\n|\r" ending)))) newline)))
                     trailing (if next-slot (str newline newline) (when (seq ending) newline))
                     inserted (str leading (str/replace (rich/markdown replacement) "\n" newline) trailing)]
                 (str (subs text 0 pos) inserted (subs text pos))))))
         source (reverse hunks))))))

(defn apply-model [source projection model]
  (reduce (fn [text {:keys [from model]}]
            (let [region (first (filter #(= from (:from %)) (vals (:regions projection))))]
              (apply-run text (:slots region) model)))
          source (sort-by :from > (edits projection model))))

(defn region-source
  "Locate a current editable content region in the frozen map. Adjacent rich
   edits shift its offsets; semantic children/opaque nodes are never included."
  [source projection model key]
  (when-let [region (get-in projection [:regions key])]
    (let [changes (edits projection model)
          delta (fn [edit]
                  (let [original (first (filter #(= (:from edit) (:from %)) (vals (:regions projection))))]
                    (- (count (apply-run source (:slots original) (:model edit))) (count source))))
          before (reduce + 0 (map delta (filter #(< (:from %) (:from region)) changes)))
          own (first (filter #(= (:from %) (:from region)) changes))
          from (+ (:from region) before) to (+ (:to region) before (if own (delta own) 0))
          current (apply-model source projection model)]
      {:region key :from from :to to :text (subs current from to)})))
