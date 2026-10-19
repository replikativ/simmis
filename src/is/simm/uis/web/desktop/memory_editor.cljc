(ns is.simm.uis.web.desktop.memory-editor
  "Serializable, execution-context-owned document drafts and conflict rules.")

(defn draft [file]
  {:source file :text (:text file) :analysis (:analysis file) :analysis-text (:text file) :generation 0
   :mode (if (= :markdown (:kind file)) :read :source) :selection {:anchor 0 :head 0}})

(defn dirty? [d]
  (or (and (:source d) (nil? (get-in d [:source :entry])))
      (not= (:text d) (get-in d [:source :text]))
      (not= (get-in d [:office :text]) (get-in d [:office :saved-text]))))

(defn loaded [old file]
  (cond
    (nil? old) (draft file)
    (or (dirty? old) (:rich-edit old))
    (if (= (:entry file) (get-in old [:source :entry])) old (assoc old :incoming file))
    :else (assoc (merge (draft file) (select-keys old [:mode :selection]))
                 :generation (inc (:generation old 0)))))

(defn changed [d text selection]
  (cond-> (assoc d :text text :selection selection)
    (not= text (:analysis-text d)) (dissoc :analysis)))

(defn loaded-at-generation [d generation file]
  (if (= generation (:generation d)) (loaded d file) d))

(defn use-incoming [d]
  (if-let [incoming (:incoming d)]
    (assoc (draft incoming) :generation (inc (:generation d 0))) d))

(defn analyzed [d text analysis]
  (if (= text (:text d)) (assoc d :analysis analysis :analysis-text text) d))

(defn saved [d sent reply]
  (-> d
      (assoc-in [:source :entry] (:entry reply))
      (assoc-in [:source :text] sent)
      (assoc :last-commit (:commit reply))
      (update :generation (fnil inc 0))
      (cond-> (= (:entry reply) (get-in d [:incoming :entry])) (dissoc :incoming))))

(defn moved
  "Move the saved baseline, retaining any edits made while the move was in flight."
  [d reply]
  (-> d (assoc-in [:source :path] (:path reply))
      (assoc-in [:source :entry] (:entry reply))
      (assoc :last-commit (:commit reply))
      (update :generation (fnil inc 0))
      (dissoc :incoming)))

(defn saved-at-generation [d generation sent reply]
  (if (= generation (:generation d)) (saved d sent reply)
      (assoc d :earlier-save-receipt reply)))

(defn response-current? [state slot token]
  (= token (get-in state [:requests slot])))

(defn outline
  ([blocks] (outline blocks 0))
  ([blocks depth]
   (vec (mapcat (fn [b] (cons (assoc b :depth depth) (outline (:children b) (inc depth)))) blocks))))
