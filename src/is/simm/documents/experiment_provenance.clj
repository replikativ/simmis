(ns is.simm.documents.experiment-provenance
  "Bounded display provenance from frozen ExperimentDefs. No current file
   lookup, split inference or arbitrary metadata reflection.")

(defn workflow
  "Only bounded provenance from the frozen ExperimentDef, never current files
   or arbitrary experiment metadata. Legacy/non-workflow experiments omit it."
  [definition]
  (let [{:keys [name title bundle]} (get-in definition [:experiment/metadata :simmis/workflow])
        bundle-id (cond (uuid? bundle) bundle
                        (and (string? bundle) (= 36 (count bundle))) (parse-uuid bundle))]
    (when (and (string? name) (<= 1 (count name) 64)
               (re-matches #"[a-z0-9][a-z0-9-]*" name) bundle-id)
      (cond-> {:name name :bundle (str bundle-id)}
        (and (string? title) (seq title)) (assoc :title (subs title 0 (min 256 (count title))))))))

(defn evaluation
  "Bounded frozen dataset provenance; never infer a split for old experiments."
  [definition]
  (let [{:keys [id version split cases]} (get-in definition [:experiment/metadata :simmis/evaluation])
        dataset-id (when (and (string? id) (= 36 (count id))) (parse-uuid id))]
    (when (and dataset-id (= 1 version) (#{"development" "validation" "held-out"} split)
               (vector? cases) (<= 1 (count cases) 100)
               (every? #(and (string? %) (<= 1 (count %) 100)) cases)
               (= (count cases) (count (set cases))))
      {:id (str dataset-id) :version version :split split :cases cases})))
