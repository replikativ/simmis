(ns is.simm.uis.web.desktop.memory-editor
  "Serializable, execution-context-owned document drafts and conflict rules."
  (:require [is.simm.documents.rich-text :as rich]))

(def max-rich-regions 200)

(defn code-view [draft preference]
  (or (:code-view draft) preference :clojure))

(defn repl-syntax [state preference] (or (get-in state [:repl :syntax]) preference :clojure))

(defn repl-input [state syntax] (get-in state [:repl :inputs syntax :text] ""))

(defn repl-result [state receipt]
  (cond-> (update-in state [:repl :history] #(vec (take 20 (cons receipt %))))
    (and (:namespace receipt)
         (= (:requested-namespace receipt) (get-in state [:repl :namespace] "user")))
    (assoc-in [:repl :namespace] (:namespace receipt))))

(defn evaluation-receipt? [slot reply]
  (and (or (= :repl slot) (and (vector? slot) (= :evaluate (first slot))))
       (= :human-sci (:target reply)) (uuid? (:id reply))
       (or (true? (:success reply)) (false? (:success reply)))))

(defn begin-rich [d]
  (let [analysis (:analysis d)]
    (if (or (:rich-base d) (not= :ready (get-in analysis [:semantic :status]))
            (> (count (:slots analysis)) max-rich-regions)) d
        (assoc d :rich-base {:id [(:revision analysis) (:generation d)]
                            :text (:text d) :analysis analysis
                            :models (into {} (keep (fn [slot] (when (:model slot) [(:from slot) (:model slot)]))) (:slots analysis))
                            :selections {}}))))

(defn rich-edits [d]
  (let [{:keys [analysis models]} (:rich-base d)]
    (vec (for [slot (:slots analysis)
               :let [model (get models (:from slot))]
               :when (and (:model slot) (not= (rich/normalize (:model slot)) (rich/normalize model)))]
           {:from (:from slot) :to (:to slot) :model model}))))

(defn rich-changed [d base-id from model selection]
  (if (not= base-id (get-in d [:rich-base :id])) d
      (let [slots (get-in d [:rich-base :analysis :slots])
            slot (first (filter #(= from (:from %)) slots))]
        (when-not (:model slot) (throw (ex-info "This region is source-only" {:type ::unsupported})))
        (rich/validate-model! model)
        (let [next (-> d (assoc-in [:rich-base :models from] model)
                       (assoc-in [:rich-base :selections from] selection))
              edits (into {} (map (juxt :from identity)) (rich-edits next))
              text (reduce (fn [source slot]
                             (if-let [edit (get edits (:from slot))]
                               (rich/apply-region source slot (:model edit)) source))
                           (get-in d [:rich-base :text]) (reverse slots))]
          (assoc next :text text)))))

(defn source-selection [d from to]
  (let [source (get-in d [:rich-base :text]) edits (rich-edits d)
        slots (into {} (map (juxt :from identity)) (get-in d [:rich-base :analysis :slots]))
        offset (fn [position]
                 (+ position (reduce + 0 (for [edit edits :when (<= (:to edit) position)]
                                            (- (count (rich/apply-region source (get slots (:from edit)) (:model edit)))
                                               (count source))))))]
    {:anchor (offset from) :head (offset to)}))

(defn draft [file]
  (let [d {:source file :text (:text file) :analysis (:analysis file) :analysis-text (:text file) :generation 0
           :mode (if (= :markdown (:kind file)) :rich :source) :selection {:anchor 0 :head 0}}]
    (if (= :markdown (:kind file)) (begin-rich d) d)))

(defn dirty? [d]
  (or (and (:source d) (nil? (get-in d [:source :entry])))
      (not= (:text d) (get-in d [:source :text]))
      (not= (get-in d [:office :text]) (get-in d [:office :saved-text]))))

(defn loaded [old file]
  (cond
    (nil? old) (draft file)
    (or (dirty? old) (:rich-edit old))
    (if (= (:entry file) (get-in old [:source :entry]))
      (dissoc old :incoming :incoming-entry :remote-deleted?)
      (-> old (assoc :incoming file) (dissoc :incoming-entry)))
    :else (assoc (merge (draft file) (select-keys old [:mode :selection :code-view]))
                 ;; A same-entry refresh must not supersede an in-flight save.
                 :generation (if (= (:entry file) (get-in old [:source :entry]))
                               (:generation old 0) (inc (:generation old 0))))))

(defn observed-entry
  "Deletion retains the entire draft/Office model, even when clean. It is not
   permission to recreate a missing file. Only an explicit recovery can do so."
  [d entry]
  (let [d (when d (assoc d :observed-entry entry))]
   (cond
    (nil? d) d
    (= entry (get-in d [:source :entry])) (dissoc d :incoming :incoming-entry :remote-deleted?)
    (nil? entry) (assoc d :remote-deleted? true
                       :incoming {:path (get-in d [:source :path]) :entry nil :deleted? true})
    :else (cond-> (assoc (dissoc d :remote-deleted?) :incoming-entry entry)
            (get-in d [:incoming :deleted?]) (dissoc :incoming)))))

(defn metadata-request [state]
  (let [pending (for [[slot loading?] (:loading state)
                      :when (and loading? (vector? slot))] slot)]
    {:folders (vec (sort (distinct (concat (keys (:folders state))
                                          (when (:root-expanded? state) [(or (:focus-folder state) "")])
                                          (map second (filter #(= :folder (first %)) pending))))))
     :paths (vec (sort (distinct (concat (keys (:drafts state))
                                        (map second (filter #(= :file (first %)) pending))))))}))

(defn property-entries [properties] (sort-by (comp pr-str key) properties))

(defn changed [d text selection]
  (cond-> (assoc d :text text :selection selection)
    (not= text (:text d)) (dissoc :rich-base)
    (not= text (:analysis-text d)) (dissoc :analysis)))

(defn loaded-at-generation [d generation file]
  (if (= generation (:generation d)) (loaded d file) d))

(defn metadata-generation-current? [expected actual]
  (or (= expected actual)
      ;; An initial file read may have finished while metadata was in flight.
      ;; Generation zero permits observation, not draft replacement: dirty?
      ;; still protects typing; any completed save/move advances the generation.
      (and (nil? expected) (= 0 actual))))

(defn use-incoming [d]
  (if-let [incoming (:incoming d)]
    (if (:deleted? incoming) d
        (assoc (draft incoming) :generation (inc (:generation d 0)))) d))

(defn analyzed [d text analysis]
  (if (= text (:text d))
    (cond-> (assoc d :analysis analysis :analysis-text text)
      (= :rich (:mode d)) begin-rich) d))

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
