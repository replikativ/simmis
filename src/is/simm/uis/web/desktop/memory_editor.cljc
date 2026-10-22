(ns is.simm.uis.web.desktop.memory-editor
  "Serializable, execution-context-owned document drafts and conflict rules."
  (:require [is.simm.documents.rich-text :as rich]
            [is.simm.documents.rich-document :as document]))

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
                             :selections {}
                             :document (document/project (:text d) analysis)}))))

(defn rich-edits [d]
  (let [{:keys [analysis models]} (:rich-base d)]
    (vec (for [slot (:slots analysis)
               :let [model (get models (:from slot))]
               :when (and (:model slot) (not= (rich/normalize (:model slot)) (rich/normalize model)))]
           {:from (:from slot) :to (:to slot) :model model}))))

(defn document-edits [d]
  (if-let [model (get-in d [:rich-base :document-model])]
    (document/edits (get-in d [:rich-base :document]) model)
    (rich-edits d)))

(defn document-changed [d base-id model selection]
  (if (not= base-id (get-in d [:rich-base :id])) d
      (let [base (:rich-base d) projection (:document base)
            text (document/apply-model (:text base) projection model)]
        (-> d (assoc :text text)
            (assoc-in [:rich-base :document-model] model)
            (assoc-in [:rich-base :document-selection] selection)))))

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

(defn same-target? [a b]
  (= (:target-stamp a) (:target-stamp b)))

(defn- same-document? [a b]
  (and (same-target? a b) (= (:entry a) (:entry b))))

(defn observe-target
  "Inactive pure target-epoch transition, for the stamped adapter rollout.
   A stamp is identity, not authority. Invalidate file I/O/cache identities even
   for identical bytes, retaining every draft and its original source/Office/
   rich model for explicit conflict handling. Generic Room REPL is not retargeted."
  [state stamp]
  (when-not (uuid? stamp)
    (throw (ex-info "A Memory target needs an exact stamp" {:type ::invalid-target})))
  (if (= stamp (:target-stamp state)) state
      (-> state
          (assoc :target-stamp stamp)
          (update :read-epoch (fnil inc 0))
          (update :drafts
                  (fn [drafts]
                    (into {} (map (fn [[path d]]
                                    [path (-> d (assoc :target-stale? true :current-target-stamp stamp)
                                              (update :generation (fnil inc 0)))])) drafts)))
          (update :requests #(select-keys % [:repl]))
          (update :loading #(select-keys % [:repl]))
          (update :errors #(select-keys % [:repl]))
          (dissoc :folders :workspace :observed-entries :history :history-preview
                  :search :download :evaluations :permissions :opened-path
                  :watch-connection :sync))))

(defn loaded [old file]
  (cond
    (nil? old) (draft file)
    ;; Only observe-target may establish an intentional identity transition.
    ;; A late/unversioned body cannot erase the known target's incoming copy.
    (and (uuid? (:current-target-stamp old))
         (not= (:current-target-stamp old) (:target-stamp file))) old
    (or (dirty? old) (:rich-edit old))
    (if (same-document? file (:source old))
      (cond-> (dissoc old :incoming :incoming-entry :remote-deleted?)
        ;; An actual current-target body, not identity-only metadata, proves
        ;; an A→B→A draft's original baseline is available again. Keep typing,
        ;; rich/XML models and the explicit current marker; never trust late A
        ;; against a B marker or infer recreation from a missing file.
        (and (uuid? (:target-stamp file)) (some? (:entry file))
             (= (:target-stamp file) (:current-target-stamp old)))
        (dissoc :target-stale?))
      (cond-> (-> old (assoc :incoming file) (dissoc :incoming-entry))
        (not (same-target? file (:source old))) (assoc :target-stale? true)))
    :else (assoc (merge (draft file) (select-keys old [:mode :selection :code-view]))
                 ;; A same-entry refresh must not supersede an in-flight save.
                 :generation (if (same-document? file (:source old))
                               (:generation old 0) (inc (:generation old 0))))))

(defn observed-entry
  "Deletion retains the entire draft/Office model, even when clean. It is not
   permission to recreate a missing file. Only an explicit recovery can do so.

   Three-arity observations are stamped metadata, not full incoming bodies.
   observe-target must establish intentional target transitions; without that
   marker the first observed/incoming stamp anchors later observations. UUIDs
   do not carry chronology: a late old-target observation cannot switch back."
  ([d entry]
   (let [d (when d (assoc d :observed-entry entry))]
     (cond
       (nil? d) d
       (= entry (get-in d [:source :entry])) (dissoc d :incoming :incoming-entry :remote-deleted?)
       (nil? entry) (assoc d :remote-deleted? true
                           :incoming {:path (get-in d [:source :path]) :entry nil :deleted? true})
       :else (cond-> (assoc (dissoc d :remote-deleted?) :incoming-entry entry)
               (get-in d [:incoming :deleted?]) (dissoc :incoming)))))
  ([d entry stamp]
   (when-not (uuid? stamp)
     (throw (ex-info "A Memory observation needs an exact target stamp" {:type ::invalid-target})))
   (let [known (or (:current-target-stamp d) (:observed-target-stamp d)
                   (:incoming-target-stamp d) (get-in d [:incoming :target-stamp]))]
     (cond
       (nil? d) nil
       (and known (not= known stamp)) d
       (= stamp (get-in d [:source :target-stamp]))
       (let [incoming (:incoming d)
             same-incoming? (and (some? entry) (= stamp (:target-stamp incoming))
                                 (= entry (:entry incoming)))
             prior (cond-> (dissoc d :incoming-target-stamp :incoming-missing?)
                     (and incoming (not same-incoming?)) (dissoc :incoming))]
         (assoc (observed-entry prior entry) :observed-target-stamp stamp))
       :else
       (let [incoming (:incoming d)
             same-incoming? (and (some? entry) (= stamp (:target-stamp incoming))
                                 (= entry (:entry incoming))
                                 (not (:deleted? incoming)))]
         (cond-> (-> d
                     (assoc :observed-entry entry :observed-target-stamp stamp
                            :incoming-entry entry :incoming-target-stamp stamp :target-stale? true)
                     (dissoc :remote-deleted? :incoming-missing?))
           (nil? entry) (assoc :incoming-missing? true)
           (not same-incoming?) (dissoc :incoming)))))))

(defn metadata-request [state]
  (let [pending (for [[slot loading?] (:loading state)
                      :when (and loading? (vector? slot))] slot)]
    {:folders (vec (sort (distinct (concat (keys (:folders state))
                                           (when (:root-expanded? state) [(or (:focus-folder state) "")])
                                           (map second (filter #(= :folder (first %)) pending))))))
     :paths (vec (sort (distinct (concat (keys (:drafts state))
                                         (map second (filter #(= :file (first %)) pending))))))}))

(defn property-entries [properties] (sort-by (comp pr-str key) properties))

(defn property-baseline [model]
  (select-keys model [:path :target :expected :revision :scope-revision :target-stamp]))

(defn property-input
  "Use exact host-rendered text, never stringify a browser numeric value."
  [model field draft]
  (if draft (:input draft) (:input field)))

(defn property-draft [model input]
  {:baseline (property-baseline model) :input input})

(defn property-draft-current? [model draft]
  (or (nil? draft) (= (property-baseline model) (:baseline draft))))

(defn property-drafts? [state path]
  (boolean (some (fn [[[p _] drafts]] (and (= p path) (seq drafts))) (:property-drafts state))))

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
      (cond-> (same-document? reply (:incoming d)) (dissoc :incoming))))

(defn moved
  "Move the saved baseline, retaining any edits made while the move was in flight."
  [d reply]
  (-> d (assoc-in [:source :path] (:path reply))
      (assoc-in [:source :entry] (:entry reply))
      (assoc :last-commit (:commit reply))
      (update :generation (fnil inc 0))
      (dissoc :incoming)))

(defn saved-at-generation [d generation sent reply]
  (if (and (= generation (:generation d)) (same-target? (:source d) reply)) (saved d sent reply)
      (assoc d :earlier-save-receipt reply)))

(defn property-saved
  "Update the baseline while retaining typing made during a property write.
   Never replace a newer file generation."
  [draft generation reply]
  (cond
    (nil? draft) nil
    (and (= generation (:generation draft)) (same-target? (:source draft) reply))
    (if (dirty? draft)
      ;; Do not rebase concurrent typing onto the newer saved descriptor: a
      ;; later ordinary save would silently overwrite the property change.
      (assoc (if (or (:incoming draft) (contains? draft :incoming-entry) (:remote-deleted? draft)) draft
                 (loaded draft (assoc (:source draft) :entry (:entry reply) :text (:text reply)
                                      :analysis (:analysis reply)))) :property-save-receipt reply)
      (-> (saved draft (:text reply) reply)
          (assoc :text (:text reply) :analysis (:analysis reply) :analysis-text (:text reply))
          (dissoc :rich-base)
          (cond-> (and (= (:entry reply) (:incoming-entry draft)) (not (:remote-deleted? draft)))
            (dissoc :incoming-entry))
          (cond-> (= :rich (:mode draft)) begin-rich)))
    :else (assoc draft :earlier-save-receipt reply)))

(defn response-current? [state slot token]
  (= token (get-in state [:requests slot])))

(defn outline
  ([blocks] (outline blocks 0))
  ([blocks depth]
   (vec (mapcat (fn [b] (cons (assoc b :depth depth) (outline (:children b) (inc depth)))) blocks))))
