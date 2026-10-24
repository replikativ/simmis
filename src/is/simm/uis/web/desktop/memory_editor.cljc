(ns is.simm.uis.web.desktop.memory-editor
  "Serializable, execution-context-owned document drafts and conflict rules."
  (:require [is.simm.documents.rich-text :as rich]
            [is.simm.documents.rich-document :as document]
            [is.simm.uis.web.desktop.memory-cells :as cells]
            [is.simm.uis.web.desktop.memory-tree :as tree]))

(def max-rich-regions 200)

(defn code-view [draft preference]
  (or (:code-view draft) preference :clojure))

(defn repl-syntax [state preference] (or (get-in state [:repl :syntax]) preference :clojure))

(defn repl-input [state syntax] (get-in state [:repl :inputs syntax :text] ""))

(defn file-evaluation-result [state path generation input receipt]
  (let [receipt (assoc receipt :path path :input-text input
                       :stale? (not= generation (get-in state [:drafts path :generation])))
        history (vec (take 20 (cons receipt (:evaluation-history state))))
        retained-paths (conj (set (keep :path history)) path)]
    (-> state
        (update :evaluations #(assoc (select-keys % retained-paths) path receipt))
        (assoc :evaluation-history history))))

(defn repl-result [state receipt]
  (cond-> (update-in state [:repl :history] #(vec (take 20 (cons receipt %))))
    (and (:namespace receipt)
         (= (:requested-namespace receipt) (get-in state [:repl :namespace] "user")))
    (assoc-in [:repl :namespace] (:namespace receipt))))

(defn evaluation-slot? [slot]
  (or (= :repl slot) (and (vector? slot) (#{:evaluate :cell} (first slot)))))

(defn keep-evaluation-slots [entries]
  (into {} (filter (comp evaluation-slot? key)) entries))

(defn evaluation-receipt? [slot reply]
  (and (evaluation-slot? slot)
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

(declare changed begin-rich)

(defn open-local-source [d key]
  (if (:local-source d) d
      (let [base (:rich-base d)
            model (or (:document-model base) (get-in base [:document :model]))
            region (document/region-source (:text base) (:document base) model key)]
        (when-not region (throw (ex-info "Select editable block content first." {})))
        (assoc d :local-source (assoc region :basis-text (:text d) :opened-text (:text region)
                                            :selection {:anchor 0 :head 0} :id (random-uuid))))))

(defn local-source-candidate [d]
  (let [{:keys [basis-text from to text]} (:local-source d)]
    (when-not (and (string? text) (= basis-text (:text d)))
      (throw (ex-info "The document changed while local Markdown was open. Keep your source and reopen against the current draft." {})))
    (str (subs basis-text 0 from) text (subs basis-text to))))

(defn- identity-tree [blocks]
  (mapv (fn [b] [(:id b) (identity-tree (:children b))])
        (filter #(= :explicit (:identity-status %)) blocks)))

(defn local-source-applied [d expected text analysis]
  (when-not (and (= expected (:local-source d)) (= text (local-source-candidate d))
                 (= :ready (get-in analysis [:semantic :status]))
                 (= (:frontmatter (:analysis d)) (:frontmatter analysis))
                 (= (identity-tree (get-in d [:analysis :semantic :blocks]))
                    (identity-tree (get-in analysis [:semantic :blocks]))))
    (throw (ex-info "Local Markdown cannot change document metadata or knowledge-block identities. Your source edit is retained." {})))
  (-> d (changed text {:anchor (:from expected) :head (+ (:from expected) (count (:text expected)))})
      (dissoc :local-source :rich-base)
      (assoc :analysis analysis :analysis-text text :mode :rich)
      begin-rich))

(defn local-source-projected
  "Accept a validated source buffer without closing it or replacing its native
   editor. Rebase its exact span so subsequent keystrokes edit the new draft."
  [d expected text analysis]
  (let [expected (assoc expected :selection (get-in d [:local-source :selection]))
        projected (local-source-applied d expected text analysis)]
    (assoc projected :local-source
           (assoc expected :basis-text text :opened-text (:text expected)
                  :to (+ (:from expected) (count (:text expected)))))))

(defn same-source-buffer? [a b]
  (= (dissoc a :selection) (dissoc b :selection)))

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
    (if (= :markdown (:kind file)) (cells/ensure-projection (begin-rich d)) d)))

(defn dirty? [d]
  (or (and (:source d) (nil? (get-in d [:source :entry])))
      (not= (:text d) (get-in d [:source :text]))
      (not= (get-in d [:office :text]) (get-in d [:office :saved-text]))
      (and (:office-correction d) (not= (get-in d [:office-correction :text]) (get-in d [:office-correction :original])))
      (and (:local-source d) (not= (get-in d [:local-source :text]) (get-in d [:local-source :opened-text])))))

(defn autosave-ready?
  "Automatic writes only follow a new edit, never render/mount. Errors require
   reconciliation and explicit retry; a lost ACK must not become a retry loop."
  [state path]
  (let [d (get-in state [:drafts path])]
    (and (true? (get-in state [:autosave path]))
         (not (get-in state [:autosave-holds path]))
         (string? (:text d)) (not= (:text d) (get-in d [:source :text]))
         (not (:office d)) (not (:earlier-save-receipt d))
         (or (not (:local-source d))
             (= (get-in d [:local-source :text]) (get-in d [:local-source :opened-text])))
         (not (:target-stale? d))
         (not (:remote-deleted? d)) (not (:incoming d)) (not (:incoming-entry d))
         (not (get-in state [:errors [:save path]]))
         (true? (get-in state [:permissions :write?])))))

(defn same-target? [a b]
  (= (:target-stamp a) (:target-stamp b)))

(defn- same-document? [a b]
  (and (same-target? a b) (= (:entry a) (:entry b))))

(defn observe-target
  "Inactive pure target-epoch transition, for the stamped adapter rollout.
   A stamp is identity, not authority. Invalidate file I/O/cache identities even
   for identical bytes, retaining every draft and its original source/Office/
   rich model for explicit conflict handling. Pending human evaluations keep
   their original input/receipt; a new file target never retargets that work."
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
          (update :requests keep-evaluation-slots)
          (update :loading keep-evaluation-slots)
          (update :errors keep-evaluation-slots)
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
    (or (dirty? old) (:rich-edit old) (:local-source old))
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
    ;; An acknowledged autosave followed by a same-file stream refresh is
    ;; observation, not authoritative replacement. Keep the native editor's
    ;; source map, selection and undo owner while refreshing saved analysis.
    (and (:rich-base old) (= (:text old) (:text file))
         (= (select-keys file [:target-stamp :repository-id :path :kind :entry])
            (select-keys (:source old) [:target-stamp :repository-id :path :kind :entry])))
    (-> old (assoc :source file :analysis (:analysis file) :analysis-text (:text file))
        (cond-> (and (uuid? (:target-stamp file)) (some? (:entry file))
                     (= (:target-stamp file) (:current-target-stamp old)))
          (dissoc :target-stale?))
        cells/ensure-projection)
    :else (assoc (merge (draft file) (select-keys old [:mode :selection :code-view :office-mode]))
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
    {:folders (vec (sort (tree/active-folders state)))
     :paths (vec (sort (distinct (concat (keys (:drafts state))
                                         (map second (filter #(= :file (first %)) pending))))))}))

(defn property-entries [properties] (sort-by (comp pr-str key) properties))

(defn property-marker [state path target]
  [(:connection state) (:target-stamp state) (:read-epoch state) (:definition-basis state)
   (get-in state [:observed-entries path]) target])

(defn install-page-properties [state path file]
  (if-let [model (:page-properties file)]
    (let [scope [path {:kind :page}] marker (property-marker state path {:kind :page})]
      (if (= (:entry file) (get-in state [:drafts path :source :entry]))
        (-> state
            (assoc-in [:property-models scope] (assoc model :target-stamp (:target-stamp file) :request-marker marker))
            (assoc-in [:property-attempts scope] marker)) state)) state))

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

(defn office-reading-entry
  "Current saved revision for an inert preview, including an incoming revision
   while an older XML draft stays open. A missing/replaced target never falls
   back to the old saved file."
  [draft]
  (when-not (or (:target-stale? draft) (:remote-deleted? draft) (:incoming-missing? draft)
                (get-in draft [:incoming :deleted?]))
    (or (get-in draft [:incoming :entry]) (:incoming-entry draft) (get-in draft [:source :entry]))))

(defn office-loaded
  "Reading-only responses cannot overwrite XML text, selection or the save
   baseline. View mode is context-backed and survives part switches."
  [draft reply reading-only?]
  (if (and reading-only? (:office draft))
    (update draft :office merge {:reading (:reading reply) :reading-entry (:entry reply)})
    (assoc draft :office (assoc reply :saved-text (:text reply) :reading-entry (:entry reply)))))

(defn office-correction-current? [draft]
  (let [{:keys [entry token path]} (:office-correction draft)]
    (and token (= path (get-in draft [:source :path])) (= entry (office-reading-entry draft))
         (some #{token} (vals (get-in draft [:office :reading :corrections :tokens])))
         (= (get-in draft [:office :text]) (get-in draft [:office :saved-text])))))

(defn office-corrected
  "Only a still-current, unchanged correction may advance its file baseline.
   Newer text/XML edits survive an ACK against their original revision."
  [draft generation sent reply]
  (when draft
    (if (and (= generation (:generation draft)) (same-target? (:source draft) reply)
             (= sent (:office-correction draft))
             (= (get-in draft [:office :text]) (get-in draft [:office :saved-text])))
      (-> draft (dissoc :office :office-correction)
          (assoc-in [:source :entry] (:entry reply))
          (assoc :last-commit (:commit reply)) (update :generation inc)
          (cond-> (same-document? reply (:incoming draft)) (dissoc :incoming)
            (and (same-target? reply (:source draft)) (= (:entry reply) (:incoming-entry draft))) (dissoc :incoming-entry)))
      (assoc draft :earlier-save-receipt reply))))

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
  (let [d (cond-> d
            (and (= text (get-in d [:source :text]))
                 (= (:revision analysis) (get-in d [:source :entry :content]))
                 (uuid? (:revision analysis)))
            (assoc-in [:source :analysis] analysis))]
    (cells/ensure-projection (if (= text (:text d))
      (cond-> (assoc d :analysis analysis :analysis-text text)
        (= :rich (:mode d)) begin-rich) d))))

(defn saved [d sent reply]
  (-> d
      (assoc-in [:source :entry] (:entry reply))
      (assoc-in [:source :text] sent)
      (assoc :last-commit (:commit reply))
      (update :generation (fnil inc 0))
      (cond-> (same-document? reply (:incoming d)) (dissoc :incoming)
        (and (same-target? reply (:source d)) (= (:entry reply) (:incoming-entry d)))
        (dissoc :incoming-entry))
      cells/ensure-projection))

(defn moved
  "Move the acknowledged baseline, retaining concurrent typing. Observations of
   the removed old path are not conflicts against the new destination."
  [d reply]
  (-> d (assoc-in [:source :path] (:path reply))
      (assoc-in [:source :entry] (:entry reply))
      (assoc :last-commit (:commit reply))
      (update :generation (fnil inc 0))
      (dissoc :incoming :incoming-entry :observed-entry :remote-deleted?)))

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
