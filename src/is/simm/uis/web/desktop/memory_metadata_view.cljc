(ns is.simm.uis.web.desktop.memory-metadata-view
  "Pure per-view reconciliation for independently admitted metadata snapshots."
  (:require [is.simm.documents.attachments :as attachments]
            [is.simm.uis.web.desktop.memory-editor :as editor]
            [is.simm.uis.web.desktop.memory-tree :as tree]))

(defn- file-request [state]
  (let [base (editor/metadata-request state)]
    {:folders (:folders base)
     :paths (vec (sort (set (concat (:paths base) (keys (:file-intents state))
                                    (keys (:create-intents state))
                                    (when-let [path (:selected state)] [path])))))}))

(defn request [state]
  (let [base (file-request state) path (:selected state)
        images (attachments/images path (get-in state [:drafts path :analysis]))
        extra (take (max 0 (- 64 (count (:paths base))))
                    (remove (set (:paths base)) (distinct (map :path images))))]
    (update base :paths #(vec (sort (concat % extra))))))

(defn admission [connection state]
  (let [request (request state) stream-marker (:stream-marker state)
        revision (:discovery-revision state 0)]
    {:request request :connection connection :stream-marker stream-marker
     :revision revision :marker [connection request stream-marker revision]
     :generations (into {} (map (fn [[path draft]] [path (:generation draft)])) (:drafts state))}))

(defn retire-upload [state]
  (cond-> (dissoc state :upload-token :upload-intent)
    (:upload-intent state) (assoc :cancelled-upload (:upload-intent state))))

(defn unverify [state]
  ;; Invalidation owns retirement of scoped bookkeeping. Their late callbacks
  ;; must touch nothing, rather than strand still-marked in-flight requests.
  (let [slots (set (keep (fn [[slot frame]] (when (= :target (:scope frame)) slot))
                         (:request-contexts state)))]
    (-> state retire-upload (assoc :target-verified? false)
        (dissoc :verified-connection :permissions :github-source)
        (update :requests #(apply dissoc % slots))
        (update :request-contexts #(apply dissoc % slots))
        (update :loading #(apply dissoc % slots))
        (update :accepted #(apply dissoc % slots))
        (cond-> (contains? slots :reference) (dissoc :block-reference)))))

(defn retire-reads [state slots]
  (reduce (fn [s slot]
            (-> s (update :requests dissoc slot) (update :request-contexts dissoc slot)
                (update :accepted dissoc slot) (update :finished dissoc slot)
                (assoc-in [:loading slot] false))) state slots))

(defn target-bookkeeping [state]
  (reduce (fn [s k] (update s k editor/keep-evaluation-slots))
          state [:request-contexts :finished :accepted]))

(defn consume
  "Reconcile one admitted observation into one view. Captures belong to dispatch,
   never callback time. This function performs no reads and invokes no callbacks."
  [current reply {:keys [connection stream-marker marker generations]}]
  (when-not (and (= 2 (:protocol-version reply)) (uuid? (:target-stamp reply)))
    (throw (ex-info "Memory protocol changed; reload this browser." {})))
  (let [stamp (:target-stamp reply) changed? (not= stamp (:target-stamp current))
        pending (into {} (filter (fn [[slot loading?]]
                                   (and loading? (vector? slot)
                                        (#{:file :folder :properties :chart :saved-view :office :office-reading} (first slot))))) (:loading current))
        next (cond-> (editor/observe-target current stamp)
               changed? (update :target-epoch (fnil inc 0))
               changed? retire-upload
               changed? target-bookkeeping)
        next (retire-reads next (keys pending))
        next (if (get-in current [:loading :reference])
               (-> next (retire-reads [:reference]) (dissoc :block-reference)) next)
        next (reduce-kv
              (fn [s path entry]
                (if (and (some? (get-in s [:drafts path]))
                         (or changed?
                             (editor/metadata-generation-current? (get generations path)
                                                                  (get-in s [:drafts path :generation]))))
                  (update-in s [:drafts path] editor/observed-entry entry stamp) s))
              next (:entries reply))
        next (reduce-kv
              (fn [s path {:keys [origin text kind]}]
                (if (get-in s [:drafts path]) s
                    (assoc-in s [:drafts path]
                              (cond-> (assoc (editor/draft {:path path :text "" :entry nil
                                                            :kind kind :target-stamp origin})
                                             :text text :mode :source)
                                (not= origin stamp)
                                (assoc :target-stale? true :current-target-stamp stamp)))))
              next (:create-intents current))
        next (reduce-kv
              (fn [s path entry]
                (if (or (some? entry) (not (contains? (set (:paths (file-request current))) path))) s
                    (cond-> (update s :file-intents dissoc path)
                      (nil? (get-in s [:drafts path]))
                      (assoc-in [:errors [:file path]] "No file at this path in the current workspace."))))
              next (:entries reply))]
    (-> next
        (assoc :connection connection :target-verified? true :verified-connection connection
               :permissions (:permissions reply) :sandbox-permissions (:permissions reply)
               :github-source (:github-source reply)
               :workspace (:workspace reply) :definition-basis (:definition-basis reply)
               :metadata-followups pending)
        (update :read-epoch (fnil inc 0))
        (update :observed-entries merge (:entries reply))
        ;; A superseded snapshot may still reconcile
        ;; file receipts, but cannot refill closed caches.
        (update :folders merge (when (= stream-marker (:stream-marker current)) (:folders reply)))
        tree/trim-folder-cache
        (update :file-intents #(apply dissoc % (keys (:create-intents current))))
        (dissoc :create-intents)
        (assoc-in [:sync :attempted] marker)
        (assoc-in [:sync :revision] (:revision reply))
        (assoc-in [:sync :completed] marker))))

(defn followups
  "Plan reads from CURRENT view intents after consuming an admitted snapshot.
   Omitted entries stay unknown; explicit nil never triggers an implicit load."
  [s reply]
  (let [stamp (:target-stamp reply)
      requested-files (set (:paths (file-request s)))
      files (set (concat (keys (:file-intents s))
                         (keep (fn [[[kind p] _]] (when (= :file kind) p)) (:metadata-followups s))
                         (keep (fn [[p entry]]
                                 (let [d (get-in s [:drafts p])]
                                   (when (and entry (contains? requested-files p)
                                              (or (nil? d)
                                                  (not= [stamp entry] [(get-in d [:source :target-stamp])
                                                                       (get-in d [:source :entry])]))
                                              (not= [stamp entry] [(get-in d [:incoming :target-stamp])
                                                                   (get-in d [:incoming :entry])])) p)))
                               (:entries reply))))
      folders (set (concat (keys (:folder-intents s))
                           (keep (fn [[[kind p] _]] (when (= :folder kind) p)) (:metadata-followups s))))]
    {:files (into #{} (remove #(and (contains? (:entries reply) %)
                                    (nil? (get-in reply [:entries %])))) files)
     :folders folders
     :office (vec (for [[[kind p] _] (:metadata-followups s)
                        :when (and (#{:office :office-reading} kind)
                                   (not (contains? files p)) (get-in s [:drafts p])
                                   (not (get-in s [:drafts p :remote-deleted?])))]
                    {:path p :part (when (= :office kind) (get-in s [:drafts p :office :part]))
                     :reading? (= :office-reading kind)}))
     :search? (boolean (seq (:query s))) :histories (vec (keys (:history s)))}))
