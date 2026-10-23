(ns is.simm.uis.web.desktop.memory-actions
  "Context/session-owned stamped Memory I/O. A stamp is a precondition, never
   authority. Generic Room SCI keeps its separate selected-workspace scope."
  (:require [clojure.string :as str]
            [org.replikativ.spindel.engine.core :as ec]
            [is.simm.uis.web.desktop.runtime :refer [runtime]]
            [is.simm.uis.web.desktop.signals :as sig]
            [is.simm.uis.web.desktop.memory-editor :as editor]
            [is.simm.uis.web.desktop.memory-declarations :as declarations]
            [is.simm.uis.web.desktop.memory-sync :as sync]
            [is.simm.uis.web.desktop.memory-remote :as remote]
            [is.simm.uis.web.desktop.chart-remote :as chart-remote]
            [is.simm.uis.web.desktop.memory-charts :as charts]
            [is.simm.uis.web.desktop.remote :as rem]
            [is.simm.runtimes.web :as web]))

(defn state-key [data]
  [(:id (web/current-session)) (str (:room-id data)) (:col-id data) (:tab-id data)])

(defn- parent [path] (str/join "/" (butlast (str/split path #"/"))))

(defn verified? [state connection]
  (and (some? connection) (true? (:target-verified? state))
       (= connection (:verified-connection state)) (uuid? (:target-stamp state))))

(defn- draft-current? [state path]
  (let [draft (get-in state [:drafts path])]
    (and draft (not (:target-stale? draft))
         (= (:target-stamp state) (get-in draft [:source :target-stamp])))))

(defn- retire-upload [state]
  (cond-> (dissoc state :upload-token :upload-intent)
    (:upload-intent state) (assoc :cancelled-upload (:upload-intent state))))

(defn- unverify [state]
  ;; Invalidation owns retirement of scoped bookkeeping. Their late callbacks
  ;; must touch nothing, rather than strand still-marked in-flight requests.
  (let [slots (set (keep (fn [[slot frame]] (when (= :target (:scope frame)) slot))
                         (:request-contexts state)))]
    (-> state retire-upload (assoc :target-verified? false)
        (dissoc :verified-connection :permissions)
        (update :requests #(apply dissoc % slots))
        (update :request-contexts #(apply dissoc % slots))
        (update :loading #(apply dissoc % slots))
        (update :accepted #(apply dissoc % slots)))))

(defn- retire-reads [state slots]
  (reduce (fn [s slot]
            (-> s (update :requests dissoc slot) (update :request-contexts dissoc slot)
                (update :accepted dissoc slot) (update :finished dissoc slot)
                (assoc-in [:loading slot] false))) state slots))

(defn- target-bookkeeping [state]
  (reduce (fn [s k] (update s k #(select-keys % [:repl])))
          state [:request-contexts :finished :accepted]))

(defn controller [data]
  (let [ctx (or ec/*execution-context* runtime) session (web/current-session)
        room (str (:room-id data)) key (state-key data)
        state-signal (or (:state-signal data) sig/memory-editor-state)
        current? #(binding [ec/*execution-context* ctx]
                    (and (:id session) (web/current-session? session)))
        state #(binding [ec/*execution-context* ctx] (get @state-signal key))
        connection #(binding [ec/*execution-context* ctx] @sig/memory-connection)
        update! (fn [f & args]
                  (binding [ec/*execution-context* ctx]
                    (when (current?) (apply swap! state-signal update key f args))))
        change! (fn [path f & args] (apply update! update-in [:drafts path] f args))]
    (letfn [(connected! []
              (let [c (connection)]
                (when (and (current?) (not= c (:connection (state))))
                  (update! #(-> % retire-upload (assoc :connection c :target-verified? false)
                                (update :target-epoch (fnil inc 0))
                                (assoc :requests {} :request-contexts {} :loading {} :accepted {} :finished {})
                                (dissoc :verified-connection :permissions :sandbox-permissions
                                        :watch-connection :sync))))
                c))
            (frame-current? [s frame]
              (and (= (:connection frame) (:connection s))
                   (or (not= :target (:scope frame))
                       (and (verified? s (:connection frame))
                            (= (:stamp frame) (:target-stamp s))
                            (= (:target-epoch frame) (:target-epoch s))
                            (or (not (:read? frame)) (= (:read-epoch frame) (:read-epoch s)))))))
            (invalidate! []
              (update! unverify))
            (request! [slot invoke consume after opts]
              (let [c (connected!) token (random-uuid)
                    frame {:scope (:scope opts :target) :connection c :stamp (:stamp opts)
                           :target-epoch (:target-epoch (state)) :read? (:read? opts)
                           :read-epoch (:read-epoch (state))}
                    live? #(and (current?) (= c (connection)))
                    owns? #(and (editor/response-current? % slot token)
                                (not= token (get-in % [:finished slot])) (frame-current? % frame))
                    settled! (fn []
                               (when (and (:on-settled opts) (live?)
                                          (= token (get-in (state) [:requests slot]))
                                          (= token (get-in (state) [:finished slot]))
                                          (frame-current? (state) frame))
                                 (binding [ec/*execution-context* ctx] ((:on-settled opts)))))
                    fail! (fn [error]
                            (when (and (live?) (owns? (state)))
                              (let [type (:type (rem/error-info error))]
                                (update! (fn [s]
                                           (if-not (owns? s) s
                                                   (if (and (:error-current? opts) (not ((:error-current? opts) s)))
                                                     (-> s (assoc-in [:loading slot] false) (assoc-in [:finished slot] token))
                                                     (let [failed (cond-> (-> s (assoc-in [:loading slot] false)
                                                                              (assoc-in [:finished slot] token)
                                                                              (assoc-in [:errors slot] (rem/error-text error)))
                                                                    (= :metadata slot) unverify
                                                                    (#{:authentication-required :not-authorized} type)
                                                                    (-> unverify (dissoc :sandbox-permissions)))]
                                                       (if-let [f (:failure-state opts)] (f failed error) failed))))))
                                (when (and (= :memory/target-changed type) (not= :metadata slot))
                                  (invalidate!)
                                  (discover! true))
                                (settled!))))]
                (when (and c (live?) (frame-current? (state) frame))
                  (update! #(-> % (assoc-in [:requests slot] token)
                                (assoc-in [:request-contexts slot] frame)
                                (assoc-in [:loading slot] true)
                                (update :accepted dissoc slot) (update :finished dissoc slot) (update :errors dissoc slot)))
                  (try
                    (binding [ec/*execution-context* ctx ec/*spin-id* nil]
                      ((invoke)
                       (fn [reply]
                         (when (and (live?) (owns? (state)))
                           (if-let [error (when-not (editor/evaluation-receipt? slot reply)
                                            (rem/result-failure reply))]
                             (fail! error)
                             (try
                               (when (and (= :target (:scope frame))
                                          (not= (:stamp frame) (:target-stamp reply)))
                                 (throw (ex-info "Memory returned a different workspace; Refresh to verify it." {})))
                               (update! (fn [s]
                                          (if-not (owns? s) s
                                                  (if (and (:accept? opts) (not ((:accept? opts) s reply)))
                                              ;; Still our request, but its draft
                                              ;; changed: finish bookkeeping only.
                                                    (-> s (assoc-in [:loading slot] false) (assoc-in [:finished slot] token))
                                                    (cond-> (-> (consume s reply)
                                                                (assoc-in [:requests slot] token)
                                                                (assoc-in [:loading slot] false)
                                                                (assoc-in [:finished slot] token)
                                                                (assoc-in [:accepted slot] token))
                                                      (and (#{:watch :sandbox} (:scope frame)) (:permissions reply))
                                                      (assoc :sandbox-permissions (:permissions reply)))))))
                               ;; A truthful acceptance marker, not swap!'s
                               ;; always-truthy whole signal return value.
                               (when (and after (live?) (= token (get-in (state) [:accepted slot]))
                                          (frame-current? (state) frame))
                                 (try (binding [ec/*execution-context* ctx] (after reply))
                                      (catch :default error
                                        (when (and (live?) (= token (get-in (state) [:accepted slot]))
                                                   (frame-current? (state) frame))
                                          (update! assoc-in [:errors slot] (rem/error-text error))))))
                               (settled!)
                               (catch :default error (fail! error))))))
                       fail!))
                    (catch :default error (fail! error))))))
            (metadata-request [s]
              (let [request (editor/metadata-request s)]
                {:folders (vec (sort (set (concat (:folders request) [""]
                                                  (keep (fn [[p open?]] (when open? p)) (:expanded s))
                                                  (keys (:folder-intents s))))))
                 :paths (vec (sort (set (concat (:paths request) (keys (:file-intents s))
                                                (keys (:create-intents s))
                                                (when-let [p (:selected s)] [p])))))}))
            (discover! [force?]
              (when force? (update! update :discovery-revision (fnil inc 0)))
              (let [c (connected!) s (state)
                    request (metadata-request s)
                    marker [c request (:stream-marker s)
                            (:discovery-revision s 0)]]
                (when (and c (current?) (not (get-in s [:loading :metadata]))
                           (or force? (not= marker (get-in s [:sync :attempted]))))
                  (update! assoc-in [:sync :attempted] marker)
                  (if (or (> (count (:folders request)) 64) (> (count (:paths request)) 64))
                    (do (invalidate!)
                        (update! assoc-in [:errors :metadata]
                                 "Memory supports 64 expanded folders and file buffers per view. Collapse folders or close buffers, then Refresh."))
                    (let [generations (into {} (map (fn [[p d]] [p (:generation d)])) (:drafts s))
                          revision (:discovery-revision s 0)]
                      (request!
                       :metadata #(remote/load-memory-metadata! web/server-id room request)
                       (fn [current reply]
                         (when-not (and (= 2 (:protocol-version reply)) (uuid? (:target-stamp reply)))
                           (throw (ex-info "Memory protocol changed; reload this browser." {})))
                         (let [stamp (:target-stamp reply) changed? (not= stamp (:target-stamp current))
                               pending (into {} (filter (fn [[slot loading?]]
                                                          (and loading? (vector? slot)
                                                               (#{:file :folder :properties :chart :office :office-reading} (first slot))))) (:loading current))
                               next (cond-> (editor/observe-target current stamp)
                                      changed? (update :target-epoch (fnil inc 0))
                                      changed? retire-upload
                                      changed? target-bookkeeping)
                               next (retire-reads next (keys pending))
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
                                       (if (some? entry) s
                                           (cond-> (update s :file-intents dissoc path)
                                             (nil? (get-in s [:drafts path]))
                                             (assoc-in [:errors [:file path]] "No file at this path in the current workspace."))))
                                     next (:entries reply))]
                           (-> next
                               (assoc :connection c :target-verified? true :verified-connection c
                                      :permissions (:permissions reply) :sandbox-permissions (:permissions reply)
                                      :workspace (:workspace reply) :definition-basis (:definition-basis reply)
                                      :metadata-followups pending)
                               (update :read-epoch (fnil inc 0))
                               (update :observed-entries merge (:entries reply))
                               (update :folders merge (:folders reply))
                               (update :file-intents #(apply dissoc % (keys (:create-intents current))))
                               (dissoc :create-intents)
                               (assoc-in [:sync :attempted] marker)
                               (assoc-in [:sync :revision] (:revision reply))
                               (assoc-in [:sync :completed] marker))))
                       (fn [reply]
                         ;; Preserve the browser-proved omitted-path fix: use
                         ;; CURRENT pending reads, not metadata's old request.
                         (let [s (state) stamp (:target-stamp reply)
                               files (set (concat (keys (:file-intents s))
                                                  (keep (fn [[[kind p] _]] (when (= :file kind) p)) (:metadata-followups s))
                                                  (keep (fn [[p entry]]
                                                          (let [d (get-in s [:drafts p])]
                                                            (when (and entry
                                                                       (or (nil? d)
                                                                           (not= [stamp entry] [(get-in d [:source :target-stamp])
                                                                                                (get-in d [:source :entry])]))
                                                                       (not= [stamp entry] [(get-in d [:incoming :target-stamp])
                                                                                            (get-in d [:incoming :entry])])) p)))
                                                        (:entries reply))))
                               folders (set (concat (keys (:folder-intents s))
                                                    (keep (fn [[[kind p] _]] (when (= :folder kind) p)) (:metadata-followups s))))]
                           (update! dissoc :metadata-followups)
                           (doseq [p files]
                             (when-not (and (contains? (:entries reply) p) (nil? (get-in reply [:entries p])))
                               (open! p true nil true)))
                           (doseq [[[kind p] _] (:metadata-followups s)
                                   :when (and (#{:office :office-reading} kind) (not (contains? files p))
                                              (get-in s [:drafts p]) (not (get-in s [:drafts p :remote-deleted?])))]
                             (office-open! p (when (= :office kind) (get-in s [:drafts p :office :part]))
                                           (= :office-reading kind)))
                           (doseq [p folders] (folder! p))
                           (when (seq (:query (state))) (search!))
                           (doseq [p (keys (:history s))] (history! p))))
                       {:scope :discovery
                        :accept? (fn [s _] (= revision (:discovery-revision s 0)))
                        :error-current? (fn [s] (= revision (:discovery-revision s 0)))
                        :on-settled (fn []
                                      (when (not= revision (:discovery-revision (state) 0))
                                        (discover! false)))}))))))
            (target! [slot]
              (connected!)
              (if (verified? (state) (connection)) (:target-stamp (state))
                  (do (update! assoc-in [:errors slot] "Verify the current workspace with Refresh before this action.")
                      (discover! false) nil)))
            (draft-stamp! [path slot]
              (when (target! slot)
                (if (draft-current? (state) path) (get-in (state) [:drafts path :source :target-stamp])
                    (do (update! assoc-in [:errors slot] "This draft belongs to another workspace. Choose its current copy explicitly before publishing.") nil))))
            (generation? [path generation]
              (fn [s _] (= generation (get-in s [:drafts path :generation]))))
            (navigation-refresh! []
              (when-not (= :navigation (:col-id data))
                ((:refresh (controller {:room-id (:room-id data) :col-id :navigation
                                        :state-signal sig/memory-navigation-state})))))
            (folder! [path]
              (update! assoc-in [:folder-intents path] true)
              (if-let [stamp (when (verified? (state) (connected!)) (:target-stamp (state)))]
                (request! [:folder path] #(remote/load-memory-folder! web/server-id room
                                                                      {:folder path :expected-target-stamp stamp})
                          #(-> %1 (assoc-in [:folders path] %2) (update :folder-intents dissoc path))
                          nil {:stamp stamp :read? true})
                (discover! false)))
            (open! [path refresh? & [observed passive?]]
              (when-not (or observed passive?) (update! assoc :selected path))
              (when (or refresh? (nil? (get-in (state) [:drafts path])))
                (update! assoc-in [:file-intents path] true)
                (if-let [stamp (when (verified? (state) (connected!)) (:target-stamp (state)))]
                  (let [generation (get-in (state) [:drafts path :generation])]
                    (request! [:file path] #(remote/load-memory-file! web/server-id room
                                                                      {:path path :expected-target-stamp stamp})
                              #(-> %1 (update-in [:drafts path] editor/loaded-at-generation generation %2)
                                   (update :file-intents dissoc path))
                              (fn [_]
                                (let [draft (get-in (state) [:drafts path])]
                                  (when (= :office (get-in draft [:source :kind]))
                                    (office-open! path nil (boolean (:office draft))))))
                              {:stamp stamp :read? true :accept? (generation? path generation)}))
                  (discover! false))))
            (analyze! [path & [saved-baseline?]]
              (let [draft (get-in (state) [:drafts path]) text (:text draft)]
                (when-let [stamp (and (string? text) (draft-stamp! path [:analysis path]))]
                  (request! [:analysis path] #(remote/analyze-memory-text! web/server-id room
                                                                           {:path path :text text :expected-target-stamp stamp})
                            #(update-in %1 [:drafts path] editor/analyzed text %2) nil
                            {:stamp stamp :accept? (fn [s _] (and (= (:generation draft) (get-in s [:drafts path :generation]))
                                                                  (or (= text (get-in s [:drafts path :text]))
                                                                      (and saved-baseline?
                                                                           (= text (get-in s [:drafts path :source :text]))))))}))))
            (property-marker [s path target]
              [(:connection s) (:target-stamp s) (:read-epoch s) (:definition-basis s)
               (get-in s [:observed-entries path]) target])
            (chart! [path block]
              (let [s (state) marker (charts/marker s path block) scope [path block]
                    slot [:chart path block] draft (get-in s [:drafts path])]
                (when (and (verified? s (connected!)) (draft-current? s path)
                           (not (editor/dirty? draft)) (not= :source (:mode draft))
                           (not (get-in s [:loading slot]))
                           (not= marker (get-in s [:chart-attempts scope])))
                  (update! assoc-in [:chart-attempts scope] marker)
                  (request! slot #(chart-remote/load-memory-chart! web/server-id room (charts/request s path block))
                            #(assoc-in %1 [:charts scope] {:marker marker :reply %2}) nil
                            {:stamp (:target-stamp s) :read? true
                             :accept? #(and (not (editor/dirty? (get-in %1 [:drafts path])))
                                            (charts/reply-current? %1 path block marker %2))
                             :failure-state (fn [current _]
                                              (-> current (update :errors dissoc slot)
                                                  (assoc-in [:charts scope] {:marker marker :status :unavailable})))}))))
            (properties! [path target]
              (let [s (state) marker (property-marker s path target) scope [path target]
                    slot [:properties path target]]
                (when (and (verified? s (connected!)) (get-in s [:drafts path :source :entry])
                           (not (get-in s [:loading slot]))
                           (not= marker (get-in s [:property-attempts scope])))
                  (update! assoc-in [:property-attempts scope] marker)
                  (request! slot #(remote/load-memory-properties! web/server-id room
                                                                  {:path path :target target :expected-target-stamp (:target-stamp s)})
                            #(assoc-in %1 [:property-models scope] (assoc %2 :request-marker marker)) nil
                            {:stamp (:target-stamp s) :read? true}))))
            (property-save! [path target field]
              (let [s (state) scope [path target] model (get-in s [:property-models scope])
                    pd (get-in s [:property-drafts scope (:id field)]) draft (get-in s [:drafts path])
                    slot [:property-save path] input (editor/property-input model field pd)]
                (when (and (not (get-in s [:loading slot])) (not (get-in s [:loading [:save path]]))
                           (not (get-in s [:loading [:move path]]))
                           (:editable? field) (not (editor/dirty? draft)) (not (declarations/drafts? s path))
                           (= (:request-marker model) (property-marker s path target))
                           (= (:expected model) (get-in draft [:source :entry]))
                           (editor/property-draft-current? model pd))
                  (when-let [stamp (draft-stamp! path slot)]
                    (let [command (-> (editor/property-baseline model) (dissoc :target-stamp)
                                      (assoc :expected-target-stamp stamp :property (:id field) :alias (:alias field) :input input))]
                      (request! slot #(remote/save-memory-property! web/server-id room command)
                                (fn [current reply]
                                  (cond-> (-> current (update :property-models dissoc scope)
                                              (update :property-attempts dissoc scope))
                                    (some? (get-in current [:drafts path]))
                                    (update-in [:drafts path] editor/property-saved (:generation draft) reply)
                                    (= pd (get-in current [:property-drafts scope (:id field)]))
                                    (update-in [:property-drafts scope] dissoc (:id field))))
                                (fn [_] (discover! true) (navigation-refresh!)) {:stamp stamp}))))))
            (declaration-preview! [path target]
              (let [s (state) scope [path target] dd (get-in s [:declaration-drafts scope])
                    model (get-in s [:property-models scope]) generation (:generation dd)]
                (when (and dd (verified? s (connected!)) (declarations/current? dd model)
                           (= (:request-marker model) (property-marker s path target)))
                  (request! [:declaration-preview path target]
                            #(remote/preview-memory-declaration! web/server-id room (declarations/preview-request dd))
                            (fn [current reply]
                              (if-let [draft (get-in current [:declaration-drafts scope])]
                                (assoc-in current [:declaration-drafts scope] (declarations/previewed draft generation reply))
                                current))
                            nil {:stamp (get-in dd [:baseline :target-stamp]) :read? true}))))
            (declaration-save! [path target]
              (let [s (state) scope [path target] dd (get-in s [:declaration-drafts scope])
                    model (get-in s [:property-models scope]) draft (get-in s [:drafts path])
                    slot [:property-save path] command (declarations/command dd)]
                (when (and command (declarations/current? dd model)
                           (= (:request-marker model) (property-marker s path target))
                           (= (:expected model) (get-in draft [:source :entry]))
                           (not (editor/dirty? draft)) (not (editor/property-drafts? s path))
                           (not (declarations/busy? s path)))
                  (when-let [stamp (draft-stamp! path slot)]
                    (request! slot #(remote/save-memory-declaration! web/server-id room command)
                              (fn [current reply]
                                (cond-> (-> current (update :property-models dissoc scope)
                                            (update :property-attempts dissoc scope))
                                  (some? (get-in current [:drafts path]))
                                  (update-in [:drafts path] editor/property-saved (:generation draft) reply)
                                  (= dd (get-in current [:declaration-drafts scope]))
                                  (update :declaration-drafts dissoc scope)))
                              (fn [_] (discover! true) (navigation-refresh!)) {:stamp stamp})))))
            (apply-local-source! [path]
              (let [draft (get-in (state) [:drafts path]) local (:local-source draft) slot [:local-source path]]
                (when-not (get-in (state) [:loading slot])
                  (try
                    (let [text (editor/local-source-candidate draft)]
                      (when-let [stamp (draft-stamp! path slot)]
                        (request! slot #(remote/analyze-memory-text! web/server-id room
                                                                    {:path path :text text :expected-target-stamp stamp})
                                  #(-> %1 (update-in [:drafts path] editor/local-source-projected local text %2)
                                       (update :errors dissoc [:rich path])
                                       (assoc-in [:autosave path] true))
                                  (fn [_] (autosave! path))
                                  {:stamp stamp :accept? (fn [s _]
                                                              (and (editor/same-source-buffer? local (get-in s [:drafts path :local-source]))
                                                                   (= (:text draft) (get-in s [:drafts path :text]))))
                                                   :error-current? (fn [s] (editor/same-source-buffer? local (get-in s [:drafts path :local-source])))
                                                   :on-settled (fn []
                                                                   (let [next (get-in (state) [:drafts path :local-source])]
                                                                     ;; Drain only a newer buffer. Invalid input
                                                                     ;; does not repeatedly validate itself.
                                                                     (when (and next (not (editor/same-source-buffer? local next))
                                                                                (not= (:text next) (:opened-text next)))
                                                                       (apply-local-source! path))))})))
                    (catch :default error (update! assoc-in [:errors slot] (rem/error-text error)))))))
            (save! [path]
              (let [draft (get-in (state) [:drafts path]) sent (:text draft) slot [:save path]]
                (when (and (not (get-in (state) [:loading slot])) (not (get-in (state) [:loading [:move path]]))
                           (not (get-in (state) [:loading [:property-save path]]))
                           (or (not (:local-source draft))
                               (= (get-in draft [:local-source :text]) (get-in draft [:local-source :opened-text])))
                           (not (declarations/drafts? (state) path)))
                  (when-let [stamp (draft-stamp! path slot)]
                    (let [request {:path path :expected (get-in draft [:source :entry]) :text sent :expected-target-stamp stamp}
                          base (:rich-base draft)]
                      ;; Persist the pause in context state before dispatch. A
                      ;; disconnect retires request bookkeeping, not this hold.
                      ;; Only an accepted save receipt releases it.
                      (update! assoc-in [:autosave-holds path] true)
                      (request! slot
                                #(if base
                                   (remote/save-memory-rich-file! web/server-id room
                                                                  (assoc request :source (:text base)
                                                                         :revision (get-in base [:analysis :revision]) :edits (editor/document-edits draft)))
                                   (remote/save-memory-file! web/server-id room request))
                                #(-> %1 (update-in [:drafts path] editor/saved-at-generation (:generation draft) sent %2)
                                     (update :autosave-holds dissoc path))
                                (fn [_] (folder! (parent path)) (navigation-refresh!))
                                {:stamp stamp :accept? (generation? path (:generation draft))
                                 :on-settled #(autosave! path)}))))))
            (autosave! [path]
              ;; The request slot is the single-flight coordinator. An accepted
              ;; ACK advances the baseline before draining the latest draft.
              ;; Failed requests leave an error and cannot automatically retry.
              (when (and (some? (ec/home-context ctx))
                         (identical? (ec/home-context ctx) (ec/home-context runtime))
                         (verified? (state) (connection))
                         (editor/autosave-ready? (state) path)
                         (not (editor/property-drafts? (state) path)))
                (save! path)))
            (edited! [path f & args]
              (let [before (get-in (state) [:drafts path :text])]
                (apply change! path f args)
                (when (not= before (get-in (state) [:drafts path :text]))
                  (update! assoc-in [:autosave path] true)
                  (autosave! path))))
            (create! [path origin]
              (if-not (and (string? path) (not (str/blank? path)) (uuid? origin))
                (do (update! assoc-in [:errors :create] "Verify the initiating workspace before creating a file.") (discover! false))
                (let [markdown? (re-find #"(?i)\.(md|markdown)$" path)
                      intent {:origin origin :kind (if markdown? :markdown :text)
                              :text (if markdown? (str "---\nsimmis-id: '" (random-uuid) "'\n---\n# Untitled\n\n") "")}]
                  (update! #(-> % (assoc :selected path) (assoc-in [:create-intents path] intent)))
                  (discover! true))))
            (download! [path]
              (when-let [stamp (draft-stamp! path [:download path])]
                (let [draft (get-in (state) [:drafts path])]
                  (request! [:download path] #(remote/download-memory-file! web/server-id room
                                                                            {:path path :expected (get-in draft [:source :entry]) :expected-target-stamp stamp})
                            #(assoc %1 :download %2) nil {:stamp stamp :accept? (generation? path (:generation draft))}))))
            (upload! [folder file]
              (when-let [stamp (target! :upload)]
                (let [path (str (when (seq folder) (str folder "/")) (.-name file))
                      c (connection) epoch (:target-epoch (state)) token (random-uuid)
                      valid? #(and (current?) (= c (connection)) (verified? (state) c)
                                   (= stamp (:target-stamp (state))) (= epoch (:target-epoch (state)))
                                   (= token (:upload-token (state))))]
                  (update! #(-> % (assoc :upload-token token :upload-intent {:path path :target-stamp stamp})
                                (dissoc :cancelled-upload)))
                  (if (> (.-size file) (* 8 1024 1024))
                    (update! #(-> % (dissoc :upload-token :upload-intent)
                                  (assoc-in [:errors :upload] "File exceeds the 8 MiB interactive upload limit.")))
                    (-> (.arrayBuffer file)
                        (.then (fn [buffer]
                                 (when (valid?)
                                   (let [bytes (js/Uint8Array. buffer)
                                         chunks (for [from (range 0 (.-length bytes) 16384)]
                                                  (.apply (.-fromCharCode js/String) nil
                                                          (.subarray bytes from (min (+ from 16384) (.-length bytes)))))
                                         payload (js/btoa (apply str chunks))]
                                     (when (valid?)
                                       (request! [:upload path]
                                                 #(remote/save-memory-file! web/server-id room
                                                                            {:path path :expected nil :base64 payload :expected-target-stamp stamp})
                                                 (fn [s _] (dissoc s :upload-token :upload-intent))
                                                 (fn [_] (folder! folder) (navigation-refresh!) (open! path true))
                                                 {:stamp stamp
                                                  :accept? (fn [s _] (= token (:upload-token s)))
                                                  :error-current? (fn [s] (= token (:upload-token s)))
                                                  :failure-state (fn [s _]
                                                                   (if (= token (:upload-token s))
                                                                     (dissoc s :upload-token :upload-intent) s))}))))))
                        (.catch (fn [error]
                                  (when (valid?)
                                    (update! #(-> % (dissoc :upload-token :upload-intent)
                                                  (assoc-in [:errors :upload] (rem/error-text error))))))))))))
            (office-open! [path part & [reading-only?]]
              (let [draft (get-in (state) [:drafts path])
                    expected (if reading-only? (editor/office-reading-entry draft) (get-in draft [:source :entry]))
                    slot [(if reading-only? :office-reading :office) path]]
                (if (or (and (not reading-only?) (editor/dirty? draft)) (get-in (state) [:loading [:office-save path]]))
                  (update! assoc-in [:errors :office] "Save or discard this XML draft before switching parts.")
                  (when-let [stamp (when expected (draft-stamp! path slot))]
                    (request! slot #(remote/load-memory-office-part! web/server-id room
                                                                               {:path path :expected expected
                                                                                :part part :expected-target-stamp stamp})
                              #(update-in %1 [:drafts path] editor/office-loaded %2 reading-only?) nil
                              {:stamp stamp :read? true
                               :accept? (fn [s reply]
                                          (let [current (get-in s [:drafts path])]
                                            (and (= expected (:entry reply))
                                                 (= (:generation draft) (:generation current))
                                                 (= expected (if reading-only? (editor/office-reading-entry current) (get-in current [:source :entry])))
                                                 (or reading-only? (not (editor/dirty? current))))))})))))
            (office-save! [path]
              (let [draft (get-in (state) [:drafts path]) office (:office draft) sent (:text office)]
                (when (and (not (get-in (state) [:loading [:office-save path]])) (not (get-in (state) [:loading [:move path]])))
                  (when-let [stamp (draft-stamp! path [:office-save path])]
                    (request! [:office-save path] #(remote/save-memory-office-part! web/server-id room
                                                                                    {:path path :expected (get-in draft [:source :entry])
                                                                                     :part (:part office) :text sent :expected-target-stamp stamp})
                              #(update-in %1 [:drafts path]
                                          (fn [d] (-> d (assoc-in [:source :entry] (:entry %2))
                                                      (assoc-in [:office :saved-text] sent)
                                                      (assoc :last-commit (:commit %2)) (update :generation inc))))
                              (fn [_]
                                (folder! (parent path)) (navigation-refresh!)
                                (office-open! path nil true))
                              {:stamp stamp :accept? (fn [s _] (and (= (:generation draft) (get-in s [:drafts path :generation]))
                                                                    (= (:part office) (get-in s [:drafts path :office :part]))))})))))
            (history! [path]
              (when-let [stamp (draft-stamp! path [:history path])]
                (let [generation (get-in (state) [:drafts path :generation])]
                  (request! [:history path] #(remote/load-memory-history! web/server-id room {:path path :expected-target-stamp stamp})
                            #(assoc-in %1 [:history path] %2) nil {:stamp stamp :accept? (generation? path generation)}))))
            (search! []
              (let [query (:query (state))]
                (when-let [stamp (and (not (str/blank? query)) (target! :search))]
                  (request! :search #(remote/search-memory-files! web/server-id room {:query query :expected-target-stamp stamp})
                            #(assoc %1 :search %2) nil {:stamp stamp :accept? (fn [s _] (= query (:query s)))}))))
            (ensure-sync! [c stream]
              (connected!)
              (when (and c (= c (connection)) (current?))
                (let [marker [(:connection stream) (:status stream) (get-in stream [:dirty room] 0)]]
                  (when (not= marker (:stream-marker (state)))
                    (update! assoc :stream-marker marker)))
                (when (not= c (:watch-connection (state)))
                  (update! assoc :watch-connection c)
                  (request! :watch #(remote/prepare-memory-stream! web/server-id room) (fn [s _] s)
                            (fn [_]
                              (if (:retry-stream? (state))
                                (do (update! dissoc :retry-stream?) (sync/retry! c))
                                (sync/ensure! c))) {:scope :watch}))
                ;; Discovery is NOT gated by subscription readiness.
                (discover! false)))
            (refresh! []
              (connected!)
              (update! #(-> % (assoc :errors {} :retry-stream? true)
                            (dissoc :watch-connection)))
              (discover! true)
              (when-let [c (connection)]
                (ensure-sync! c (binding [ec/*execution-context* ctx] @sync/state)))
              (navigation-refresh!))]
      {:owner key :state state :change change! :edit edited! :folder folder! :open #(open! % false) :reload #(open! % true)
       :ensure-charts (fn []
                        (let [s (state) path (:selected s) draft (get-in s [:drafts path])
                              marker (charts/analysis-marker s path)]
                          ;; Room change notifications advance read-epoch, thus
                          ;; invalidating ALL displayed charts, including spec-
                          ;; only commits whose JSON file is not an open buffer.
                          (if (charts/saved-analysis draft)
                            (doseq [block (charts/blocks draft)] (chart! path block))
                            (when (and (verified? s (connected!)) (draft-current? s path)
                                       (= :markdown (get-in draft [:source :kind]))
                                       (not= :source (:mode draft)) (not (editor/dirty? draft))
                                       (not (get-in s [:loading [:analysis path]]))
                                       (not= marker (get-in s [:chart-analysis-attempts path])))
                              ;; Failed optional analysis is attempted once per
                              ;; saved baseline, never retried by its own render.
                              (update! assoc-in [:chart-analysis-attempts path] marker)
                              (analyze! path true)))))
       :chart-models #(charts/visible (state) %)
       :open-properties (fn [path target] (update! assoc-in [:property-targets path] target) (properties! path target))
       :ensure-properties
       (fn []
         (let [s (state) path (:selected s)
               targets (if (= :markdown (get-in s [:drafts path :source :kind]))
                         (declarations/visible-targets s path)
                         (when-let [target (get-in s [:property-targets path])] [target]))]
           (doseq [target targets] (properties! path target))))
       :property-input (fn [path target field input]
                         (let [scope [path target] model (get-in (state) [:property-models scope])]
                           (update! update-in [:property-drafts scope (:id field)]
                                    #(if % (assoc % :input input)
                                         (assoc (editor/property-draft model input)
                                                :alias (:alias field) :label (:label field))))))
       :property-discard (fn [path target id] (update! update-in [:property-drafts [path target]] dissoc id))
       :property-current? (fn [path target model] (= (:request-marker model) (property-marker (state) path target)))
       :property-save property-save!
       :declaration-open
       (fn [path target]
         (let [s (state) scope [path target] model (get-in s [:property-models scope])]
           (when (and (declarations/openable? s path model) (verified? s (connected!))
                      (= (:request-marker model) (property-marker s path target)))
             (update! assoc-in [:declaration-drafts scope] (declarations/draft model))
             (declaration-preview! path target))))
       :declaration-types (fn [path target types]
                            (update! update-in [:declaration-drafts [path target]] declarations/set-types types)
                            (declaration-preview! path target))
       :declaration-bind (fn [path target field alias]
                           (update! update-in [:declaration-drafts [path target]] declarations/add-binding field alias)
                           (declaration-preview! path target))
       :declaration-unbind (fn [path target id]
                             (update! update-in [:declaration-drafts [path target]] declarations/remove-binding id)
                             (declaration-preview! path target))
       :declaration-input (fn [path target id input]
                            (update! update-in [:declaration-drafts [path target]]
                                     #(when % (assoc-in % [:inputs id] input))))
       :declaration-discard (fn [path target] (update! declarations/discard path target))
       :declaration-save declaration-save!
       :verified? #(verified? (state) (connection)) :discover #(discover! false)
       :repl-change (fn [f & args] (apply update! update :repl f args))
       :repl-evaluate
       (fn [syntax]
         (when (and (not (get-in (state) [:loading :repl]))
                    (not (false? (get-in (state) [:sandbox-permissions :evaluate?]))))
           (let [request {:text (editor/repl-input (state) syntax) :syntax syntax
                          :namespace (or (get-in (state) [:repl :namespace]) "user")}]
             (request! :repl #(remote/evaluate-memory-repl! web/server-id room request)
                       editor/repl-result nil {:scope :sandbox}))))
       :history history!
       :history-file (fn [path commit]
                       (when-let [stamp (draft-stamp! path [:history-file path])]
                         (request! [:history-file path]
                                   #(remote/load-memory-history-file! web/server-id room {:path path :commit commit :expected-target-stamp stamp})
                                   #(assoc-in %1 [:history-preview path] %2) nil
                                   {:stamp stamp :accept? (generation? path (get-in (state) [:drafts path :generation]))})))
       :evaluate (fn [path scope]
                   (let [draft (get-in (state) [:drafts path]) {:keys [anchor head]} (:selection draft)
                         request {:path path :text (:text draft) :scope scope
                                  :from (min (or anchor 0) (or head 0)) :to (max (or anchor 0) (or head 0))}]
                     (when (and (not (get-in (state) [:loading [:evaluate path]]))
                                (not (false? (get-in (state) [:sandbox-permissions :evaluate?]))))
                       (request! [:evaluate path] #(remote/evaluate-memory-code! web/server-id room request)
                                 #(assoc-in %1 [:evaluations path] (assoc %2 :input-text (:text draft))) nil
                                 {:scope :sandbox :accept? (generation? path (:generation draft))}))))
       :ensure-path (fn []
                      (when (and (:path data) (not= (:path data) (:opened-path (state))))
                        (update! assoc :opened-path (:path data))
                        (if (and (:create-origin-stamp data)
                                 (not= (:create-intent-id data) (:created-intent (state))))
                          (do (update! assoc :created-intent (:create-intent-id data))
                              (create! (:path data) (:create-origin-stamp data)))
                          (open! (:path data) false))))
       :observe-navigation
       (fn [stamp]
         (when (and (uuid? stamp) (not= stamp (:target-stamp (state))))
           ;; Client hint prompts verification, never target adoption.
           (invalidate!) (discover! true)))
       :ensure-root #(when-not (get-in (state) [:folders ""]) (folder! ""))
       :ensure-sync ensure-sync! :refresh refresh!
       :analyze analyze! :save save! :mode (fn [path mode]
                                             (change! path #(cond-> (assoc % :mode mode) (= mode :rich) editor/begin-rich))
                                             (let [draft (get-in (state) [:drafts path])]
                                               (when (and (not= mode :source)
                                                          (or (nil? (:analysis draft))
                                                              (and (#{:read :outline} mode)
                                                                   (not= (:text draft) (:analysis-text draft)))))
                                                 (analyze! path))))
       :create (fn [path & [origin]] (create! path (or origin (when (verified? (state) (connection)) (:target-stamp (state))))))
       :download download! :upload upload! :office-open office-open! :office-save office-save!
       :select-path #(update! assoc :selected %) :set-query #(update! assoc :query %) :search search!
       :toggle-root (fn [] (update! update :root-expanded? not) (folder! (or (:focus-folder (state)) "")))
       :focus-folder (fn [path] (update! assoc :focus-folder path :current-folder path :query "" :root-expanded? true) (folder! path))
       :rich-change (fn [path base-id from model selection]
                      (try (edited! path editor/rich-changed base-id from model selection)
                           (update! update :errors dissoc [:rich path])
                           (catch :default error (update! assoc-in [:errors [:rich path]] (rem/error-text error)))))
       :document-change (fn [path base-id model selection]
                          (try (let [before (get-in (state) [:drafts path :text])]
                                 (update! (fn [s]
                                          (let [before (get-in s [:drafts path])
                                                after (editor/document-changed before base-id model selection)]
                                            (cond-> (assoc-in s [:drafts path] after)
                                              (not= (:text before) (:text after))
                                              (update :errors dissoc [:rich path])))))
                                 (when (not= before (get-in (state) [:drafts path :text]))
                                   (update! assoc-in [:autosave path] true)
                                   (autosave! path)))
                               (catch :default error (update! assoc-in [:errors [:rich path]] (rem/error-text error)))))
       :document-blocked (fn [path]
                           (update! assoc-in [:errors [:rich path]]
                                    "This edit crosses a protected knowledge boundary. Copying is allowed; use Source for structural changes."))
       :local-source-open (fn [path region]
                            (if (or (declarations/busy? (state) path) (declarations/drafts? (state) path) (editor/property-drafts? (state) path))
                              (update! assoc-in [:errors [:rich path]] "Apply or discard property drafts before editing local Markdown.")
                              (try (change! path editor/open-local-source region)
                                   (catch :default error (update! assoc-in [:errors [:rich path]] (rem/error-text error))))))
       :local-source-change (fn [path text selection]
                              (let [before (get-in (state) [:drafts path :local-source :text])]
                                (change! path #(if (:local-source %)
                                                 (update % :local-source assoc :text text :selection selection) %))
                                (when (not= before text) (apply-local-source! path))))
       :local-source-cancel (fn [path] (change! path dissoc :local-source)
                              (update! update :errors dissoc [:local-source path]))
       :root (fn [] (update! assoc :current-folder "") (folder! ""))
       :mkdir (fn [path]
                (when-let [stamp (target! [:mkdir path])]
                  (request! [:mkdir path] #(remote/save-memory-file! web/server-id room
                                                                     {:path (str path "/.keep") :expected nil :text "" :expected-target-stamp stamp})
                            #(-> %1 (assoc :current-folder path) (assoc-in [:expanded path] true))
                            (fn [_] (folder! (parent path)) (folder! path) (navigation-refresh!)) {:stamp stamp})))
       :toggle (fn [path]
                 (let [expanded (get-in (state) [:expanded path])]
                   (update! assoc-in [:expanded path] (not expanded))
                   (when expanded (update! update :folders dissoc path))
                   (update! assoc :current-folder path)
                   (when (and (not expanded) (nil? (get-in (state) [:folders path]))) (folder! path))))
       :use-incoming (fn [path]
                       (let [incoming (get-in (state) [:drafts path :incoming])]
                         (when (and (verified? (state) (connection))
                                    (= (:target-stamp (state)) (:target-stamp incoming))
                                    (js/confirm "Replace this local draft with the current workspace's saved version?"))
                           (change! path editor/use-incoming)
                           (update! update :errors dissoc [:save path] [:file path] [:rich path]))))
       :move (fn [path target]
               (let [draft (get-in (state) [:drafts path])]
                 (if (or (declarations/move-blocked? (state) path)
                         (get-in (state) [:loading [:save path]])
                         (get-in (state) [:loading [:office-save path]]) (get-in (state) [:loading [:move path]])
                         (get-in (state) [:loading [:property-save path]])
                         (get-in (state) [:drafts target]))
                   (update! assoc-in [:errors :move] "Apply or discard field drafts, finish pending saves and choose a destination without another open draft before moving.")
                   (when-let [stamp (draft-stamp! path [:move path])]
                     (request! [:move path] #(remote/move-memory-file! web/server-id room
                                                                       {:path path :target target :expected (get-in draft [:source :entry]) :expected-target-stamp stamp})
                               (fn [s reply]
                                 (-> s (assoc-in [:drafts target] (editor/moved (get-in s [:drafts path]) reply))
                                     (update :drafts dissoc path) (assoc :selected (:path reply))))
                               (fn [_]
                                 (binding [ec/*execution-context* ctx]
                                   (swap! sig/layout-columns
                                          (fn [columns]
                                            (mapv (fn [col]
                                                    (if (= (:col-id data) (:id col))
                                                      (update col :tabs
                                                              (fn [tabs] (mapv (fn [tab]
                                                                                 (if (= (:tab-id data) (:id tab))
                                                                                   (-> tab (assoc-in [:data :path] target)
                                                                                       (assoc :title (last (str/split target #"/")))) tab)) tabs))) col)) columns))))
                                 (folder! (parent path)) (folder! (parent target)) (navigation-refresh!) (open! target true))
                               {:stamp stamp :accept? (fn [s _] (and (= (:generation draft) (get-in s [:drafts path :generation]))
                                                                     (nil? (get-in s [:drafts target]))))})))))})))
