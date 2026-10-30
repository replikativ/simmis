(ns is.simm.uis.web.desktop.memory-actions
  "Context/session-owned stamped Memory I/O. A stamp is a precondition, never
   authority. Generic Room SCI keeps its separate selected-workspace scope."
  (:require [clojure.string :as str]
            [org.replikativ.spindel.engine.core :as ec]
            [is.simm.uis.web.desktop.runtime :refer [runtime]]
            [is.simm.uis.web.desktop.signals :as sig]
            [is.simm.uis.web.desktop.memory-editor :as editor]
            [is.simm.uis.web.desktop.memory-owner :as owner]
            [is.simm.uis.web.desktop.memory-edit-owners :as edit-owners]
            [is.simm.uis.web.desktop.memory-metadata-view :as metadata-view]
            [is.simm.uis.web.desktop.memory-metadata-io :as metadata-io]
            [is.simm.uis.web.desktop.memory-cells :as cells]
            [is.simm.uis.web.desktop.memory-tree :as tree]
            [is.simm.uis.web.desktop.memory-declarations :as declarations]
            [is.simm.uis.web.desktop.memory-sync :as sync]
            [is.simm.uis.web.desktop.memory-remote :as remote]
            [is.simm.uis.web.desktop.chart-remote :as chart-remote]
            [is.simm.uis.web.desktop.memory-charts :as charts]
            [is.simm.uis.web.desktop.memory-saved-views :as saved-views]
            [is.simm.uis.web.desktop.saved-view-remote :as saved-view-remote]
            [is.simm.uis.web.desktop.memory-references :as references]
            [is.simm.uis.web.desktop.memory-backlinks :as backlinks]
            [is.simm.documents.links :as links]
            [is.simm.documents.saved-view-snapshot :as snapshot]
            [is.simm.uis.web.desktop.refs :as refs]
            [is.simm.uis.web.desktop.memory-reference-remote :as reference-remote]
            [is.simm.uis.web.desktop.remote :as rem]
            [is.simm.runtimes.web :as web]))

(defn state-key [data]
  (owner/key (:id (web/current-session)) data))

(defn- parent [path] (str/join "/" (butlast (str/split path #"/"))))

(defn verified? [state connection]
  (and (some? connection) (true? (:target-verified? state))
       (= connection (:verified-connection state)) (uuid? (:target-stamp state))))

(defn- draft-current? [state path]
  (let [draft (get-in state [:drafts path])]
    (and draft (not (:target-stale? draft))
         (= (:target-stamp state) (get-in draft [:source :target-stamp])))))

(defn- moved-state [state path target reply]
  (let [slots (into #{[:file path]}
                    (keep (fn [[slot frame]]
                            (when (and (vector? slot) (= path (second slot))
                                       (= :target (:scope frame)) (not= :move (first slot))) slot)))
                    (:request-contexts state))]
    (reduce (fn [s key]
              (let [values (get s key)]
                (if (contains? values path)
                  (assoc s key (assoc (dissoc values path) target (get values path))) s)))
     (-> state
        (assoc-in [:drafts target] (editor/moved (get-in state [:drafts path]) reply))
        (update :drafts dissoc path)
        (assoc :selected target)
        (metadata-view/retire-reads slots)
        (update :errors #(apply dissoc % slots))
        (update :file-intents dissoc path)
        (update :metadata-followups #(into {} (remove (fn [[slot _]] (= path (second slot)))) %))
        (update :observed-entries dissoc path)
        (update :history dissoc path)
        (update :history-preview dissoc path)
        (update :accepted dissoc :metadata)
        ;; An older discovery keeps its token so on-settled can drain the
        ;; current intent, but cannot consume or re-open the retired path.
        (update :discovery-revision (fnil inc 0)))
     [:autosave :autosave-holds])))

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
        change! (fn [path f & args] (apply update! update-in [:drafts path] f args))
        shared (metadata-io/controller ctx session room key state-signal)
        property-blocked? #(binding [ec/*execution-context* ctx]
                             (edit-owners/blocked? @state-signal key %))]
    (letfn [(select! [path & [retain-reference?]]
              (let [preserve? (or retain-reference? (= path (:selected (state))))]
                (update! #(cond-> (assoc % :selected path)
                            (not preserve?) (dissoc :block-reference)
                            (not preserve?) (update :errors dissoc :reference)))
                (when (and (current?) (:tab-id data))
                  (binding [ec/*execution-context* ctx]
                    (when-let [location (owner/current-tab @sig/layout-columns room (:tab-id data))]
                      (sig/retarget-tab! (:col-id location) (:tab-id data)
                                         #(if (and (= :memory (:type %))
                                                   (= room (str (get-in % [:data :room-id]))))
                                            (references/selected-tab % path preserve?) %)
                                         (if retain-reference? :passive :navigate)))))))
            (connected! []
              (let [c (connection) stream (binding [ec/*execution-context* ctx] @sync/state)]
                (when (and (current?) (not= c (:connection (state))))
                  (update! #(-> % metadata-view/retire-upload
                                (assoc :connection c :target-verified? false
                                       :stream-marker [(:connection stream) (:status stream)
                                                       (get-in stream [:dirty room] 0)])
                                (update :target-epoch (fnil inc 0))
                                (assoc :requests {} :request-contexts {} :loading {} :accepted {} :finished {})
                                (dissoc :folders :verified-connection :permissions :sandbox-permissions
                                        :watch-connection :sync))))
                (when (and (current?) (nil? (:stream-marker (state))))
                  (update! assoc :stream-marker [(:connection stream) (:status stream)
                                                (get-in stream [:dirty room] 0)]))
                c))
            (frame-current? [s frame]
              (and (= (:connection frame) (:connection s))
                   (or (not= :target (:scope frame))
                       (and (verified? s (:connection frame))
                            ((:available? shared))
                            (or (nil? ((:target shared))) (= (:stamp frame) ((:target shared))))
                            (= (:stamp frame) (:target-stamp s))
                            (= (:target-epoch frame) (:target-epoch s))
                            (or (not (:read? frame))
                                (and (= (:read-epoch frame) (:read-epoch s))
                                     (= (:roster frame) (binding [ec/*execution-context* ctx] @sig/user-rooms))))))))
            (invalidate! []
              (update! metadata-view/unverify))
            (request! [slot invoke consume after opts]
              (let [c (connected!) token (random-uuid)
                    frame {:scope (:scope opts :target) :connection c :stamp (:stamp opts)
                           :target-epoch (:target-epoch (state)) :read? (:read? opts)
                           :roster (binding [ec/*execution-context* ctx] @sig/user-rooms)
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
                                                                    (= :metadata slot) metadata-view/unverify
                                                                    (#{:authentication-required :not-authorized} type)
                                                                    (-> metadata-view/unverify (dissoc :sandbox-permissions)))]
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
            (metadata-followups! [reply]
              (let [{:keys [files folders office search? histories]}
                    (metadata-view/followups (state) reply)]
                (update! dissoc :metadata-followups)
                (doseq [p files] (open! p true nil true))
                (doseq [{:keys [path part reading?]} office]
                  (office-open! path part reading?))
                (doseq [p folders] (folder! p))
                (when search? (search!))
                (doseq [p histories] (history! p))))
            (consume-metadata! []
              (when-let [envelope ((:observation shared))]
                (let [identity [(:epoch envelope) (:sequence envelope)]
                      frame (get-in envelope [:owners key])]
                  (when (and (not= identity (:metadata-consumed (state)))
                             (or (:error envelope) frame))
                    ;; Mark consumption before any followup can synchronously
                    ;; reenter this controller. The original admission owns gens.
                    (if-let [error (:error envelope)]
                      (update! #(-> % metadata-view/unverify
                                    (dissoc :folders :observed-entries)
                                    (cond-> (#{:authentication-required :not-authorized} (:type error))
                                      (dissoc :sandbox-permissions))
                                    (assoc :metadata-consumed identity)
                                    (assoc-in [:loading :metadata] false)
                                    (assoc-in [:errors :metadata] (:message error))))
                      (let [reply (-> (:reply envelope)
                                      (update :folders select-keys (get-in frame [:request :folders]))
                                      (update :entries select-keys (get-in frame [:request :paths])))]
                        (update! #(-> (metadata-view/consume % reply frame)
                                      (assoc :metadata-consumed identity)
                                      (assoc-in [:loading :metadata] false)
                                      (update :errors dissoc :metadata)))
                        (metadata-followups! reply)))))))
            (discover! [force?]
              (when-not (:metadata-interest? (state)) (update! assoc :metadata-interest? true))
              (when force?
                (update! update :discovery-revision (fnil inc 0))
                ((:invalidate shared)))
              (let [c (connected!) request (metadata-view/request (state))]
                (when (and c (current?))
                  (cond
                    (not ((:available? shared)))
                    (when (or (:target-verified? (state)) (:folders (state))
                              (not= "This room is no longer available." (get-in (state) [:errors :metadata])))
                      (update! #(-> % metadata-view/unverify
                                    (dissoc :folders :observed-entries :sandbox-permissions)
                                    (assoc-in [:loading :metadata] false)
                                    (assoc-in [:errors :metadata] "This room is no longer available."))))
                    (or (> (count (:folders request)) 64) (> (count (:paths request)) 64))
                    (do (invalidate!)
                        (update! assoc-in [:errors :metadata]
                                 "Memory supports 64 expanded folders and file buffers per view. Collapse folders or close buffers, then retry."))
                    :else (do
                      ((:ensure shared))
                      (let [target ((:target shared)) envelope ((:observation shared))]
                        (when (and target (not= target (:target-stamp (state)))
                                   (not= target (get-in envelope [:reply :target-stamp])))
                          (update! #(-> % (editor/observe-target target)
                                        (update :target-epoch (fnil inc 0))
                                        metadata-view/retire-upload metadata-view/target-bookkeeping
                                        metadata-view/unverify))))
                      (when-not ((:observation shared))
                        (let [included? (boolean ((:included? shared)))]
                          (when (not= included? (get-in (state) [:loading :metadata]))
                            (update! assoc-in [:loading :metadata] included?))))
                      (consume-metadata!))))))
            (target! [slot]
              (connected!)
              (if (and (verified? (state) (connection)) ((:available? shared))
                       (or (nil? ((:target shared))) (= (:target-stamp (state)) ((:target shared))))) (:target-stamp (state))
                  (do (update! assoc-in [:errors slot] "Verify the current workspace with Refresh before this action.")
                      (discover! false) nil)))
            (draft-stamp! [path slot]
              (when (target! slot)
                (if (draft-current? (state) path) (get-in (state) [:drafts path :source :target-stamp])
                    (do (update! assoc-in [:errors slot] "This draft belongs to another workspace. Choose its current copy explicitly before publishing.") nil))))
            (generation? [path generation]
              (fn [s _] (= generation (get-in s [:drafts path :generation]))))
            (backlink-marker [s]
              (binding [ec/*execution-context* ctx]
                (when (and (current?) (owner/current-tab @sig/layout-columns room (:tab-id data)))
                  (backlinks/marker room s (connection) @sig/user-rooms))))
            (ensure-backlinks! []
              (connected!)
              (let [s (state) marker (backlink-marker s)]
                (when (get-in s [:backlinks :open?])
                  (cond
                    (nil? marker)
                    (when (get-in s [:backlinks :marker])
                      (update! #(-> % (update :backlinks dissoc :marker :reply :error) (update :errors dissoc :backlinks))))
                    (not= marker (get-in s [:backlinks :marker]))
                    (do
                      (update! #(-> % (assoc-in [:backlinks :marker] marker) (update :backlinks dissoc :reply :error)))
                      (request! :backlinks #(reference-remote/load-memory-backlinks! web/server-id room (backlinks/request marker))
                                (fn [current result]
                                  (let [updated (assoc-in current [:backlinks :reply] result)]
                                    (when-not (backlinks/reply updated marker)
                                      (throw (ex-info "Backlinks returned an unexpected workspace observation." {})))
                                    updated)) nil
                                {:stamp (:target-stamp marker) :read? true
                                 :accept? (fn [current _] (= marker (backlink-marker current)))
                                 :error-current? (fn [current] (= marker (backlink-marker current)))
                                 :failure-state (fn [current error]
                                                  (-> current (update :errors dissoc :backlinks)
                                                      (assoc-in [:backlinks :error] (rem/error-text error))))}))))))
            (navigation-refresh! []
              ((:invalidate shared))
              (when-not (= :navigation (:col-id data))
                ((:discover (controller {:room-id (:room-id data) :col-id :navigation
                                         :state-signal sig/memory-navigation-state}))))
              (discover! false))
            (folder! [path]
              (update! assoc-in [:folder-intents path] true)
              (if-let [stamp (when (verified? (state) (connected!)) (:target-stamp (state)))]
                (let [marker (:stream-marker (state))]
                  (request! [:folder path] #(remote/load-memory-folder! web/server-id room
                                                                      {:folder path :expected-target-stamp stamp})
                          #(-> %1 (assoc-in [:folders path] %2) (update :folder-intents dissoc path)
                               tree/trim-folder-cache)
                          nil {:stamp stamp :read? true :accept? (fn [s _] (= marker (:stream-marker s)))}))
                (discover! false)))
            (open! [path refresh? & [observed passive?]]
              (when-not (or observed passive?) (select! path))
              (when (or refresh? (nil? (get-in (state) [:drafts path])))
                (update! assoc-in [:file-intents path] true)
                (if-let [stamp (when (verified? (state) (connected!)) (:target-stamp (state)))]
                  (let [generation (get-in (state) [:drafts path :generation])]
                    (request! [:file path] #(remote/load-memory-file! web/server-id room
                                                                      {:path path :expected-target-stamp stamp})
                              #(-> %1 (update-in [:drafts path] editor/loaded-at-generation generation %2)
                                   (editor/install-page-properties path %2)
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
              (editor/property-marker s path target))
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
            (saved-view! [path view]
              ;; Connection initialization may install the stream marker. Read
              ;; the snapshot only afterwards so the first reply can be current.
              (let [c (connected!) s (state) slot [:saved-view path view] marker (saved-views/marker s path view)
                    input (saved-views/request s path view)]
                (when (and (saved-views/opened? s path view) input
                           (verified? s c) (not (get-in s [:loading slot]))
                           (not= marker (get-in s [:saved-view-attempts [path view]])))
                  (update! assoc-in [:saved-view-attempts [path view]] marker)
                  (request! slot #(saved-view-remote/load-memory-saved-view! web/server-id room input)
                            (fn [current reply]
                              (cond-> (assoc-in current [:saved-views [path view]] {:reply reply :marker marker})
                                (and (saved-views/pages? current path view)
                                     (= :form (get-in reply [:descriptor :presentation]))
                                     (= :ready (:status reply)) (= 1 (count (:rows reply)))
                                     (nil? (get-in current [:page-view-edits [path view]])))
                                (assoc-in [:page-view-edits [path view]] (select-keys (first (:rows reply)) [:id :path]))))
                            nil {:stamp (:target-stamp s) :read? true
                                 :accept? (fn [current reply]
                                            (and (saved-views/opened? current path view)
                                                 (saved-views/reply-current? current path view marker reply)))
                                 :failure-state (fn [current _] (update current :saved-views dissoc [path view]))}))))
            (enroll-page-view-row! [path view row]
              (let [s (state) row-path (:path row) slot [:file row-path]
                    marker [(:connection s) (:target-stamp s) (:read-epoch s) (:id row) row-path]]
                (when (and (saved-views/pages? s path view) (string? row-path) (uuid? (:id row))
                           (nil? (get-in s [:drafts row-path]))
                           (not (get-in s [:loading slot]))
                           (not (and (contains? (:observed-entries s) row-path)
                                     (nil? (get-in s [:observed-entries row-path]))))
                           (not= marker (get-in s [:page-view-row-attempts [path view]])))
                  (update! #(-> % (assoc-in [:page-view-row-attempts [path view]] marker)
                                (update :page-view-enrolled (fnil conj #{}) row-path)))
                  (open! row-path false nil true))))
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
                (when (and pd (not (property-blocked? path))
                           (not (get-in s [:loading slot])) (not (get-in s [:loading [:save path]]))
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
                                  (cond-> (-> current (update :property-attempts dissoc scope))
                                    (some? (get-in current [:drafts path]))
                                    (update-in [:drafts path] editor/property-saved (:generation draft) reply)
                                    (= pd (get-in current [:property-drafts scope (:id field)]))
                                    (update-in [:property-drafts scope] dissoc (:id field))))
                                (fn [_] (navigation-refresh!)) {:stamp stamp}))))))
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
                (when (and command (not (property-blocked? path)) (declarations/current? dd model)
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
                              (fn [_] (navigation-refresh!)) {:stamp stamp})))))
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
            (snapshot! [path view]
              (let [c (connected!) s (state) key [path view] slot [:snapshot path view]
                    pending (get-in s [:snapshot-intents key])
                    reply (saved-views/visible s path view)
                    sent (get-in s [:saved-views key :marker])]
                (when-not (get-in s [:loading slot])
                  (if-not (and (verified? s c) (true? (get-in s [:permissions :write?]))
                               (if pending (= (:target pending) (:target-stamp s))
                                   (and (= :ready (:status reply))
                                        (saved-views/reply-current? s path view sent reply))))
                    (update! assoc-in [:errors slot] "Wait for a current saved view and workspace write access. A pending export must be recovered in its original workspace.")
                    (try
                      (let [id (when-not pending (random-uuid))
                            intent (or pending
                                       {:path (str "reports/snapshots/" id ".md") :target (:target-stamp s)
                                        :basis (:basis reply) :view view
                                        :text (snapshot/markdown reply {:id id :room (if (uuid? room) room (parse-uuid room))
                                                                        :target (:target-stamp s) :created-at (.toISOString (js/Date.))})})
                            stamp (:target intent) destination (:path intent)
                            write-request {:path destination :expected nil :text (:text intent) :expected-target-stamp stamp}]
                        ;; Freeze before dispatch. A lost ACK must not allocate
                        ;; a second artifact or silently export a newer view.
                        (update! assoc-in [:snapshot-intents key] intent)
                        (request! slot
                                  (fn []
                                    (let [token (get-in (state) [:requests slot])
                                          frame (get-in (state) [:request-contexts slot])
                                          active? #(let [current (state)]
                                                     (and (current?) (= c (connection))
                                                          (frame-current? current frame)
                                                          (true? (get-in current [:permissions :write?]))
                                                          (editor/response-current? current slot token)
                                                          (not= token (get-in current [:finished slot]))
                                                          (= intent (get-in current [:snapshot-intents key]))))
                                          write! (fn [ok error]
                                                   (if (active?)
                                                     ((remote/save-memory-file! web/server-id room write-request) ok error)
                                                     (error (js/Error. "The snapshot's initiating request is no longer current."))))]
                                      (fn [ok error]
                                        (if pending
                                          ((remote/load-memory-metadata! web/server-id room {:folders [] :paths [destination]})
                                           (fn [metadata]
                                             (if-not (and (active?) (= stamp (:target-stamp metadata))
                                                          (contains? (:entries metadata) destination))
                                               (error (js/Error. "The snapshot destination could not be verified in its original workspace."))
                                               (if (nil? (get-in metadata [:entries destination]))
                                                 (write! ok error)
                                                 ((remote/load-memory-file! web/server-id room
                                                                            {:path destination :expected-target-stamp stamp})
                                                  (fn [file]
                                                    (if (= (:text intent) (:text file)) (ok file)
                                                        (error (js/Error. "The snapshot destination contains different content; it was not overwritten."))))
                                                  error))))
                                           error)
                                          (write! ok error)))))
                                  (fn [current _]
                                    (-> current (assoc-in [:snapshot-exports key] (select-keys intent [:path :basis :view]))
                                        (update :snapshot-intents dissoc key)))
                                  (fn [_] (folder! (parent destination)) (navigation-refresh!))
                                  {:stamp stamp}))
                      (catch :default error (update! assoc-in [:errors slot] (rem/error-text error))))))))
            (create! [path origin]
              (if-not (and (string? path) (not (str/blank? path)) (uuid? origin))
                (do (update! assoc-in [:errors :create] "Verify the initiating workspace before creating a file.") (discover! false))
                (let [markdown? (re-find #"(?i)\.(md|markdown)$" path)
                      intent {:origin origin :kind (if markdown? :markdown :text)
                              :text (if markdown? (str "---\nsimmis-id: '" (random-uuid) "'\n---\n# Untitled\n\n") "")}]
                  (select! path)
                  (update! assoc-in [:create-intents path] intent)
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
                (when (and (not (get-in (state) [:loading [:office-save path]]))
                           (not (get-in (state) [:loading [:office-correct path]])) (not (get-in (state) [:loading [:move path]])))
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
            (office-correct! [path]
              (let [draft (get-in (state) [:drafts path]) sent (:office-correction draft)
                    slot [:office-correct path]]
                (when (and (editor/office-correction-current? draft)
                           (not= (:text sent) (:original sent))
                           (not (get-in (state) [:loading slot]))
                           (not (get-in (state) [:loading [:office-save path]]))
                           (not (get-in (state) [:loading [:move path]])))
                  (when-let [stamp (draft-stamp! path slot)]
                    (request! slot #(remote/correct-memory-office-text! web/server-id room
                                                                      {:path path :expected (:entry sent) :token (:token sent)
                                                                       :replacement (:text sent) :expected-target-stamp stamp})
                              #(update-in %1 [:drafts path] editor/office-corrected (:generation draft) sent %2)
                              (fn [_] (open! path true) (folder! (parent path)) (navigation-refresh!))
                              {:stamp stamp})))))
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
            (ensure-sync! [c]
              (connected!)
              (when (and c (= c (connection)) (current?))
                ;; A resumed render may hold an older tracked stream value. Sample
                ;; the current signal here, as connected! does, so two render
                ;; paths cannot alternate current and obsolete markers forever.
                (let [stream (binding [ec/*execution-context* ctx] @sync/state)
                      marker [(:connection stream) (:status stream) (get-in stream [:dirty room] 0)]]
                  (when (not= marker (:stream-marker (state)))
                    (update! #(-> % tree/invalidate-closed-folders (assoc :stream-marker marker)))))
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
              (let [stream (binding [ec/*execution-context* ctx] @sync/state)]
                (update! #(-> % tree/invalidate-closed-folders
                              (assoc :errors {} :retry-stream? true
                                     :stream-marker [(:connection stream) (:status stream)
                                                     (get-in stream [:dirty room] 0)])
                              (dissoc :watch-connection))))
              (navigation-refresh!)
              (when-let [c (connection)]
                (ensure-sync! c)))]
      {:owner key :room-id room :state state :change change! :edit edited! :folder folder! :open #(open! % false) :reload #(open! % true)
       :settings-open #(update! assoc :settings-open? (boolean %))
       :observe-receipt-context
       (fn [context]
         (when (not= context (:receipt-context (state)))
           (update! assoc :receipt-context context)))
       :toggle-saved-view
       (fn [path view]
         (let [slot [:saved-view path view] opened? (saved-views/opened? (state) path view)]
           (update! #(cond-> (-> % (assoc-in [:page-view-open [path view]] (not opened?))
                                  (update :saved-view-attempts dissoc [path view]))
                       opened? (metadata-view/retire-reads #{slot})))
           (when-not opened? (saved-view! path view))))
       :retry-saved-view (fn [path view]
                          (update! update :saved-view-attempts dissoc [path view])
                          (saved-view! path view))
       :edit-page-view-row
       (fn [path view row]
         (let [s (state) current (get-in s [:page-view-edits [path view]])
               available (when (saved-views/pages? s path view)
                           (some #(when (= (select-keys row [:id :path]) (select-keys % [:id :path])) %)
                                 (:rows (saved-views/visible s path view))))
               held? (and current (not= (:path current) (:path row))
                          (or (editor/dirty? (get-in s [:drafts (:path current)]))
                              (editor/property-drafts? s (:path current))
                              (declarations/drafts? s (:path current))
                              (declarations/busy? s (:path current))))]
           (cond
             held? (update! assoc-in [:page-view-edit-errors [path view]]
                            "Finish or discard the current row's edits before selecting another row.")
             available (do (update! #(-> % (assoc-in [:page-view-edits [path view]] (select-keys row [:id :path]))
                                          (update :page-view-edit-errors dissoc [path view])
                                          (update :page-view-row-attempts dissoc [path view])))
                           (enroll-page-view-row! path view row)))))
       :retry-page-view-row
       (fn [path view]
         (when-let [row (when (saved-views/pages? (state) path view)
                         (get-in (state) [:page-view-edits [path view]]))]
           (update! #(-> % (update :page-view-row-attempts dissoc [path view])
                         (update :errors dissoc [:file (:path row)])))
           (if (and (contains? (:observed-entries (state)) (:path row))
                    (nil? (get-in (state) [:observed-entries (:path row)])))
             (discover! true)
             (enroll-page-view-row! path view row))))
       :ensure-saved-views
       (fn []
         (when (not= (state) (saved-views/retire-unused-rows (state)))
           (update! saved-views/retire-unused-rows))
         (let [s (state) path (:selected s)]
           (doseq [entry (saved-views/entries s path)
                   :let [view (:view entry)]
                   :when (and (= :ready (:status entry)) (saved-views/opened? s path view))]
             (saved-view! path view)
             (when-let [row (when (saved-views/pages? (state) path view)
                             (get-in (state) [:page-view-edits [path view]]))]
               (when (some #(= (select-keys row [:id :path]) (select-keys % [:id :path]))
                           (:rows (saved-views/visible (state) path view)))
                 (enroll-page-view-row! path view row)
                 (when (saved-views/row-identity-current? (state) row)
                   (properties! (:path row) {:kind :page})))))))
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
       :chart-budget #(cells/chart-budget (state) %)
       :chart-models (fn [path]
                       (let [s (state) models (charts/visible s path) budget (cells/chart-budget s path)]
                         (into {} (take budget models))))
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
       :reference-search (fn [path target id query] (update! assoc-in [:reference-search [path target id]] query))
       :property-discard (fn [path target id] (update! update-in [:property-drafts [path target]] dissoc id))
       :property-current? (fn [path target model] (= (:request-marker model) (property-marker (state) path target)))
       :property-save property-save!
       :property-blocked? property-blocked?
       :declaration-open
       (fn [path target]
         (let [s (state) scope [path target] model (get-in s [:property-models scope])]
           (when (and (not (property-blocked? path))
                      (declarations/openable? s path model) (verified? s (connected!))
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
       :cell-config #(when (verified? (state) (connection)) (cells/visible (state) %))
       :run-cell (fn [path key]
                   (when (and (verified? (state) (connection))
                              (not (false? (get-in (state) [:sandbox-permissions :evaluate?]))))
                     (when-let [request (cells/request (state) path key)]
                       (let [slot [:cell path (:revision request) key]]
                         (when-not (get-in (state) [:loading slot])
                           (request! slot #(remote/evaluate-memory-cell! web/server-id room request)
                                     editor/repl-result nil {:scope :sandbox}))))))
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
                                 #(editor/file-evaluation-result %1 path (:generation draft) (:text draft) %2) nil
                                 {:scope :sandbox}))))
       :block-links (fn [path] (references/block-links room path (get-in (state) [:drafts path])))
       :inline-chart-previews (fn [path]
                                (let [s (state)]
                                  (when (and (current?) (verified? s (connection)) (draft-current? s path))
                                    (mapv #(select-keys % [:language :text :status :data])
                                          (:inline-charts (charts/saved-analysis (get-in s [:drafts path])))))))
       :ensure-backlinks ensure-backlinks!
       :backlinks-open (fn [open?]
                         (when (not= (boolean open?) (boolean (get-in (state) [:backlinks :open?])))
                           (update! assoc-in [:backlinks :open?] (boolean open?))))
       :backlinks #(when (current?) (backlinks/observation (state) (backlink-marker (state))))
       :retry-backlinks #(update! update :backlinks dissoc :marker :reply :error)
       :open-backlink (fn [result]
                        (when-let [reply (when (and (current?) (get-in (state) [:backlinks :open?]))
                                          (backlinks/reply (state) (backlink-marker (state))))]
                          (when (some #{result} (:results reply))
                            (binding [ec/*execution-context* ctx]
                              (when-let [location (owner/current-tab @sig/layout-columns room (:tab-id data))]
                                (refs/open! (links/parse-route (:url result)) {:col-id (:col-id location) :new-tab? true}))))))
       :jump (fn [path] (references/jump (state) path))
       :linked-block (fn [path] (references/linked-block (state) path))
       :retry-reference (fn [] (update! #(-> % (dissoc :block-reference) (update :errors dissoc :reference))))
       :before-jump (fn [path jump]
                      (and (current?) (verified? (state) (connection))
                           (= jump (references/jump (state) path))))
       :jumped (fn [path jump mode selection]
                 (when (and (current?) (verified? (state) (connection))
                            (= jump (references/jump (state) path)))
                   (update! (fn [s]
                              (-> s (assoc-in [:block-reference :applied?] true)
                                  (assoc-in (if (= :rich mode)
                                              [:drafts path :rich-base :document-selection]
                                              [:drafts path :selection]) selection))))))
       :ensure-path (fn []
                       ;; A previous incremental frame can resume after this tab
                       ;; navigates. Its captured path must not rewind the live
                       ;; address and start a feedback loop with the new frame.
                       (let [data (if (some? (:tab-id data))
                                    (binding [ec/*execution-context* ctx]
                                      (get-in (owner/current-tab @sig/layout-columns room (:tab-id data)) [:tab :data]))
                                    data)]
                         (when data
                           (if-let [intent (not-empty (select-keys (:reference data) [:repository-id :block-id]))]
                             (let [c (connected!) s (state) marker (references/marker s intent)]
                               (when (and (verified? s c)
                                          (not (references/completed? s intent))
                                          (not= marker (get-in s [:block-reference :request-marker])))
                                 (update! assoc :block-reference {:intent intent :request-marker marker :id (random-uuid)})
                                 (request! :reference #(reference-remote/resolve-memory-block! web/server-id room
                                                                                               (assoc intent :expected-target-stamp (:target-stamp s)))
                                           #(assoc-in %1 [:block-reference :reply] %2)
                                           (fn [reply]
                                             (if (= :resolved (:status reply))
                                               (do (select! (:path reply) true) (open! (:path reply) true nil true))
                                               (update! assoc-in [:errors :reference]
                                                        (if (= :ambiguous (:status reply))
                                                          "This block identity appears more than once. Resolve the duplicate in Markdown before opening its link."
                                                          "This block is not in the current workspace."))))
                                           {:stamp (:target-stamp s) :read? true
                                            :accept? (fn [current _] (= marker (get-in current [:block-reference :request-marker])))
                                            :error-current? (fn [current] (= marker (get-in current [:block-reference :request-marker])))})))
                             (when (and (:path data) (not= (:path data) (:opened-path (state))))
                               (update! assoc :opened-path (:path data))
                               (if (and (:create-origin-stamp data)
                                        (not= (:create-intent-id data) (:created-intent (state))))
                                 (do (update! assoc :created-intent (:create-intent-id data))
                                     (create! (:path data) (:create-origin-stamp data)))
                                 (open! (:path data) false)))))))
       :observe-navigation
       (fn [stamp]
         (when (and (uuid? stamp) (not= stamp (:target-stamp (state))))
           ;; Client hint prompts verification, never target adoption.
           (invalidate!) (discover! true)))
       :ensure-root #(when-not (get-in (state) [:folders ""]) (folder! ""))
       :ensure-sync ensure-sync!
       :maintain-metadata #(do ((:ensure shared))
                               (when (:metadata-interest? (state)) (discover! false)))
       :consume-metadata #(when (:metadata-interest? (state)) (discover! false)) :refresh refresh!
       :analyze analyze! :save save! :mode (fn [path mode]
                                             (change! path #(cond-> (assoc % :mode mode) (= mode :rich) editor/begin-rich))
                                             (let [draft (get-in (state) [:drafts path])]
                                               (when (and (not= mode :source)
                                                          (or (nil? (:analysis draft))
                                                              (and (#{:read :outline} mode)
                                                                   (not= (:text draft) (:analysis-text draft)))))
                                                 (analyze! path))))
       :create (fn [path & [origin]] (create! path (or origin (when (verified? (state) (connection)) (:target-stamp (state))))))
       :save-view-snapshot snapshot!
       :toggle-saved-view-source (fn [path view] (update! update-in [:saved-view-source [path view]] not))
       :download download! :upload upload! :office-open office-open! :office-save office-save! :office-correct office-correct!
       :select-path select! :set-query #(update! assoc :query %) :search search!
       :toggle-root (fn []
                      (update! update :root-expanded? not)
                      (let [s (state) path (or (:focus-folder s) "")]
                        (when (and (:root-expanded? s) (nil? (get-in s [:folders path]))) (folder! path))))
       :focus-folder (fn [path]
                       (update! assoc :focus-folder path :current-folder path :query "" :root-expanded? true)
                       (when-not (get-in (state) [:folders path]) (folder! path)))
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
       :document-blocked (fn [path & [reason]]
                           (update! assoc-in [:errors [:rich path]]
                                    (or reason "This edit crosses a protected knowledge boundary. Copying is allowed; use Source for structural changes.")))
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
                   (update! tree/trim-folder-cache)
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
                         (get-in (state) [:loading [:office-correct path]])
                         (get-in (state) [:loading [:property-save path]])
                         (get-in (state) [:drafts target]))
                   (update! assoc-in [:errors :move] "Apply or discard field drafts, finish pending saves and choose a destination without another open draft before moving.")
                   (when-let [stamp (draft-stamp! path [:move path])]
                     (request! [:move path] #(remote/move-memory-file! web/server-id room
                                                                       {:path path :target target :expected (get-in draft [:source :entry]) :expected-target-stamp stamp})
                               (fn [s reply]
                                 (moved-state s path target reply))
                               (fn [_]
                                 (select! target true)
                                 (doseq [p (set [(parent path) (parent target)])] (folder! p))
                                 (navigation-refresh!) (open! target true) (autosave! target))
                               {:stamp stamp :accept? (fn [s _] (and (= (:generation draft) (get-in s [:drafts path :generation]))
                                                                     (nil? (get-in s [:drafts target]))))})))))})))
