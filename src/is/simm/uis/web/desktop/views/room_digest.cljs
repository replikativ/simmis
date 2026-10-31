(ns is.simm.uis.web.desktop.views.room-digest
  "Plain text expandable digest → typed claims → exact frozen originals."
  (:require [org.replikativ.spindel.dom.elements :as el]
            [is.simm.documents.links :as links]
            [is.simm.documents.workflow-links :as workflows]
            [is.simm.model.conversation-digest :as digest]
            [is.simm.uis.web.desktop.room-digest-actions :as actions]
            [is.simm.uis.web.desktop.room-digest :as model]
            [is.simm.uis.web.desktop.routes :as routes]))

(defn control [{:keys [room-state] :as data}]
  (let [paths (mapv :path (take 8 (get-in room-state [:saved :writes]))) a (actions/controller data)]
    (when (seq paths)
      (el/button {:type "button" :class "chat-message-reply"
                  :on-click (fn [e] (.stopPropagation e) ((:preview a) paths))}
                 (str "Prepare digest of " (count paths) " retained originals")))))

(defn result-card [{:keys [result room-id]}]
  (let [report (:digest result) originals (:originals result)
        by-revision (into {} (map (juxt :source-revision identity)) originals)]
    (el/details {:open true :class "room-digest-result"}
      (el/summary {} (if (= :room-summary (:kind result)) "Daily conversation summary" "Verified navigation digest"))
      (el/p {} (get-in report [:summary :text]))
      (el/p {} "Optional navigation. Original evidence remains eligible for knowledge maintenance; this does not publish a wiki update.")
      (for [section digest/section-keys
              :let [items (get-in report [:sections section])]
              :when (seq items)]
        (el/details {:key (name section)}
          (el/summary {} (case section :open-work "Open work" (name section)))
          (for [[index item] (map-indexed vector items)]
            (el/div {:key index}
              (el/p {} (:text item))
              (for [revision (:sources item) :let [source (get by-revision revision)]]
                (el/details {:key (str revision)}
                  (el/summary {} (str (if (= :summary (:kind source)) "Prior context · " "Frozen original · ") (name (:authority source))))
                  (el/p {} (:path source))
                  (el/p {} (str "Recorded source revision: " revision))
                  (el/pre {:class "conversation-capture-source"} (:text source))
                  (when-let [context (model/message-context room-id source)]
                    (el/a {:href (routes/ref->route context)} "Open current message context separately"))
                  (el/a {:href (links/route {:kind :memory-file :room (str room-id) :path (:path source)})}
                        "Inspect current file separately")))))))
      (el/details {}
        (el/summary {} "All selected originals and coverage")
        (for [{:keys [source-revision status note]} (:coverage report)
                :let [source (get by-revision source-revision)]]
          (el/details {:key (str source-revision)}
            (el/summary {} (str (name status) " · " (:path source)))
            (el/p {} note)
            (el/p {} (str "Exact retained revision: " source-revision))
            (el/pre {:class "conversation-capture-source"} (:text source)))))
      (el/a {:href (str "/room/" room-id)} "Open current conversation context"))))

(defn panel [{:keys [digest-state message-id room-id] :as data}]
  (let [s digest-state a (actions/controller data) p (:preview s)
        selected-model (or (:model s) (get-in p [:default-model :id]) "")]
    (when (and (:visible? s) (= message-id (:message s)))
      (el/section {:class "conversation-capture-panel" :aria-label "Optional room digest"}
        (el/div {:class "conversation-capture-heading"}
          (el/strong {} "Optional room digest")
          (el/button {:type "button" :on-click (fn [e] (.stopPropagation e) ((:hide a)))} "×"))
        (case (:phase s)
          :preview-loading (el/p {:role "status"} "Verifying retained originals…")
          :preview (el/div {}
                     (el/p {} (str (count (get-in p [:selection :sources])) " exact originals frozen for this optional digest."))
                     (el/details {}
                       (el/summary {} "Inspect original selection")
                       (for [source (get-in p [:selection :sources])]
                         (el/details {:key (:path source)}
                           (el/summary {} (:path source))
                           (el/p {} (str "Exact file content: " (:file-content source)))
                           (el/pre {:class "conversation-capture-source"} (get (:inputs p) (str "/" (:path source)))))))
                     (if (= :missing (:workflow p))
                       (el/div {}
                         (el/p {} "The optional digest workflow is missing. Installation starts no work and preserves existing custom source.")
                         (el/button {:type "button" :on-click (fn [e] (.stopPropagation e) ((:install a)))} "Install optional digest workflow"))
                       (when (= :installed (:workflow p))
                         (el/div {}
                           (el/label {} "Model"
                             (el/input {:type "text" :value selected-model :placeholder "Select an available model"
                                        :on-input (fn [e] ((:model a) (.. e -target -value)))}))
                           (el/p {} "Maximum 25,000 microdollars · 4 dispatches · 8 steps · 180 seconds. Existing room allowance; discard settlement.")
                           (el/button {:type "button" :disabled (empty? selected-model)
                                       :on-click (fn [e] (.stopPropagation e) ((:launch a) selected-model))} "Review and run digest"))))
                     (el/a {:href (workflows/route {:room (str room-id) :workflow "summarize-room" :action :run})}
                           "Inspect editable workflow"))
          :installing (el/p {:role "status"} "Installing inactive source…")
          :installed (el/p {:role "status"} "Installed. Prepare the digest again to inspect this source and select a model.")
          :launching (el/p {:role "status"} "Submitting one finite ordinary workflow…")
          :submitted (el/p {:role "status"} "Submitted. Load the result after completion; this view does not poll or rerun it.")
          :result-loading (el/p {:role "status"} "Checking retained Request, certified output and frozen originals…")
          :verified (el/div {}
                      (result-card {:result (:result s) :room-id room-id})
                      (when-not (:publication-pending? s)
                        (el/button {:type "button" :on-click (fn [e] (.stopPropagation e) ((:publish a)))} "Keep digest in chat")))
          :publishing (el/p {:role "status"} "Keeping the validated digest in chat…")
          :published (el/p {:role "status"} "Digest kept in chat. Reloading this view preserves its summary and exact retained-result pointer.")
          :inspection-required (el/p {:role "alert"} "The operation outcome needs inspection. It will not be retried automatically.")
          (el/p {:role "alert"} "The digest or completed result is unavailable. Original evidence remains unchanged."))
        (when (:request s)
          (el/div {}
            (el/p {} (str "Request: " (:request s)))
            (el/button {:type "button" :on-click (fn [e] (.stopPropagation e) ((:result a)))} "Load completed digest")))))))


(defn notification-card
  "A native message is the durable navigation group; full originals load only
   on explicit read through the same certified retained-result endpoint."
  [{:keys [content room-id digest-state notification] :as data}]
  (let [a (actions/controller data) s digest-state]
    (el/details {:class "room-digest-result"
                 :on-toggle (fn [event]
                              (when (and (identical? (.-target event) (.-currentTarget event))
                                         (.-open (.-currentTarget event)) notification
                                         (not (contains? #{:result-loading :verified :unavailable} (:phase s))))
                                ((:result a))))}
      (el/summary {} (str "Summary · " content))
      (el/p {} "Generated summary. Original evidence is preserved; wiki updates are separate.")
      (if notification
        (el/div {}
          (el/details {}
            (el/summary {} "Run details")
            (el/p {} (str "Request " (:request notification) " · digest " (:digest-id notification))))
          (case (:phase s)
            :verified (result-card {:room-id room-id :result (:result s)})
            :result-loading (el/p {:role "status"} "Checking retained result and original revisions…")
            :unavailable (el/p {:role "alert"} "The exact retained digest is unavailable. Current files are not substituted.")
            nil))
        (el/p {:role "alert"} "The retained digest pointer is unavailable.")))))
