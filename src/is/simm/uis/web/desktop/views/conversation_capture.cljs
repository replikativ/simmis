(ns is.simm.uis.web.desktop.views.conversation-capture
  "Small inline evidence export. Preview text never becomes rendered Markdown."
  (:require [org.replikativ.spindel.dom.elements :as el]
            [is.simm.documents.workflow-links :as workflow-links]
            [is.simm.uis.web.desktop.conversation-capture-actions :as actions]))

(defn- room-data [data]
  (assoc data :room-evidence? true :state (:room-state data)))

(defn control [{:keys [state] :as data}]
  (let [a (actions/controller data) room (actions/controller (room-data data))]
    (el/span {}
      (el/button {:type "button" :class "chat-message-reply" :title "Save this human message as evidence in Memory"
                  :on-click (fn [e] (.stopPropagation e) ((:preview a)))}
                 (if (= (:phase state) :saved) "Saved to Memory" "Save to Memory"))
      (el/button {:type "button" :class "chat-message-reply"
                  :on-click (fn [e] (.stopPropagation e) ((:preview room)))} "Collect room evidence"))))

(defn- human-panel [{:keys [state] :as data}]
  (let [a (actions/controller data) phase (:phase state)]
      (when (:visible? state)
        (el/section {:class "conversation-capture-panel" :aria-label "Save conversation to Memory"}
          (el/div {:class "conversation-capture-heading"}
                  (el/strong {} "Conversation evidence")
                  (el/button {:type "button" :aria-label "Close capture preview"
                              :on-click (fn [e] (.stopPropagation e) ((:hide a)))} "×"))
          (case phase
            :preview-loading (el/p {:role "status"} "Preparing preview…")
            :preview (el/div {}
                       (el/p {} "Save an exact snapshot in Memory. This does not summarize or update the wiki. Later chat changes will not update the file.")
                       (el/p {:class "conversation-capture-path"} (get-in state [:preview :path]))
                       (el/details {}
                         (el/summary {} "Inspect exact evidence")
                         (el/pre {:class "conversation-capture-source"} (get-in state [:preview :text])))
                       (el/button {:type "button" :on-click (fn [e] (.stopPropagation e) ((:publish a)))} "Save evidence"))
            :publishing (el/p {:role "status"} "Saving evidence…")
            :saved (el/div {}
                     (el/p {:role "status"} "Saved to Memory. It is evidence for workflows, not an adopted wiki change.")
                     (el/button {:type "button" :on-click (fn [e] (.stopPropagation e) ((:open a)))} "Open saved file")
                     (el/a {:href (workflow-links/route {:room (str (:room-id data))
                                                        :workflow "maintain-wiki" :action :run})}
                           "Review knowledge update")
                     (el/p {} "Review the saved maintainer’s setup before running. Opening it adds no allowance and starts no work."))
            :inspection-required
            (el/div {}
              (el/p {:role "alert"} "The save outcome needs inspection. It will not be retried automatically.")
              (el/button {:type "button" :on-click (fn [e] (.stopPropagation e) ((:open a)))} "Inspect destination"))
            (el/p {:role "alert"} "Could not prepare this evidence. Check the connection and room permissions, then open the preview again."))))))


(defn- room-panel [data]
  (let [{:keys [state] :as data} (room-data data)
        a (actions/controller data) phase (:phase state) p (:preview state)]
    (when (and (:visible? state) (= [(:message-id data)] (:message-ids state)))
      (el/section {:class "conversation-capture-panel" :aria-label "Collect room evidence"}
        (el/div {:class "conversation-capture-heading"}
          (el/strong {} "Collect room evidence")
          (el/button {:type "button" :aria-label "Close room evidence preview"
                      :on-click (fn [e] (.stopPropagation e) ((:hide a)))} "×"))
        (case phase
          :preview-loading (el/p {:role "status"} "Inspecting original room messages…")
          :preview (el/div {}
                     (el/p {} "Original human and agent messages from this room → its default Memory. Collection saves evidence; it does not update knowledge.")
                     (el/p {} (str (count (:artifacts p)) " original revisions ready. "
                                   (:retained-count p 0) " already retained. "
                                   (if (:complete? p) "Current bounded scan is complete." "More evidence remains or collection is held.")))
                     (el/p {} "Deleted messages and unavailable historical originals cannot be recovered by this scan.")
                     (when (= :held (:state p))
                       (el/p {:role "alert"} (str "Collection held: " (name (get-in p [:held :reason])) ". No evidence will be published.")))
                     (el/details {}
                       (el/summary {} "Inspect exact original evidence")
                       (el/pre {:class "conversation-capture-source"}
                         (apply str (map #(str (:path %) "\n\n" (:text %) "\n") (:artifacts p)))))
                     (when (and (seq (:artifacts p)) (not= :held (:state p)))
                       (el/button {:type "button" :on-click (fn [e] (.stopPropagation e) ((:publish a)))}
                                  "Save this evidence batch")))
          :publishing (el/p {:role "status"} "Saving original revisions…")
          :saved (el/div {}
                   (el/p {:role "status"} "Batch saved. Each original revision remains independently inspectable in Memory. Knowledge processing is separate.")
                   (el/button {:type "button" :on-click (fn [e] (.stopPropagation e) ((:open a)))} "Open first retained original")
                   (el/button {:type "button" :on-click (fn [e] (.stopPropagation e) ((:preview a)))} "Preview next batch"))
          :inspection-required (el/div {}
                                 (el/p {:role "alert"} "Publication outcome needs inspection. Some originals may have been saved. This operation will not be retried.")
                                 (el/button {:type "button" :on-click (fn [e] (.stopPropagation e) ((:open a)))} "Inspect first destination"))
          (el/p {:role "alert"} "Could not inspect room evidence. Check the connection and room permissions."))))))

(defn panel [data]
  (el/div {}
    (human-panel data)
    (room-panel data)))
