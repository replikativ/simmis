(ns is.simm.uis.web.desktop.views.conversation-capture
  "Small inline evidence export. Preview text never becomes rendered Markdown."
  (:require [org.replikativ.spindel.dom.elements :as el]
            [is.simm.documents.workflow-links :as workflow-links]
            [is.simm.uis.web.desktop.conversation-capture-actions :as actions]))

(defn control [{:keys [state] :as data}]
  (let [a (actions/controller data)]
      (el/button {:type "button" :class "chat-message-reply" :title "Save this human message as evidence in Memory"
                  :on-click (fn [e] (.stopPropagation e) ((:preview a)))}
                 (if (= (:phase state) :saved) "Saved to Memory" "Save to Memory"))))

(defn panel [{:keys [state] :as data}]
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
