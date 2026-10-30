(ns is.simm.uis.web.desktop.views.workspace-creation
  "One canonical file-backed workspace, with Team and Memory entry points."
  (:require [org.replikativ.spindel.dom.elements :as el]
            [org.replikativ.spindel.effects.track :refer [track]]
            [org.replikativ.spindel.incremental.interval :as iv]
            [is.simm.uis.web.desktop.signals :as sig]
            [is.simm.uis.web.desktop.workspace-actions :as actions]
            [is.simm.uis.web.desktop.workspace-editor :as editor]
            [is.simm.uis.web.desktop.views.core :as vc])
  (:require-macros [org.replikativ.spindel.spin.cps :refer [spin]]))

(defn- outcome-text [outcome]
  (case (:status outcome)
    :created "Your workspace is ready."
    :preparing "Workspace setup is not complete yet."
    :prepared-not-published "Files are prepared, but the workspace has not been published. Check again; if setup failed, it needs recovery rather than another creation request."
    :not-observed "Creation has not been observed yet. The request may still be queued; check this creation again."
    :conflict "This creation ID belongs to a different request. Keep this result for diagnosis."
    :unavailable "Creation could not be confirmed. Check this creation before starting another."
    nil))

(defn render [data]
  (spin
   (let [states (iv/get-new (track sig/workspace-creation-state))
         _socket (iv/get-new (track sig/memory-connection))
         _layout (iv/get-new (track sig/layout-columns))
         _focus (iv/get-new (track sig/active-column-id))
         rooms (iv/get-new (track sig/user-rooms))
         user (iv/get-new (track sig/current-user))
         a (actions/controller data) state (get states (:owner a))
         submitted? (some? (:submission state)) memory? (= :memory (:landing data))
         github? (= :github (:source-kind data))
         created? (editor/created? (:outcome state))
         created-id (get-in state [:outcome :room :room/id])
         visible? (some #(= created-id (str (:room/id %))) (:rooms rooms))]
     ((:ensure a))
     (el/div {:class "settings-page workspace-creation"}
             (el/div {:class "settings-header"}
                     (vc/icon (if memory? "folder" "users") {:class "settings-header-icon"})
                     (el/h2 {} (cond github? "Import GitHub repository" memory? "New knowledge workspace" :else "New team")))
             (el/p {:class "settings-section-desc"}
                   (if github?
                     "Import a public repository into Memory. Files stay unchanged; imported code is not executed. Pull applies proven fast-forwards; merges are proposed for review. Memory settings can require review of every update."
                     "A shared place for chat, Markdown knowledge, files and workflow source. Wiki maintenance is included; nothing is scheduled automatically."))
             (when github?
               (el/div {:class "settings-section"}
                       (el/p {:class "memory-note"}
                             "Public repositories only: up to 1,024 regular or executable files, 4 MiB per Git object and 32 MiB checked out. Symlinks and submodules are not imported.")
                       (el/label {:class "settings-section-title"}
                                 "Public GitHub repository"
                                 (el/input {:class "settings-input" :aria-label "Public GitHub repository"
                                            :type "text" :maxlength 256 :disabled submitted?
                                            :value (or (get-in state [:submission :source :repository]) (:repository state) "")
                                            :placeholder "owner/repository"
                                            :on-input (fn [event] ((:set-repository a) (.. event -target -value)))}))))
             (el/div {:class "settings-section"}
                     (el/label {:class "settings-section-title"}
                               "Name"
                               (el/input {:class "settings-input" :aria-label "Workspace name"
                                          :type "text" :maxlength 512 :disabled submitted?
                                          :value (or (get-in state [:submission :name]) (:name state) "")
                                          :placeholder (if memory? "e.g. Research notebook" "e.g. Product team")
                                          :on-input (fn [event] ((:set-name a) (.. event -target -value)))})))
             (when-not memory?
               (el/div {:class "settings-section"}
                       (el/h3 {:class "settings-section-title"} "People (optional)")
                       (el/p {:class "settings-section-desc"} "You are included automatically. More people and agents can be added later.")
                       (when (:people-error state) (el/p {:class "memory-note"} (:people-error state)))
                       (for [person (:people state)
                             :let [id (str (:party/id person))]
                             :when (not= id (:id user))]
                         (el/label {:key id :class "settings-model-option"}
                                   (el/input {:type "checkbox" :disabled submitted?
                                              :checked (contains? (set (or (get-in state [:submission :members]) (:members state))) id)
                                              :on-change (fn [_] ((:toggle-member a) id))})
                                   (el/span {} (or (:party/display-name person) (:party/email person)))))))
             (when-let [error (:error state)] (el/p {:role "alert" :class "proposal-error"} error))
             (when-let [message (outcome-text (:outcome state))]
               (el/p {:role "status"} message))
             (when-let [correlation (get-in state [:outcome :correlation-id])]
               (el/p {:class "memory-note"} (str "Reference: " correlation)))
             (if-not submitted?
               (el/button {:class "settings-btn settings-btn--primary"
                           :disabled (not (editor/valid? state))
                           :on-click (fn [_] ((:create a)))}
                          (cond github? "Import repository" memory? "Create workspace" :else "Create team"))
               (if created?
                 (el/button {:class "settings-btn settings-btn--primary" :disabled (not visible?)
                             :on-click (fn [_] ((:open a)))}
                            (if visible? "Open workspace" "Adding workspace to navigation…"))
                 (el/div {}
                         (when (= :submitting (:phase state)) (el/p {:role "status"} "Creating workspace…"))
                         (when (= :unconfirmed (:phase state))
                           (el/div {}
                                   (el/button {:class "settings-btn" :disabled (boolean (get-in state [:loading :observe]))
                                               :on-click (fn [_] ((:check a)))} "Check creation")
                                   (el/p {:class "memory-note"}
                                         "This checks the same request. Reconnecting or checking does not create a second workspace."))))))))))
