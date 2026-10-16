(ns is.simm.uis.web.desktop.views.room-settings
  "Room settings tab — view and manage room parties (humans + agents)."
  (:require [org.replikativ.spindel.dom.elements :as el]
            [is.simm.uis.web.desktop.views.core :as vc]
            [is.simm.uis.web.desktop.signals :as sig]
            #?(:cljs [is.simm.uis.web.desktop.room-details :as room-details])
            #?(:cljs [is.simm.uis.web.desktop.chat-remote :as cr])
            #?(:cljs [is.simm.uis.web.desktop.remote :as rem])
            #?(:cljs [is.simm.uis.web.desktop.message-notify-sync :as mns])
            #?(:cljs [is.simm.uis.web.desktop.user-rooms-sync :as urs])
            #?(:cljs [is.simm.runtimes.web :as web]))
  #?(:cljs (:require-macros [org.replikativ.spindel.dom.elements :as el]
                            [org.replikativ.spindel.dom.foreach :refer [ifor-each]])))

(defn render-room-settings
  "Render room settings. data is {:room ... :humans [...] :agents [...] :all-humans [...] ...}."
  [data]
  (let [room (:room data)
        room-id (str (:room/id room))
        humans (:humans data)
        agents (:agents data)
        all-humans (:all-humans data)
        ;; membership and budget are owner-level (access/rpc-policy :grant);
        ;; offer the controls only to whoever the server will let use them
        can-grant? (:can-grant? data)
        current-user #?(:cljs @sig/current-user :clj nil)]

    (el/div {:class "settings-page"}
      (el/div {:class "settings-header"}
        (vc/icon "settings" {:class "settings-header-icon"})
        (el/h2 {} (str (or (:room/name room) "Room") " — Settings")))

      (el/div {:class "settings-sections"}

        ;; --- Notifications (personal, per-room level) ---
        #?(:cljs
           (let [rid   (str (:room/id room))
                 ;; passed in by the column, which TRACKS `sig/notify-prefs`
                 ;; (a bare deref here re-rendered on nothing)
                 level (:notify-level data :mentions)
                 btn   (fn [lvl label]
                         (el/button {:class (vc/class-names "settings-btn"
                                                            (when (= level lvl) "settings-btn--primary"))
                                     :on-click (fn [_] (mns/set-notify-pref! rid lvl))}
                           label))]
             (el/div {:class "settings-section"}
               (el/h3 {:class "settings-section-title"} "Notifications")
               (el/p {:class "settings-section-desc"}
                 "When to get a browser notification for this room. Unread badges show either way.")
               (el/div {:style {:display "flex" :gap "8px" :flex-wrap "wrap"}}
                 (btn :all "All messages")
                 (btn :mentions "Mentions only")
                 (btn :none "None"))))
           :clj nil)

        ;; --- Review decisions (labels) ---
        ;; What people decided on work a review handed them
        ;; (is.simm.workflows.review). Read-only: a label is the record of a
        ;; decision and nothing edits one.
        (let [labels (:labels data)]
          (el/div {:class "settings-section"}
            (el/h3 {:class "settings-section-title"}
              (str "Review decisions" (when labels (str " (" (count labels) ")"))))
            (el/p {:class "settings-section-desc"}
              "Each decision on reviewed work, recorded as a label: whether the work was right, and on spot checks whether the reviewer was.")
            (el/div {:class "settings-env-list"}
              (cond
                (nil? labels)
                (el/div {:class "settings-empty"}
                  "Not available: this room's store is not loaded on the server.")
                (seq labels)
                (ifor-each :id labels
                  (fn [{:keys [id about verdict by-name reasons workflow]}]
                    (el/div {:key id :class "settings-env-row label-row"}
                      (el/span {:class "settings-env-key"}
                        (str (if (= :reviewer about) "Reviewer" "Work")
                             (when (:id workflow) (str " · " (:id workflow)))))
                      (el/span {:class (str "label-verdict label-verdict--" (name verdict))}
                        (case [about verdict]
                          [:reviewer :accept] "agreed"
                          [:reviewer :reject] "disagreed"
                          (name verdict)))
                      (el/span {:class "settings-env-value"}
                        (str "by " by-name (when (seq reasons) (str " — " reasons)))))))
                :else (el/div {:class "settings-empty"} "No decisions yet")))))

        ;; --- Humans Section ---
        (el/div {:class "settings-section"}
          (el/h3 {:class "settings-section-title"}
            (str "Members (" (count humans) ")"))
          (el/div {:class "settings-env-list"}
            (if (seq humans)
              (ifor-each #(str (:party/id %)) humans
                (fn [h]
                  (let [pid (:party/id h)
                        name (:party/display-name h)
                        email (:party/email h)]
                    (el/div {:key (str pid) :class "settings-env-row"}
                      (el/span {:class "settings-env-key"} (or name email "Unknown"))
                      (el/span {:class "settings-env-value"} email)
                      (when (and can-grant? current-user (not= (str pid) (:id current-user)))
                        (el/button {:class "settings-env-delete"
                                    :title "Remove member"
                                    :on-click (fn [_]
                                                #?(:cljs
                                                   (let [s (cr/remove-room-party!
                                                             web/server-id
                                                             (str (:room/id room))
                                                             (str pid))]
                                                     (s (rem/on-ok "Could not remove that member." (fn [_] (room-details/load! room-id {:force? true})))
                                                        (rem/on-error "Could not remove that member.")))
                                                   :clj nil))}
                          (vc/icon "x")))))))
              (el/div {:class "settings-empty"} "No members")))

          ;; Add member
          (when (and can-grant? all-humans)
            (el/div {:class "settings-env-add"}
              (el/select {:id "add-member-select"
                          :class "settings-input"
                          :style {:flex "1"}}
                (el/option {:value ""} "Select user to add...")
                (let [member-ids (into #{} (map :party/id humans))
                      addable (filterv #(not (contains? member-ids (:party/id %))) all-humans)]
                  (ifor-each #(str (:party/id %)) addable
                    (fn [h]
                      (el/option {:key (str (:party/id h)) :value (str (:party/id h))}
                        (str (or (:party/display-name h) (:party/email h))))))))
              (el/button {:class "settings-btn settings-btn--primary"
                          :on-click (fn [_]
                                      #?(:cljs
                                         (let [sel (.getElementById js/document "add-member-select")
                                               pid (.-value sel)]
                                           (when (and pid (seq pid))
                                             (let [s (cr/add-room-party!
                                                       web/server-id
                                                       (str (:room/id room))
                                                       pid)]
                                               (s (rem/on-ok "Could not add that member." (fn [_]
                                                    (set! (.-value sel) "")
                                                    (room-details/load! room-id {:force? true})))
                                                  (rem/on-error "Could not add that member.")))))
                                         :clj nil))}
                "Add"))))

        ;; --- Agents Section ---
        (el/div {:class "settings-section"}
          (el/h3 {:class "settings-section-title"}
            (str "AI Participants (" (count agents) ")"))
          (el/p {:class "settings-section-desc"}
            "Agent parties that respond to messages in this room.")
          (el/div {:class "settings-env-list"}
            (if (seq agents)
              (ifor-each #(str (:party/id %)) agents
               (fn [agent]
                (let [agent-id (str (:party/id agent))]
                  (el/div {:key agent-id :class "settings-agent-card"}
                    (el/div {:class "settings-env-row"}
                      (el/span {:class "settings-env-key"}
                        (or (:party/display-name agent) "Agent"))
                      ;; Desired configuration only. The participant resolves
                      ;; again when it joins; active runtime state is not
                      ;; introspected here and may still hold an earlier spec.
                      (el/span {:class "settings-env-value settings-agent-model"
                                :data-tooltip
                                (or (:runtime-explanation
                                     (:model-resolution agent)) "")}
                        (el/span {:class "settings-agent-model-choice"}
                          (or (:choice-label (:model-resolution agent)) "default"))
                        ;; Printed verbatim from the server. Composing it here
                        ;; promised a resolution even when the model was
                        ;; unavailable and the join would refuse it.
                        (el/span {:class "settings-agent-model-resolution"}
                          (or (:next-join-copy (:model-resolution agent))
                              "Not resolved")))
                      ;; the room's owner removes any agent, a member only
                      ;; their own (access/agent-manageable?)
                      (when (or can-grant?
                                (and current-user
                                     (= (str (get-in agent [:party/owner :party/id]))
                                        (:id current-user))))
                      (el/button {:class "settings-env-delete"
                                  :title "Remove participant"
                                  :on-click (fn [_]
                                              #?(:cljs
                                                 (let [s (cr/remove-agent-from-room!
                                                           web/server-id
                                                           (str (:room/id room))
                                                           agent-id)]
                                                   (s (rem/on-ok "Could not remove that agent." (fn [_] (room-details/load! room-id {:force? true})))
                                                      (rem/on-error "Could not remove that agent.")))
                                                 :clj nil))}
                        (vc/icon "x"))))
                    (el/div {:class "settings-env-add"
                             :style {:margin-top "0.5rem"
                                     :align-items "end"
                                     :flex-wrap "wrap"}}
                      (el/div {:style {:display "flex"
                                      :flex-direction "column"
                                      :gap "0.25rem"}}
                        (el/label {:class "settings-label"
                                   :for (str "agent-role-" agent-id)}
                          "Room role")
                        (el/select {:id (str "agent-role-" agent-id)
                                    :class "settings-input"
                                    :value (name (or (:assignment/role agent)
                                                     :specialist))}
                          (el/option {:value "lead"} "Lead")
                          (el/option {:value "specialist"} "Specialist")
                          (el/option {:value "reviewer"} "Reviewer")
                          (el/option {:value "observer"} "Observer")))
                      (el/div {:style {:display "flex"
                                      :flex-direction "column"
                                      :gap "0.25rem"
                                      :min-width "13rem"}}
                        (el/label {:class "settings-label"
                                   :for (str "agent-policy-" agent-id)}
                          "Respond")
                        (el/select {:id (str "agent-policy-" agent-id)
                                    :class "settings-input"
                                    :value (name (or (:assignment/response-policy agent)
                                                     :always))}
                          (el/option {:value "always"} "To every message")
                          (el/option {:value "mention"} "Only when @mentioned")
                          (el/option {:value "manual"} "Only when started manually")))
                      (el/button
                        {:class "settings-btn settings-btn--secondary"
                         :on-click
                         (fn [_]
                           #?(:cljs
                              (let [role-el (.getElementById
                                             js/document
                                             (str "agent-role-" agent-id))
                                    policy-el (.getElementById
                                               js/document
                                               (str "agent-policy-" agent-id))
                                    s (cr/update-agent-assignment!
                                       web/server-id
                                       (str (:room/id room))
                                       agent-id
                                       (keyword (.-value role-el))
                                       (keyword (.-value policy-el)))]
                                (s (rem/on-ok "Could not save the participation settings." (fn [_] (reset! sig/admin-data nil)))
                                   (rem/on-error "Could not save the participation settings.")))
                              :clj nil))}
                        "Save participation"))
                    (el/div {:class "settings-agent-prompt"}
                      (el/label {:class "settings-label"} "Personality / System Prompt")
                      (el/textarea
                        {:id (str "agent-prompt-" agent-id)
                         :class "settings-textarea"
                         :rows 6
                         :placeholder "Describe the agent's personality and role..."
                         :default-value (or (:party/system-prompt agent) "")}))
                    (el/button
                      {:class "settings-btn settings-btn--primary"
                       :style {:margin-top "0.5rem"}
                       :on-click (fn [_]
                                   #?(:cljs
                                      (let [ta (.getElementById js/document (str "agent-prompt-" agent-id))
                                            sp (.-value ta)
                                            ;; "" leaves the model alone.
                                            s  (cr/update-agent-config!
                                                 web/server-id agent-id
                                                 (:party/display-name agent)
                                                 ""
                                                 sp)]
                                        (s (rem/on-ok "Could not save the agent's prompt." (fn [_] (room-details/load! room-id {:force? true})))
                                           (rem/on-error "Could not save the agent's prompt.")))
                                      :clj nil))}
                      "Save")))))
              (el/div {:class "settings-empty"} "No AI participants in this room")))

          ;; Add agent — template picker
          (el/div {:class "settings-add-agent"}
            (el/h4 {:class "settings-subsection-title"} "Add Agent")
            (el/div {:class "settings-agent-templates"}
              (ifor-each first
                [["secretary"  "Vár"        "Secretary — organizes knowledge, keeps pages coherent" "bot"]
                 ["researcher" "Researcher" "Research — searches, reads, synthesizes"              "search"]
                 ["analyst"    "Analyst"    "Analyst — queries data, builds charts"                "bar-chart-2"]
                 ["coder"      "Coder"      "Coder — writes and debugs code"                       "code-2"]]
                (fn [[tmpl-id tmpl-label tmpl-desc tmpl-icon]]
                  (el/div {:key tmpl-id
                           :class "settings-agent-template-card"
                           :title tmpl-desc
                           :on-click (fn [_]
                                       #?(:cljs
                                          (when current-user
                                            (let [s (cr/add-agent-to-room!
                                                      web/server-id
                                                      (str (:room/id room))
                                                      (:id current-user)
                                                      tmpl-label
                                                      tmpl-id)]
                                              (s (rem/on-ok "Could not add that agent." (fn [_]
                                                   (room-details/load! room-id {:force? true})
                                                   ;; Contacts render from
                                                   ;; sig/user-rooms, which
                                                   ;; nothing here refreshed —
                                                   ;; a new agent showed up only
                                                   ;; after a page reload.
                                                   (urs/refresh-user-rooms! (:id current-user))))
                                                 (rem/on-error "Could not add that agent."))))
                                          :clj nil))}
                    (vc/icon tmpl-icon)
                    (el/span {:class "settings-agent-template-name"} tmpl-label)))))
            ;; Custom agent
            (el/div {:class "settings-env-add" :style {:margin-top "0.5rem"}}
              (el/input {:id "add-agent-name"
                         :type "text"
                         :class "settings-input"
                         :placeholder "Custom name…"})
              (el/button {:class "settings-btn settings-btn--secondary"
                          :on-click (fn [_]
                                      #?(:cljs
                                         (when current-user
                                           (let [input (.getElementById js/document "add-agent-name")
                                                 agent-name (.-value input)]
                                             (when (seq agent-name)
                                               (let [s (cr/add-agent-to-room!
                                                         web/server-id
                                                         (str (:room/id room))
                                                         (:id current-user)
                                                         agent-name
                                                         "")]
                                                 (s (rem/on-ok "Could not add that agent." (fn [_]
                                                      (set! (.-value input) "")
                                                      (room-details/load! room-id {:force? true})
                                                      (urs/refresh-user-rooms! (:id current-user))))
                                                    (rem/on-error "Could not add that agent."))))))
                                         :clj nil))}
                "Add Custom"))))

        ;; --- Budget Section ---
        (el/div {:class "settings-section"}
          (el/h3 {:class "settings-section-title"} "Budget")
          (el/p {:class "settings-section-desc"}
            (if can-grant?
              "Maximum spend in USD for AI responses in this room."
              (str "Maximum spend for AI responses in this room: "
                   (or (:room/budget-dollars room) 10.0) " USD. The room's owner sets it.")))
          (when can-grant?
           (el/div {:class "settings-env-add"}
            (el/input {:id "room-budget-input"
                       :type "number"
                       :class "settings-input"
                       :style {:flex "1" :max-width "120px"}
                       :step "1"
                       :min "0"
                       :ref (fn [node]
                              (when (and node (empty? (.-value node)))
                                (set! (.-value node) (str (or (:room/budget-dollars room) 10.0)))))})
            (el/span {:class "settings-env-value" :style {:padding "0 0.5rem"}} "USD")
            (el/button {:class "settings-btn settings-btn--primary"
                        :on-click (fn [_]
                                    #?(:cljs
                                       (let [input (.getElementById js/document "room-budget-input")
                                             v (js/parseFloat (.-value input))]
                                         (when (and v (not (js/isNaN v)) (>= v 0))
                                           (let [s (cr/update-room-budget!
                                                     web/server-id
                                                     (str (:room/id room))
                                                     v)]
                                             (s (rem/on-ok "Could not save the budget." (fn [_] (room-details/load! room-id {:force? true})))
                                                (rem/on-error "Could not save the budget.")))))
                                       :clj nil))}
              "Save"))))

        ;; --- Knowledge Bases Section ---
        (let [room-kbs (:knowledge-bases data)]
          (el/div {:class "settings-section"}
            (el/h3 {:class "settings-section-title"}
              (str "Wikis (" (count room-kbs) ")"))
            (el/p {:class "settings-section-desc"}
              "Wikis attached to this room. Agents can read/write to these.")
            (el/div {:class "settings-env-list"}
              (if (seq room-kbs)
                (ifor-each #(str (:kb/id %)) room-kbs
                  (fn [kb]
                    (el/div {:key (str (:kb/id kb)) :class "settings-env-row"}
                      (el/span {:class "settings-env-key"} (:kb/name kb))
                      (el/span {:class "settings-env-value"} "attached")
                      ;; the room's owner, or the resource's own owner
                      (when (or can-grant?
                                (and current-user (= (str (:kb/owner kb)) (:id current-user))))
                      (el/button {:class "settings-env-delete"
                                  :title "Detach KB"
                                  :on-click (fn [_]
                                              #?(:cljs
                                                 (let [s (cr/detach-kb-from-room!
                                                           web/server-id
                                                           (str (:room/id room))
                                                           (str (:kb/id kb)))]
                                                   (s (rem/on-ok "Could not detach that wiki." (fn [_] (room-details/load! room-id {:force? true})))
                                                      (rem/on-error "Could not detach that wiki.")))
                                                 :clj nil))}
                        (vc/icon "x"))))))
                (el/div {:class "settings-empty"} "No wikis attached")))

            ;; Attach KB dropdown
            (let [available-kbs (:available-kbs data)
                  attached-ids (into #{} (map #(str (:kb/id %)) room-kbs))
                  unattached (when available-kbs
                               (filterv #(not (contains? attached-ids (str (:kb/id %)))) available-kbs))]
              (when (seq unattached)
                (el/div {:class "settings-env-add"}
                  (el/select {:id "attach-kb-select"
                              :class "settings-input"
                              :style {:flex "1"}}
                    (el/option {:value ""} "Select KB to attach...")
                    (ifor-each #(str (:kb/id %)) unattached
                      (fn [kb]
                        (el/option {:key (str (:kb/id kb)) :value (str (:kb/id kb))}
                          (:kb/name kb)))))
                  (el/button {:class "settings-btn settings-btn--primary"
                              :on-click (fn [_]
                                          #?(:cljs
                                             (let [sel (.getElementById js/document "attach-kb-select")
                                                   kb-id (.-value sel)]
                                               (when (and kb-id (seq kb-id))
                                                 (let [s (cr/attach-kb-to-room!
                                                           web/server-id
                                                           (str (:room/id room))
                                                           kb-id)]
                                                   (s (rem/on-ok "Could not attach that wiki." (fn [_]
                                                        (set! (.-value sel) "")
                                                        (room-details/load! room-id {:force? true})))
                                                      (rem/on-error "Could not attach that wiki.")))))
                                             :clj nil))}
                    "Attach"))))))

        ;; --- Drives (file systems) Section ---
        (let [room-drives (:drives data)]
          (el/div {:class "settings-section"}
            (el/h3 {:class "settings-section-title"}
              (str "Drives (" (count room-drives) ")"))
            (el/p {:class "settings-section-desc"}
              "File systems attached to this room. Agents see the primary drive at /drive in their shell; humans browse it in the Files tab.")
            (el/div {:class "settings-env-list"}
              (if (seq room-drives)
                (ifor-each #(str (:drive/id %)) room-drives
                  (fn [drv]
                    (el/div {:key (str (:drive/id drv)) :class "settings-env-row"}
                      (el/span {:class "settings-env-key"} (:drive/name drv))
                      (el/span {:class "settings-env-value"} "attached")
                      ;; the room's owner, or the resource's own owner
                      (when (or can-grant?
                                (and current-user (= (str (:drive/owner drv)) (:id current-user))))
                      (el/button {:class "settings-env-delete"
                                  :title "Detach drive"
                                  :on-click (fn [_]
                                              #?(:cljs
                                                 (let [s (cr/detach-drive-from-room!
                                                           web/server-id
                                                           (str (:room/id room))
                                                           (str (:drive/id drv)))]
                                                   (s (rem/on-ok "Could not detach that drive." (fn [_] (room-details/load! room-id {:force? true})))
                                                      (rem/on-error "Could not detach that drive.")))
                                                 :clj nil))}
                        (vc/icon "x"))))))
                (el/div {:class "settings-empty"} "No drives attached")))

            ;; Attach existing drive
            (let [available (:available-drives data)
                  attached-ids (into #{} (map #(str (:drive/id %)) room-drives))
                  unattached (when available
                               (filterv #(not (contains? attached-ids (str (:drive/id %)))) available))]
              (when (seq unattached)
                (el/div {:class "settings-env-add"}
                  (el/select {:id "attach-drive-select"
                              :class "settings-input"
                              :style {:flex "1"}}
                    (el/option {:value ""} "Select drive to attach...")
                    (ifor-each #(str (:drive/id %)) unattached
                      (fn [drv]
                        (el/option {:key (str (:drive/id drv)) :value (str (:drive/id drv))}
                          (:drive/name drv)))))
                  (el/button {:class "settings-btn settings-btn--primary"
                              :on-click (fn [_]
                                          #?(:cljs
                                             (let [sel (.getElementById js/document "attach-drive-select")
                                                   drive-id (.-value sel)]
                                               (when (seq drive-id)
                                                 (let [s (cr/attach-drive-to-room!
                                                           web/server-id
                                                           (str (:room/id room))
                                                           drive-id)]
                                                   (s (rem/on-ok "Could not attach that drive." (fn [_]
                                                        (set! (.-value sel) "")
                                                        (room-details/load! room-id {:force? true})))
                                                      (rem/on-error "Could not attach that drive.")))))
                                             :clj nil))}
                    "Attach"))))))))))
