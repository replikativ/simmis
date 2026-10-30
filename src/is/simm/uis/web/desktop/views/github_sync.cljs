(ns is.simm.uis.web.desktop.views.github-sync
  (:require [org.replikativ.spindel.dom.elements :as el]
            [is.simm.documents.github-source :as source]
            [is.simm.uis.web.desktop.connection-epoch :as connection]
            [is.simm.uis.web.desktop.github-sync :as sync]
            [is.simm.uis.web.desktop.github-credentials :as credential-state]
            [is.simm.uis.web.desktop.signals :as sig]))

(defn- click [action]
  (fn [event] (when (.. event -currentTarget -isConnected) (action))))

(defn- compare-url [repository before after]
  (when (and (string? before) (re-matches #"[0-9a-f]{40}" before)
             (string? after) (re-matches #"[0-9a-f]{40}" after))
    (str "https://github.com/" (source/repository! repository) "/compare/" before "..." after)))

(defn retirement-control [memory a state]
  (let [origin (:github-source memory) finished? (sync/terminal (get-in state [:reply :status]))
        enabled? ((:enabled? a)) loading? (:loading? state)
        offer (sync/action-basis state memory (connection/snapshot))]
    (when (and (:can-retire? origin)
               (or (not= :push (:sync-kind origin)) (nil? (:sync-client-id origin)))
               (not= (:sync-id origin) (:retired-sync-id state))
               (or (nil? (:intent state)) finished?)
               (or (nil? (:sync-client-id origin))
                   (not= (:sync-client-id origin) (get-in state [:intent :client-id]))))
      (el/button {:disabled (or loading? (not enabled?)) :on-click (click #((:retire a) offer))}
                 "Close previous owner's Git preparation"))))

(defn review-pull [memory a state]
  (when (and (:github-source memory) (:open? state))
    (let [origin (:github-source memory) reply (:reply state) status (:status reply)
          loading? (:loading? state) enabled? ((:enabled? a))
          local-history? (= :local-history (:reason reply))
          stopped? (and (= :held status) (:can-discard? reply)) offer (sync/action-basis state memory (connection/snapshot))]
      (el/section {:class "memory-local-source memory-remote-review" :role "dialog" :aria-label "Review incoming changes"
                   :on-keydown (fn [event] (when (= "Escape" (.-key event)) (.stopPropagation event) ((:open a) false)))}
                  (el/div {:class "memory-local-source-heading"}
                          (el/h3 {} "Pull remote changes")
                          (el/button {:class "btn btn-sm" :aria-label "Close pull review" :on-click (click #((:open a) false))} "Close"))
                  (el/div {:class "memory-properties" :aria-label "Review incoming changes"}

                          (when loading?
                            (el/p {:role "status"}
                                  (case (:operation state)
                                    :apply (if (= :incoming (:operation-kind reply)) "Creating the review proposal…" "Applying the reviewed update…")
                                    (:discard :retire) "Closing this update attempt. Your saved files will be kept…"
                                    (:observe :recover) "Checking this update attempt…"
                                    "Checking the remote for updates in a private copy…")))
                          (when-let [error (:error state)] (el/p {:class "proposal-error" :role "alert"} error))
                          (when-let [reason (:reason reply)]
                            (el/p {:class "memory-note" :role "status"}
                                  (case reason
                                    :conflicts "The incoming changes overlap your saved edits. Nothing was overwritten. Close this attempt before preparing a resolved replacement."
                                    :document-validation "The merged documents do not satisfy the Memory's current types or properties. Nothing was applied."
                                    :markdown-removal-requires-review "This update removes or moves Markdown documents. Automatic integration is held to preserve their identities."
                                    :incoming-reserved "This incoming revision already has a pending review. Check that proposal before preparing another."
                                    :dirty "The saved worktree or index has local changes. Keep them and resolve them before syncing."
                                    :diverged "The histories diverged. Sync will not reset or merge your Memory."
                                    :target-changed "Memory changed after review. The prepared update was not reapplied."
                                    :source-changed "The tracked Git source is unavailable or changed. No replacement was selected."
                                    :local-history (if stopped?
                                                     "Your edits are saved in this Memory. No remote update was applied and nothing was pushed. This attempt could not verify the retained history needed for integration."
                                                     "Local history prevented preparing this update. Check this attempt to confirm its saved state. Your edits stay in Memory.")
                                    :limit "The import or review exceeds a fixed size or complexity limit."
                                    :timeout "The bounded Git operation reached its deadline."
                                    :cancelled "The Git operation was cancelled."
                                    :busy "Another native Git operation is active."
                                    :access-unverified "Current room access could not be verified."
                                    :integration-unverified "The last integrated remote revision needs verification. No new update will be applied by guessing from the current files."
                                    "The outcome needs inspection before another action.")))
                          (when (= :ready status)
                            (el/div {}
                                    (el/p {} (str (:total reply) " changed " (if (= 1 (:total reply)) "file. " "files. ")
                                                  (if (= :incoming (:operation-kind reply))
                                                    "The prepared integration preserves your saved edits. Creating a proposal does not change Memory."
                                                    "Apply advances only this tracked branch. No push is performed.")))
                                    (when-let [url (compare-url (:repository origin) (:before-git-oid reply) (:git-oid reply))]
                                      (el/a {:href url :target "_blank" :rel "noopener noreferrer"} "View upstream diff"))
                                    (el/table {}
                                              (el/thead {} (el/tr {} (el/th {} "Change") (el/th {} "File")))
                                              (el/tbody {}
                                                        (for [{:keys [path kind]} (:changes reply)]
                                                          (el/tr {:key path} (el/td {} (name kind)) (el/td {} path)))))
                                    (el/button {:class "btn btn-affirm btn-sm" :disabled (or loading? (not ((:can-apply? a))))
                                                :on-click (click #((:apply a) offer))}
                                               (if (= :incoming (:operation-kind reply)) "Create review proposal" "Apply reviewed update"))))
                          (when stopped?
                            (el/p {:class "memory-note"}
                                  "Close this attempt to release its private copy. This keeps your saved edits and leaves the remote unchanged."))
                          (when (and (#{:unconfirmed :preparing :held :not-observed} status) (not stopped?))
                            (el/div {}
                                    (el/p {:class "memory-note"} "This sync needs inspection. Checking never fetches or applies again.")
                                    (el/button {:disabled (or loading? (not enabled?)) :on-click (click #((:check a) offer))} "Check this sync")
                                    (when (= :unconfirmed status)
                                      (el/button {:disabled (or loading? (not enabled?)) :on-click (click #((:recover a) offer))}
                                                 "Verify current outcome"))))
                          (when (:can-discard? reply)
                            (el/button {:disabled (or loading? (not enabled?)) :on-click (click #((:discard a) offer))}
                                       (if (and local-history? stopped?) "Close attempt (keep edits)" "Discard private preparation"))))))))

(defn review-push [memory a state]
  (when (and (:github-source memory) (:open? state))
    (let [reply (:reply state) status (:status reply) loading? (:loading? state) offer (sync/action-basis state memory (connection/snapshot))]
      (el/section {:class "memory-local-source memory-remote-review" :role "dialog" :aria-label "Review outbound changes"
                   :on-keydown (fn [event] (when (= "Escape" (.-key event)) (.stopPropagation event) ((:open a) false)))}
                  (el/div {:class "memory-local-source-heading"}
                          (el/h3 {} "Push to remote")
                          (el/button {:class "btn btn-sm" :aria-label "Close push review" :on-click (click #((:open a) false))} "Close"))
                  (when loading? (el/p {:class "memory-note" :role "status"} "Preparing or checking this push…"))
                  (when-let [error (:error state)] (el/p {:class "proposal-error" :role "status"} error))
                  (when (= :ready status)
                    (el/div {}
                            (el/p {:class "memory-note"} (str (:total reply) " changed " (if (= 1 (:total reply)) "file" "files")
                                                              ". Push publishes the reviewed commit; it does not merge remote changes."))
                            (el/table {}
                                      (el/thead {} (el/tr {} (el/th {} "Change") (el/th {} "File")))
                                      (el/tbody {} (for [{:keys [path kind]} (:changes reply)]
                                                     (el/tr {:key path} (el/td {} (name kind)) (el/td {} path)))))
                            (el/button {:class "btn btn-affirm btn-sm" :disabled (or loading? (not ((:can-apply? a))))
                                        :on-click (click #((:apply a) offer))} "Push reviewed changes")))
                  (when (#{:unconfirmed :preparing :held :not-observed} status)
                    (el/div {}
                            (el/p {:class "memory-note"} "Inspect this same attempt before starting another. Verification never sends again.")
                            (el/button {:class "btn btn-sm" :disabled loading? :on-click (click #((:check a) offer))} "Check attempt")
                            (when (= :unconfirmed status)
                              (el/button {:class "btn btn-sm" :disabled loading? :on-click (click #((:recover a) offer))} "Verify remote head"))))
                  (when (:can-discard? reply)
                    (el/button {:class "btn btn-sm" :disabled loading? :on-click (click #((:discard a) offer))}
                               (if (= :ready status) "Cancel prepared push" "Close attempt")))))))

(defn credential-view [memory state credentials]
  (let [credential (:credential-reply state) loading? (:credential-loading? state)
        offer (credential-state/action-basis (sync/action-basis state memory (connection/snapshot)) state)
        accounts (filterv :configured? (:accounts credential))]
    (el/div {:class "memory-properties"}
            (el/p {:class "memory-note"}
                  "Choose your personal GitHub connection and authorize it for this repository. Room membership does not share your token.")
            (when loading? (el/p {:class "memory-note"} "Checking repository access…"))
            (when-let [error (:credential-error state)] (el/p {:class "proposal-error" :role "status"} error))
            (when (and credential (seq accounts))
              (el/form {:on-submit (fn [event]
                                     (.preventDefault event)
                                     (when (.. event -currentTarget -isConnected)
                                       (let [id (.-value (.namedItem (.. event -currentTarget -elements) "personal-connection"))
                                             selected (some #(when (= id (:id %)) %) accounts)]
                                         (when selected ((:bind credentials) offer selected)))))}
                       (el/label {} "Your connection"
                                 (el/select {:name "personal-connection" :value (or (some #(when (= (:id %) (:account-id credential)) (:id %)) accounts)
                                                                                    (:id (first accounts)))
                                             :aria-label "Personal GitHub connection"}
                                            (for [account accounts]
                                              (el/option {:key (:id account) :value (:id account)} (:name account)))))
                       (el/button {:class "btn btn-sm" :type "submit" :disabled loading?} "Authorize this Memory")))
            (when (and credential (empty? accounts))
              (el/p {:class "memory-note"} "Connect GitHub in your personal settings first."))
            (when (:configured? credential)
              (el/p {:class "memory-note"} "Your selected connection is authorized for this repository."))
            (when (:account-id credential)
              (el/button {:class "btn btn-sm" :disabled loading?
                          :on-click (click #((:revoke credentials) offer))} "Unlink this Memory"))
            (el/button {:class "btn btn-sm" :disabled loading?
                        :on-click (click #((:reload credentials) offer))} "Reload access")
            (el/button {:class "btn btn-sm"
                        :on-click (click #(sig/open-or-activate-tab! :settings nil {:title "Settings"}))} "Manage personal connections"))))

(defn- active? [state]
  (and (:intent state) (not (sync/terminal (get-in state [:reply :status])))))

(defn- pending-for-another? [origin state]
  (and (:sync-active? origin)
       (not= (:sync-id origin) (:retired-sync-id state))
       (not (and (:sync-client-id origin) (= (:sync-client-id origin) (get-in state [:intent :client-id]))))))

(defn toolbar [memory pull pull-state push push-state direction open-settings!]
  (when-let [origin (:github-source memory)]
    (let [pull-offer (sync/action-basis pull-state memory (connection/snapshot))
          push-offer (sync/action-basis push-state memory (connection/snapshot))
          busy? (or (:loading? pull-state) (:loading? push-state))
          selected (if (= :push direction) push-state pull-state)
          reply (:reply selected) status (:status reply)
          status-text (cond
                        busy? "Syncing…"
                        (= :ready status) "Changes prepared"
                        (#{:unconfirmed :held :preparing :not-observed} status) "Sync needs attention"
                        (= :not-started status) "Sync did not start"
                        (= :filed status) "Incoming proposal"
                        (= :unchanged status) (if (= :push direction) "No outgoing changes" "Up to date")
                        (= :pushed (:outcome reply)) "Pushed"
                        (= :rejected (:outcome reply)) "Push rejected"
                        (#{:upstream-observed :upstream-unchanged} (:outcome reply)) "Remote head verified"
                        :else nil)
          last-sync (:last-sync origin)]
      (el/div {:class "memory-toolbar memory-remote-toolbar" :aria-label "Git synchronization"}
              (el/button {:class "btn btn-sm"
                          :disabled (or busy? (not ((:enabled? pull))) (active? push-state) (pending-for-another? origin pull-state))
                          :on-click (click #(if (active? pull-state) ((:open pull) true) ((:sync pull) pull-offer)))} "Pull")
              (el/button {:class "btn btn-sm"
                          :disabled (or busy? (not ((:enabled? push))) (active? pull-state) (pending-for-another? origin push-state))
                          :on-click (click #(cond
                                              (active? push-state) ((:open push) true)
                                              (get-in push-state [:credential-reply :configured?]) ((:sync push) push-offer)
                                              :else (open-settings!)))} "Push")
              (when status-text
                (cond
                  (= :filed status)
                  (el/button {:class "btn btn-sm" :on-click (click (:open-proposal pull))} status-text)
                  (= :not-started status)
                  (el/button {:class "btn btn-sm" :on-click (click open-settings!)} status-text)
                  :else (el/span {:class "memory-note" :role "status"} status-text)))
              (when-let [at (:at last-sync)]
                (el/span {:class "memory-note" :title (str at " · Integrated remote commit " (:git-oid last-sync))}
                         (str "Last sync " (.toLocaleString (js/Date. at)))))))))

(defn settings-view [data memory memory-actions pull pull-state push-state credentials]
  (when (:settings-open? memory)
    (let [origin (:github-source memory) close! #((:settings-open memory-actions) false)
          recent (if (= :push (:last-git-direction data)) push-state pull-state)]
      (el/section {:class "memory-local-source memory-settings-dialog" :role "dialog" :aria-label "Memory settings"
                   :on-keydown (fn [event]
                                 (when (= "Escape" (.-key event)) (.stopPropagation event) (close!)))}
                  (el/div {:class "memory-local-source-heading"}
                          (el/h3 {} "Memory settings")
                          (el/button {:class "btn btn-sm" :on-click (click close!) :aria-label "Close Memory settings"} "Close"))
                  (el/p {:class "memory-note"} (or (:room-name data) "Memory"))
                  (when (= :not-started (get-in recent [:reply :status]))
                    (el/p {:class "proposal-error" :role "status"}
                          (case (get-in recent [:reply :reason])
                            :credential-required "Configure your remote access, then prepare the push again."
                            :credential-changed "Remote access changed. Check its current settings, then prepare a new review."
                            :busy "Another Git operation is finishing. Try again after it completes."
                            "The operation did not start. Check the linked remote and current Memory access before retrying.")))
                  (el/h4 {} "Remote")
                  (if origin
                    (el/div {}
                            (el/p {}
                                  (el/a {:href (str "https://github.com/" (source/repository! (:repository origin)))
                                         :target "_blank" :rel "noopener noreferrer"} (:repository origin))
                                  (el/span {:class "memory-note"} " · GitHub · Git"))
                            (el/p {:class "memory-note"} (str "Tracked branch: " (:ref origin)))
                            (when-not (:available? origin)
                              (el/p {:class "proposal-error"} "The remote connection changed and needs verification. Sync is held."))
                            (retirement-control memory pull pull-state)
                            (when ((:enabled? pull))
                              (el/div {}
                                      (el/h4 {} "Repository access")
                                      (credential-view memory push-state credentials))))
                    (el/p {:class "memory-note"} "This Memory has no linked remote."))))))
