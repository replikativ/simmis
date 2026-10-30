(ns is.simm.uis.web.desktop.views.github-sync
  (:require [org.replikativ.spindel.dom.elements :as el]
            [is.simm.documents.github-source :as source]
            [is.simm.uis.web.desktop.connection-epoch :as connection]
            [is.simm.uis.web.desktop.github-sync :as sync]))

(defn- click [action]
  (fn [event] (when (.. event -currentTarget -isConnected) (action))))

(defn- compare-url [repository before after]
  (when (and (string? before) (re-matches #"[0-9a-f]{40}" before)
             (string? after) (re-matches #"[0-9a-f]{40}" after))
    (str "https://github.com/" (source/repository! repository) "/compare/" before "..." after)))

(defn view [memory a state]
  (when-let [origin (:github-source memory)]
    (let [reply (:reply state) status (:status reply) loading? (:loading? state)
          finished? (sync/terminal status) enabled? ((:enabled? a))
          local-history? (= :local-history (:reason reply))
          stopped? (and (= :held status) (:can-discard? reply))
          offer (sync/action-basis state memory (connection/snapshot))]
      (el/section {:class "memory-github" :aria-label "GitHub source"}
                  (el/div {:class "memory-toolbar"}
                          (el/a {:href (str "https://github.com/" (:repository origin)) :target "_blank" :rel "noopener noreferrer"}
                                (:repository origin))
                          (el/span {:class "memory-note"} (str " · " (:ref origin)))
                          (el/span {:class "memory-note" :title "Sync reviews incoming changes. Outbound publishing uses a separate explicit push review."}
                                   " · Linked Memory")
                          (el/button {:disabled (or (not enabled?) loading? (and (:intent state) (not finished?))
                                                    (and (:sync-active? origin) (not= (:sync-id origin) (:retired-sync-id state))))
                                      :on-click (click #((:sync a) offer))} "Sync from GitHub")
                          (when (:intent state)
                            (el/button {:on-click (click #((:open a) (not (:open? state))))}
                                       (if (:open? state) "Close review" "Sync details"))))
                  (when (and (:can-retire? origin)
                             (or (not= :push (:sync-kind origin)) (nil? (:sync-client-id origin)))
                             (not= (:sync-id origin) (:retired-sync-id state))
                             (or (nil? (:intent state)) finished?)
                             (or (nil? (:sync-client-id origin))
                                 (not= (:sync-client-id origin) (get-in state [:intent :client-id]))))
                    (el/button {:disabled (or loading? (not enabled?)) :on-click (click #((:retire a) offer))}
                               "Close previous owner's Git preparation"))
                  (when (not (:available? origin))
                    (el/p {:class "memory-note"} "The repository or tracked branch changed. GitHub sync is held; it will not select a replacement."))
                  (when finished?
                    (el/p {:class "memory-note" :role "status"}
                          (case status :applied "Updated from the reviewed GitHub changes."
                                :filed "Incoming changes were filed as a proposal. Open it to review the change set and its decision."
                                :not-started "Sync could not start. The source or its retained base needs attention; your saved files were kept."
                                :satisfied-observed "The current Memory matches the prepared update. This observation does not identify who applied it."
                                "Update attempt closed. Your saved files were kept.")))
                  (when (:proposal-id reply)
                    (el/button {:class "btn btn-secondary btn-sm" :on-click (click (:open-proposal a))} "Open proposal"))
                  (when (:open? state)
                    (el/div {:class "memory-properties" :aria-label "Review GitHub changes"}
                            (el/h3 {} (if (and local-history? stopped?) "GitHub update stopped" "Review GitHub changes"))
                            (when loading?
                              (el/p {:role "status"}
                                    (case (:operation state)
                                      :apply (if (= :incoming (:operation-kind reply)) "Creating the review proposal…" "Applying the reviewed update…")
                                      (:discard :retire) "Closing this update attempt. Your saved files will be kept…"
                                      (:observe :recover) "Checking this update attempt…"
                                      "Checking GitHub for updates in a private copy…")))
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
                                                       "Your edits are saved in this Memory. No GitHub update was applied and nothing was pushed. Combining local edits with incoming GitHub commits is not supported yet; restoring the old text alone does not remove the local history."
                                                       "Local history prevented preparing this update. Check this attempt to confirm its saved state. Your edits stay in Memory.")
                                      :limit "The import or review exceeds a fixed size or complexity limit."
                                      :timeout "The bounded Git operation reached its deadline."
                                      :cancelled "The Git operation was cancelled."
                                      :busy "Another native Git operation is active."
                                      :access-unverified "Current room access could not be verified."
                                      :integration-unverified "The last integrated GitHub revision needs verification. No new update will be applied by guessing from the current files."
                                      "The outcome needs inspection before another action.")))
                            (when (= :ready status)
                              (el/div {}
                                      (el/p {} (str (:total reply) " changed " (if (= 1 (:total reply)) "file. " "files. ")
                                                    (if (= :incoming (:operation-kind reply))
                                                      "The prepared integration preserves your saved edits. Creating a proposal does not change Memory."
                                                      "Apply advances only this tracked branch. No push is performed.")))
                                      (when-let [url (compare-url (:repository origin) (:before-git-oid reply) (:git-oid reply))]
                                        (el/a {:href url :target "_blank" :rel "noopener noreferrer"} "View upstream diff on GitHub"))
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
                                    "Close this attempt to release its private copy. This keeps your saved edits and leaves GitHub unchanged."))
                            (when (and (#{:unconfirmed :preparing :held :not-observed} status) (not stopped?))
                              (el/div {}
                                      (el/p {:class "memory-note"} "This sync needs inspection. Checking never fetches or applies again.")
                                      (el/button {:disabled (or loading? (not enabled?)) :on-click (click #((:check a) offer))} "Check this sync")
                                      (when (= :unconfirmed status)
                                        (el/button {:disabled (or loading? (not enabled?)) :on-click (click #((:recover a) offer))}
                                                   "Verify current outcome"))))
                            (when (:can-discard? reply)
                              (el/button {:disabled (or loading? (not enabled?)) :on-click (click #((:discard a) offer))}
                                         (if (and local-history? stopped?) "Close attempt (keep edits)" "Discard private preparation")))))))))

(defn push-view [memory a state credentials]
  (when (and (:github-source memory) ((:enabled? a)))
    (let [reply (:reply state) status (:status reply) loading? (:loading? state)
          origin (:github-source memory) offer ((:basis a)) credential-offer ((:basis credentials))
          credential (:credential-reply state) credential-loading? (:credential-loading? state)
          pending? (and (:sync-active? origin) (not= (:sync-id origin) (:retired-sync-id state)))
          active? (and (:intent state) (not (sync/terminal status)))]
      (el/section {:class "memory-github" :aria-label "Publish to GitHub"}
                  (el/div {:class "memory-toolbar"}
                          (el/button {:class "btn btn-sm" :disabled (or loading? active? pending? (not (:configured? credential)))
                                      :on-click (click #((:sync a) offer))} "Review push")
                          (el/button {:class "btn btn-sm" :aria-expanded (boolean (:credential-open? state))
                                      :on-click (click #((:open credentials) (not (:credential-open? state))))}
                                     "GitHub access")
                          (when (and (not (:configured? credential)) (not (:credential-open? state)))
                            (el/span {:class "memory-note"} "Connect your GitHub access to push.")))
                  (when (:credential-open? state)
                    (el/div {:class "memory-properties"}
                            (el/p {:class "memory-note"}
                                  "Use a fine-grained GitHub token for this repository with Contents read and write. Access belongs to your account; it is not shared with the room.")
                            (cond
                              credential-loading? (el/p {:class "memory-note"} "Checking GitHub access…")
                              (:credential-error state) (el/div {:class "proposal-error" :role "status"}
                                                                (:credential-error state)
                                                                (el/button {:class "btn btn-sm" :on-click (click #((:reload credentials) credential-offer))} "Reload settings"))
                              credential
                              (el/form {:on-submit (fn [event]
                                                     (.preventDefault event)
                                                     (when (.. event -currentTarget -isConnected)
                                                       (let [input (.namedItem (.. event -currentTarget -elements) "github-token")
                                                             token (.-value input)]
                                                         (set! (.-value input) "")
                                                         ((:save credentials) credential-offer token))))}
                                       (el/label {} (if (:configured? credential) "Replace your token" "Repository token")
                                                 (el/input {:type "password" :name "github-token" :autocomplete "off"
                                                            :minlength 20 :maxlength 512 :required true
                                                            :placeholder "Token is stored encrypted on this server"}))
                                       (el/button {:class "btn btn-sm" :type "submit" :disabled credential-loading?} "Save access")
                                       (when (:configured? credential)
                                         (el/button {:class "btn btn-sm" :type "button" :disabled credential-loading?
                                                     :on-click (click #((:revoke credentials) credential-offer))} "Disconnect"))))))
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
                                        :on-click (click #((:apply a) offer))} "Push to GitHub")))
                  (when (and (= :closed status) (:outcome reply))
                    (el/p {:class "memory-note" :role "status"}
                          (case (:outcome reply)
                            :pushed "Published to GitHub."
                            :rejected "GitHub rejected the update. Sync incoming changes before preparing a new push."
                            :upstream-observed "The reviewed commit is present on GitHub. The earlier send outcome remains unknown."
                            :upstream-unchanged "GitHub currently has the old commit. A new push requires a new review."
                            "This push attempt is closed.")))
                  (when (= :unchanged status) (el/p {:class "memory-note"} "No local commits to push."))
                  (when (= :not-started status)
                    (el/p {:class "proposal-error" :role "status"}
                          (case (:reason reply)
                            :credential-required "Connect your GitHub access first."
                            :credential-changed "GitHub access changed. Reload its settings before a new review."
                            "Preparation did not start. Check room access and GitHub settings.")))
                  (when (#{:unconfirmed :preparing :held :not-observed} status)
                    (el/div {}
                            (el/p {:class "memory-note"} "Inspect this same attempt before starting another. Verification never sends again.")
                            (el/button {:class "btn btn-sm" :disabled loading? :on-click (click #((:check a) offer))} "Check attempt")
                            (when (= :unconfirmed status)
                              (el/button {:class "btn btn-sm" :disabled loading? :on-click (click #((:recover a) offer))} "Verify GitHub head"))))
                  (when (:can-discard? reply)
                    (el/button {:class "btn btn-sm" :disabled loading? :on-click (click #((:discard a) offer))}
                               (if (= :ready status) "Cancel prepared push" "Close attempt")))))))
