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
          offer (sync/action-basis state memory (connection/snapshot))]
      (el/section {:class "memory-github" :aria-label "GitHub source"}
                  (el/div {:class "memory-toolbar"}
                          (el/a {:href (str "https://github.com/" (:repository origin)) :target "_blank" :rel "noopener noreferrer"}
                                (:repository origin))
                          (el/span {:class "memory-note"} (str " · " (:ref origin)))
                          (el/button {:disabled (or (not enabled?) loading? (and (:intent state) (not finished?))
                                                    (and (:sync-active? origin) (not= (:sync-id origin) (:retired-sync-id state))))
                                      :on-click (click #((:sync a) offer))} "Sync from GitHub")
                          (when (:intent state)
                            (el/button {:on-click (click #((:open a) (not (:open? state))))}
                                       (if (:open? state) "Close review" "Sync details"))))
                  (when (and (:can-retire? origin)
                             (not= (:sync-id origin) (:retired-sync-id state))
                             (or (nil? (:intent state)) finished?)
                             (or (nil? (:sync-client-id origin))
                                 (not= (:sync-client-id origin) (get-in state [:intent :client-id]))))
                    (el/button {:disabled (or loading? (not enabled?)) :on-click (click #((:retire a) offer))}
                               "Discard previous owner's prepared sync"))
                  (when (not (:available? origin))
                    (el/p {:class "memory-note"} "The repository or tracked branch changed. GitHub sync is held; it will not select a replacement."))
                  (when finished?
                    (el/p {:class "memory-note" :role "status"}
                          (case status :applied "Updated from the reviewed GitHub changes."
                                :satisfied-observed "The current Memory matches the prepared update. This observation does not identify who applied it."
                                "The private preparation was discarded.")))
                  (when (:open? state)
                    (el/div {:class "memory-properties" :aria-label "Review GitHub changes"}
                            (el/h3 {} "Review GitHub changes")
                            (when loading? (el/p {:role "status"} "Preparing the retained Git update…"))
                            (when-let [error (:error state)] (el/p {:class "proposal-error" :role "alert"} error))
                            (when-let [reason (:reason reply)]
                              (el/p {:class "memory-note" :role "status"}
                                    (case reason
                                      :dirty "The saved worktree or index has local changes. Keep them and resolve them before syncing."
                                      :diverged "The histories diverged. Sync will not reset or merge your Memory."
                                      :target-changed "Memory changed after review. The prepared update was not reapplied."
                                      :source-changed "The tracked Git source is unavailable or changed. No replacement was selected."
                                      :local-history "Memory has local commits without an imported Git identity. This bounded sync will not reset or merge them."
                                      :limit "The import or review exceeds a fixed size or complexity limit."
                                      :timeout "The bounded Git operation reached its deadline."
                                      :cancelled "The Git operation was cancelled."
                                      :busy "Another native Git operation is active."
                                      :access-unverified "Current room access could not be verified."
                                      "The outcome needs inspection before another action.")))
                            (when (= :ready status)
                              (el/div {}
                                      (el/p {} (str (:total reply) " changed files. Apply advances only this tracked branch. No push or automatic merge is performed."))
                                      (when-let [url (compare-url (:repository origin) (:before-git-oid reply) (:git-oid reply))]
                                        (el/a {:href url :target "_blank" :rel "noopener noreferrer"} "View upstream diff on GitHub"))
                                      (el/table {}
                                                (el/thead {} (el/tr {} (el/th {} "Change") (el/th {} "File")))
                                                (el/tbody {}
                                                          (for [{:keys [path kind]} (:changes reply)]
                                                            (el/tr {:key path} (el/td {} (name kind)) (el/td {} path)))))
                                      (el/button {:disabled (or loading? (not ((:can-apply? a))))
                                                  :on-click (click #((:apply a) offer))} "Apply reviewed update")))
                            (when (#{:unconfirmed :preparing :held :not-observed} status)
                              (el/div {}
                                      (el/p {:class "memory-note"} "This sync needs inspection. Checking never fetches or applies again.")
                                      (el/button {:disabled (or loading? (not enabled?)) :on-click (click #((:check a) offer))} "Check this sync")
                                      (when (= :unconfirmed status)
                                        (el/button {:disabled (or loading? (not enabled?)) :on-click (click #((:recover a) offer))}
                                                   "Verify current outcome"))))
                            (when (:can-discard? reply)
                              (el/button {:disabled (or loading? (not enabled?)) :on-click (click #((:discard a) offer))}
                                         "Discard private preparation"))))))))
