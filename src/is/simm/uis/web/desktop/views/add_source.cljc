(ns is.simm.uis.web.desktop.views.add-source
  "Add Memory and Add Team entry points. New workspaces share canonical
   Markdown knowledge, files and workflow source; existing legacy sources stay
   available. Unimplemented external integrations are explicit disabled choices."
  (:require [org.replikativ.spindel.dom.elements :as el]
            [is.simm.uis.web.desktop.views.core :as vc]
            [is.simm.uis.web.desktop.signals :as sig]
            #?(:cljs [org.replikativ.spindel.engine.core :as rtc])
            #?(:cljs [is.simm.uis.web.desktop.runtime :refer [runtime]])
            #?(:cljs [is.simm.runtimes.web :as web]))
  #?(:cljs (:require-macros [org.replikativ.spindel.dom.elements :as el])))

(def catalogs
  "What can become memory, and what can become a team.

   `:ready? false` — the integration is not built. `:note` — it works, but the
   action does not START here, which is the honest description of telegram: the
   team appears because you invited a bot somewhere else."
  {:memory
   {:title "Add memory"
    :blurb "Keep Markdown knowledge and files together in a workspace, or connect an existing source."
    :rows
    [{:id :workspace :verb :create :icon "folder" :label "Knowledge workspace"
      :blurb "A Markdown wiki, files and workflow source in one shared place." :ready? true}
     {:id :mail   :verb :connect :icon "mail" :label "Mail account"
      :blurb "IMAP. Messages are indexed locally and searchable with everything else."
      :ready? true}
     {:id :gdrive :verb :connect :icon "hard-drive" :label "Google Drive"
      :blurb "Documents replicated in as a drive." :ready? false}
     {:id :notion :verb :connect :icon "book-open" :label "Notion"
      :blurb "Pages replicated in as a wiki." :ready? false}
     {:id :github :verb :connect :icon "git-branch" :label "GitHub repository"
      :blurb "Import public repository files and review updates before applying."
      :ready? true}]}

   :team
   {:title "Add team"
    :blurb "A team is a group of people and agents with its own memory, scheduler
            and sandbox. Make one here, or let one arrive from a channel you
            already use."
    :rows
    [{:id :team     :verb :create  :icon "users" :label "Team"
      :blurb "Chat, Markdown knowledge, files and workflows in one workspace." :ready? true}
     {:id :telegram :verb :connect :icon "send" :label "Telegram group"
      :blurb "Add the bot to a Telegram group and its team appears here, mirrored
              both ways."
      :ready? true
      :note "starts in Telegram"}
     {:id :slack    :verb :connect :icon "hash" :label "Slack channel"
      :blurb "A channel as a team, mirrored the way telegram already is." :ready? false}
     {:id :discord  :verb :connect :icon "message-circle" :label "Discord channel"
      :blurb "Same shape as Slack." :ready? false}
     {:id :matrix   :verb :connect :icon "globe" :label "Matrix room"
      :blurb "Federated chat as a team." :ready? false}]}})

#?(:cljs
   (defn- act! [{:keys [id]} {:keys [col-id tab-id tab-type ctx session]}]
     (binding [rtc/*execution-context* ctx]
       (when (and ctx (:id session) (web/current-session? session)
                  (some (fn [column]
                     (and (= col-id (:id column)) (= tab-id (:active-tab column))
                          (some #(and (= tab-id (:id %)) (= tab-type (:type %))) (:tabs column))))
                   @sig/layout-columns))
         (case id
           :workspace (sig/open-tab! :new-kb nil {:col-id col-id :title "New workspace"})
           :github (sig/open-tab! :new-kb {:source-kind :github} {:col-id col-id :title "Import GitHub repository"})
           :mail (sig/open-or-activate-tab! :mail nil {:col-id col-id :title "Mail"})
           :team (sig/open-tab! :new-room nil {:col-id col-id :title "New team"})
           nil)))))

(defn- source-row [{:keys [id verb icon label blurb ready? note] :as src} owner]
  #?(:cljs
     (let [actionable? (and ready? (not note))]
       (el/div {:key (name id)
                :class (str "addmem-row" (when-not actionable? " addmem-row--soon"))
                :on-click (fn [_] (when actionable? (act! src owner)))}
         (vc/icon icon)
         (el/div {:class "addmem-main"}
           (el/div {:class "addmem-label"}
             (el/span {} label)
             (el/span {:class (str "addmem-verb addmem-verb--" (name verb))}
                      (if (= :create verb) "create" "connect")))
           (el/div {:class "addmem-blurb"} blurb))
         (cond
           note (el/span {:class "addmem-soon"} note)
           (not ready?) (el/span {:class "addmem-soon"} "not yet"))))
     :clj nil))

(defn add-source-view
  "`kind` is :memory or :team — see `catalogs`."
  [kind owner]
  #?(:cljs
     (let [{:keys [title blurb rows]} (get catalogs kind)
           owner (assoc owner :tab-type (if (= :memory kind) :add-memory :add-team)
                        :ctx (or rtc/*execution-context* runtime) :session (web/current-session))]
       (el/div {:class "addmem-view"}
         (el/div {:class "addmem-header"}
           (el/h2 {} title)
           (el/p {:class "addmem-sub"} blurb))
         (el/div {:class "addmem-list"}
           (for [r rows] (source-row r owner)))))
     :clj nil))
