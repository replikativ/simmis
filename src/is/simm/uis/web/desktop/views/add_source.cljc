(ns is.simm.uis.web.desktop.views.add-source
  "Add memory / Add team — one entry point per section, both verbs.

   `Create` makes an empty local thing. `Connect` attaches an existing outside
   one. They share an entry point because the user's intent is one — give the
   workspace something — and because a type-named \"New X\" button can never
   express connecting, which is not a new anything.

   For memory, connecting sets an UPSTREAM on a local replica: the content is
   replicated in and the local copy is what you read, fork, search and
   time-travel. dvergr's geschichte substrate already does exactly this for
   repositories (import, record `remote.upstream.url`, read locally). That is
   what keeps a connected source from becoming a second class of memory the
   copy-on-write story cannot reach.

   For teams the same thing is already true and shipped: dvergr's telegram
   channel creates a room when its bot is added to a group. So a team is not
   only something you make here — it can ARRIVE — and the list says so instead
   of pretending creation is the only route.

   Sources that are not built yet are LISTED and disabled with the reason. A
   picker that silently omits Slack answers \"can simmis do this?\" with
   \"apparently not\"; one that names it as not-yet answers correctly."
  (:require [org.replikativ.spindel.dom.elements :as el]
            [is.simm.uis.web.desktop.views.core :as vc]
            [is.simm.uis.web.desktop.signals :as sig]
            #?(:cljs [cljs.core.async :refer [go]])
            #?(:cljs [org.replikativ.spindel.engine.core :as rtc])
            #?(:cljs [is.simm.uis.web.desktop.runtime :refer [runtime]])
            #?(:cljs [is.simm.uis.web.desktop.chat-remote :as cr])
            #?(:cljs [is.simm.uis.web.desktop.remote :as rem])
            #?(:cljs [is.simm.uis.web.desktop.user-rooms-sync :as urs])
            #?(:cljs [is.simm.runtimes.web :as web]))
  #?(:cljs (:require-macros [org.replikativ.spindel.dom.elements :as el])))

(def catalogs
  "What can become memory, and what can become a team.

   `:ready? false` — the integration is not built. `:note` — it works, but the
   action does not START here, which is the honest description of telegram: the
   team appears because you invited a bot somewhere else."
  {:memory
   {:title "Add memory"
    :blurb "Make something new, or connect something that already exists. Either
            way it becomes memory this workspace can search, fork and review —
            a connected source is replicated in, not read through."
    :rows
    [{:id :wiki   :verb :create  :icon "file-text" :label "Wiki"
      :blurb "Pages, blocks and links. The default shape of memory here." :ready? true}
     {:id :drive  :verb :create  :icon "folder" :label "Drive"
      :blurb "Files an agent can read and write through its shell." :ready? true}
     {:id :mail   :verb :connect :icon "mail" :label "Mail account"
      :blurb "IMAP. Messages are indexed locally and searchable with everything else."
      :ready? true}
     {:id :gdrive :verb :connect :icon "hard-drive" :label "Google Drive"
      :blurb "Documents replicated in as a drive." :ready? false}
     {:id :notion :verb :connect :icon "book-open" :label "Notion"
      :blurb "Pages replicated in as a wiki." :ready? false}
     {:id :github :verb :connect :icon "git-branch" :label "GitHub repository"
      :blurb "Code as a geschichte repo — already the substrate agents work in."
      :ready? false}]}

   :team
   {:title "Add team"
    :blurb "A team is a group of people and agents with its own memory, scheduler
            and sandbox. Make one here, or let one arrive from a channel you
            already use."
    :rows
    [{:id :team     :verb :create  :icon "users" :label "Team"
      :blurb "Empty, with its own store, book and agent context." :ready? true}
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
   (defn- create!
     "Run a create spin and refresh the roster.

      The `binding` goes INSIDE the `go`, not around it. A core.async go body is
      dispatched on `nextTick`, so a binding established outside it has already
      unwound by the time the body runs — the spin then finds no execution
      context and THROWS inside the go block, where nothing catches it. Both
      create paths did that: the prompt appeared, nothing was created, and even
      the error callback never fired."
     [what make-spin]
     (go
       (binding [rtc/*execution-context* runtime]
         (when-let [me @sig/current-user]
           (let [s (make-spin (:id me))]
             (s (fn [r]
                  (binding [rtc/*execution-context* runtime]
                    ;; `create-drive!` answers a refusal as `{:status :error}`
                    (if-let [e (rem/result-failure r)]
                      (sig/show-error! (str "Could not create the " what ".")
                                       (rem/error-text e))
                      (urs/refresh-user-rooms! (:id me)))))
                (fn [err]
                  (js/console.error (str "[add-source] create " what " failed:") err)
                  (binding [rtc/*execution-context* runtime]
                    (sig/show-error! (str "Could not create the " what ".")))))))))))

#?(:cljs
   (defn- create-kb! [nm]
     (create! "wiki" (fn [me-id] (cr/create-kb! web/server-id me-id nm)))))

#?(:cljs
   (defn- create-drive! [nm]
     (create! "drive" (fn [_] (cr/create-drive! web/server-id nm)))))

#?(:cljs
   (defn- act! [{:keys [id]}]
     (case id
       :wiki  (when-let [nm (js/prompt "Wiki name:")]  (when (seq nm) (create-kb! nm)))
       :drive (when-let [nm (js/prompt "Drive name:")] (when (seq nm) (create-drive! nm)))
       ;; These already have full forms — send people there rather than
       ;; building a second, thinner one beside each.
       :mail  (binding [rtc/*execution-context* runtime]
                (sig/open-or-activate-tab! :mail nil {:title "Mail"}))
       :team  (binding [rtc/*execution-context* runtime]
                (sig/open-or-activate-tab! :new-room nil {:title "New Team"}))
       nil)))

(defn- source-row [{:keys [id verb icon label blurb ready? note] :as src}]
  #?(:cljs
     (let [actionable? (and ready? (not note))]
       (el/div {:key (name id)
                :class (str "addmem-row" (when-not actionable? " addmem-row--soon"))
                :on-click (fn [_] (when actionable? (act! src)))}
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
  [kind]
  #?(:cljs
     (let [{:keys [title blurb rows]} (get catalogs kind)]
       (el/div {:class "addmem-view"}
         (el/div {:class "addmem-header"}
           (el/h2 {} title)
           (el/p {:class "addmem-sub"} blurb))
         (el/div {:class "addmem-list"}
           (for [r rows] (source-row r)))))
     :clj nil))
