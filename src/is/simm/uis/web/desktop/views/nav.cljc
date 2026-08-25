(ns is.simm.uis.web.desktop.views.nav
  "Multi-project navigation sidebar.

   Features:
   - Feed at top (global activity stream)
   - Starred items (cross-project favorites)
   - Recent items (across all projects)
   - Collapsible project sections with icons

   Uses monochrome project icons that brighten on hover."
  (:require [org.replikativ.spindel.dom.elements :as el]
            [org.replikativ.spindel.incremental.interval :as iv]
            [org.replikativ.spindel.effects.await :refer [await]]
            [org.replikativ.spindel.effects.track :refer [track]]
            [is.simm.uis.web.desktop.views.core :as vc]
            [is.simm.uis.web.desktop.signals :as sig]
            [clojure.string :as str]
            #?(:cljs [datahike.api :as d])
            #?(:cljs [org.replikativ.spindel.spin.core])
            #?(:cljs [org.replikativ.spindel.engine.core :as rtc])
            #?(:cljs [is.simm.uis.web.desktop.runtime :refer [runtime]])
            #?(:cljs [is.simm.uis.web.desktop.db-signal :as db-sig])
            #?(:cljs [is.simm.uis.web.desktop.user-rooms-sync :as urs])
            #?(:cljs [is.simm.uis.web.desktop.message-notify-sync :as mns])
            #?(:cljs [is.simm.uis.web.desktop.chat-remote :as chat-remote])
            #?(:cljs [cljs.core.async :refer [go]])
            #?(:cljs [is.simm.runtimes.web :as web])
            #?(:cljs [is.simm.uis.web.desktop.branching-sync :as br-sync])
            #?(:cljs [is.simm.uis.web.desktop.perspectives-sync :as psync]))
  #?(:cljs (:require-macros [org.replikativ.spindel.dom.elements :as el]
                            [org.replikativ.spindel.spin.cps :refer [spin]]
                            [org.replikativ.spindel.dom.foreach :refer [ifor-each]])))

;; =============================================================================
;; Database Queries
;;
;; NO LOOKUP REF NAMES A SEED ENTITY IN HERE, and that is a requirement rather
;; than a style. The sidebar renders against `ensure-view-db-signal!`, which at
;; a past cut hands over `(d/as-of replica T)` — and a cut older than the store's
;; own install is an ordinary place to stand: seeded content is back-dated to the
;; months the scenario describes while a store not installed at narrative time
;; (`store/install!`) stamps its schema and seed at provisioning. In such a
;; database `[?e :instance/of-role [:entity/name "S/Page"]]` THROWS `Nothing
;; found for entity id [:entity/name "S/Page"]` — a missing lookup ref is an
;; error in datahike, not an empty match (measured 2026-07-27). The clause below
;; is the plain attribute `:S.Page/title`, which matches nothing and throws
;; nothing, so the far left of the rail reads "No pages" instead of taking the
;; column down with it. When a query here genuinely needs the role, resolve it
;; first as a value — `datahike-query/page-role-eid`.
;; =============================================================================

(defn query-pages
  "Query all pages from the database.
   Returns a list of {:id uuid :type :wiki :title string}."
  [db]
  #?(:cljs
     (when db
       (try
         (->> (d/q '[:find [(pull ?e [:entity/uuid :S.Page/title :S.Page/kind :S.Page/archived]) ...]
                     :where [?e :S.Page/title _]]
                   db)
              ;; archived pages and chat-summary RECORDS stay out of the
              ;; browse list (reachable via search/backlinks/timeline)
              (remove #(or (:S.Page/archived %) (= :chat-summary (:S.Page/kind %))))
              (map (fn [{:keys [entity/uuid S.Page/title]}]
                     {:id (str uuid)
                      :uuid uuid
                      :type :wiki
                      :title (or title "Untitled")}))
              (sort-by :title))
         (catch :default e
           (js/console.error "[nav] Failed to query pages:" e)
           [])))
     :clj []))

(defn- kb-page-title-key
  "Normalized title key — same folding the server dedup uses, so the diagnostic
   groups 'Report' / 'report ' together."
  [title]
  (-> (str title) str/trim str/lower-case (str/replace #"\s+" " ")))

#?(:cljs (defonce ^:private logged-dup-page-titles (atom #{})))

#?(:cljs
   (defn- warn-on-duplicate-page-titles!
     "DIAGNOSTIC for the page-duplication bug: when a KB's page list holds >1 page
      with the same normalized title, warn ONCE per (scope,title,uuid-set) with
      the offending uuids. The extra row is almost always an optimistic phantom
      minted after a denied/failed `find-page-by-title` (see
      main/handle-page-reference-click). To confirm next time: check which uuid
      exists on the server vs only in the client overlay. Quiet when there are no
      duplicates, so it costs nothing in the normal case."
     [pages kb-scope-str]
     (doseq [[k group] (group-by #(kb-page-title-key (:title %)) pages)
             :when (> (count group) 1)]
       (let [sig [kb-scope-str k (sort (map :id group))]]
         (when-not (contains? @logged-dup-page-titles sig)
           (swap! logged-dup-page-titles conj sig)
           (js/console.warn
             (str "[dup-page] KB " kb-scope-str " lists " (count group)
                  " pages titled " (pr-str k)
                  " — likely an optimistic phantom from a denied/failed"
                  " find-page-by-title.")
             (clj->js (mapv (juxt :title :id) group))))))))

(defn query-kb-pages
  "Query pages from a KB's live DB value (from the per-KB db signal).
   Returns sorted [{:id str :uuid uuid :type :wiki :title str :db-scope str}]."
  [db kb-scope-str]
  #?(:cljs
     (when db
       (try
         (let [pages (->> (d/q '[:find [(pull ?e [:entity/uuid :S.Page/title :S.Page/kind :S.Page/archived
                                                  {:instance/of-role [:object/icon :object/primitive?]}]) ...]
                                 :where [?e :S.Page/title _]] db)
                          ;; archived + chat-summary records filtered as in query-all-pages
                          (remove #(or (:S.Page/archived %) (= :chat-summary (:S.Page/kind %))))
                          (mapv (fn [{:keys [entity/uuid S.Page/title instance/of-role]}]
                                  ;; A typed page shows its type's icon (first assigned
                                  ;; type that defines :object/icon, e.g. S/Person → users)
                                  ;; instead of the default document glyph.
                                  {:id (str uuid) :uuid uuid :type :wiki
                                   :title (or title "Untitled") :db-scope kb-scope-str
                                   :obj-icon (->> of-role (keep :object/icon) first)})))]
           (warn-on-duplicate-page-titles! pages kb-scope-str)
           (sort-by :title pages))
         ;; NEVER swallow this. A pull over an attribute the replica's schema
         ;; lacks throws, and returning a bare [] made that indistinguishable
         ;; from a KB with no pages — the sidebar read "No pages" for wikis that
         ;; had them, and the missing `:S.Page/kind` behind it went unnoticed
         ;; through a boot and a demo rehearsal.
         (catch :default e
           (js/console.error "[nav] Failed to query KB pages for" kb-scope-str ":" e)
           [])))
     :clj []))

;;  Legacy query-chat-rooms removed — rooms now managed in system DB

;; =============================================================================
;; Item Rendering
;; =============================================================================

(defn type-icon
  "Get the Lucide icon name for a content type."
  [content-type]
  (case content-type
    :wiki "file-text"
    :chat "message-square"
    :video "video"
    :feed "activity"
    :home "home"
    :agent "bot"
    "file"))

(defn nav-item
  "Render a navigation item (page, chat, video, agent, etc.).

   Props:
   - :id - Content ID (string)
   - :uuid - Page UUID (for wiki items)
   - :type - :wiki, :chat, :video, :agent
   - :title - Display title
   - :project - Optional project ID (for multi-project items)
   - :icon - Optional project icon (for starred/recent)
   - :unread - Optional unread count
   - :active? - Whether this item is active
   - :room-id - Room ID (for :agent items)
   - :model - LLM model name (for :agent items)"
  [{:keys [id uuid type title project icon obj-icon unread active? db-scope room-id model
           kind-label]}]
  (el/div {:key (or id (str uuid))
           :class (vc/class-names "nav-item"
                                  (when active? "active"))
           :draggable "true"
           :data-type (name type)
           :data-id id
           :data-uuid (str uuid)
           :data-project project
           :on-click (fn [e]
                       #?(:cljs
                          (let [cmd-key? (or (.-metaKey e) (.-ctrlKey e))
                                tab-data (case type
                                           :wiki  {:page-uuid uuid :db-scope db-scope}
                                           :chat  {:room-id id :room-name title :db-scope db-scope}
                                           :video {:room-id id}
                                           :agent {:agent-id id :room-id room-id
                                                   :agent-name title :model model}
                                           nil)]
                            ;; Nav clicks always open new tabs (not replace current)
                            (sig/open-tab! type tab-data
                                           {:title title
                                            :new-column? cmd-key?
                                            :new-tab? (not cmd-key?)})
                            ;; Opening a chat clears its unread badge (and
                            ;; advances the durable server read cursor).
                            (when (= type :chat) (mns/mark-read! id)))
                          :clj nil))
           :on-dragstart (fn [e]
                           #?(:cljs
                              (do
                                (.setData (.-dataTransfer e) "text/plain" "")
                                (set! (.-effectAllowed (.-dataTransfer e)) "move")
                                ;; Store drag info in a way columns can read
                                (set! (.-dragData js/window)
                                      {:type :nav
                                       :content-type type
                                       :content-id id
                                       :page-uuid uuid
                                       :title title
                                       :project project
                                       :db-scope db-scope}))
                              :clj nil))}
    ;; Project icon (starred/recent) → type-object icon (typed pages) → default.
    (cond
      icon      (el/span {:class "project-icon"} icon)
      obj-icon  (vc/icon obj-icon)
      :else     (vc/icon (type-icon type)))
    (el/span {} title)
    ;; Only where the section heading would be WRONG for this row — a Team under
    ;; a Teams heading needs no label, an assistant does.
    (when kind-label
      (el/span {:class "nav-kind"} kind-label))
    (when unread
      (el/span {:class "badge"} (str unread)))))

;; =============================================================================
;; Section Rendering
;; =============================================================================

(defn contact-item
  "Render a single contact row. Agents open the agent inspector; humans open a contact profile."
  [{:keys [id type display-name handle room-id room-name model] :as _contact}]
  (el/div {:key id
           :class "nav-item nav-item--agent"
           :on-click (fn [e]
                       #?(:cljs
                          (let [cmd-key? (or (.-metaKey e) (.-ctrlKey e))]
                            (if (and (= :agent type) room-id)
                              (sig/open-tab! :agent
                                             {:agent-id id :room-id room-id
                                              :agent-name display-name :model model}
                                             {:title display-name
                                              :new-column? cmd-key?
                                              :new-tab? (not cmd-key?)})
                              (sig/open-tab! :contact
                                             {:contact-id id}
                                             {:title display-name
                                              :new-column? cmd-key?
                                              :new-tab? (not cmd-key?)})))
                          :clj nil))}
    (vc/icon (if (= :agent type) "bot" "user"))
    (el/div {:class "nav-item-agent-text"}
      (el/span {:class "nav-item-agent-name"}
        (or display-name handle "Unknown"))
      (when (or room-name handle)
        (el/span {:class "nav-item-agent-room"}
          (cond (= :agent type) (or room-name "")
                handle          (str "@" handle)))))))

(defn nav-section-header
  "Render a section header with optional collapse toggle."
  [{:keys [title collapsible? collapsed? on-toggle icon]}]
  (el/div {:class (vc/class-names "nav-title"
                                  (when icon "nav-title--project"))
           :on-click (when collapsible? on-toggle)}
    (when icon
      (el/span {:class "project-icon"} icon))
    (when collapsible?
      (vc/icon (if collapsed? "chevron-right" "chevron-down")
               {:class "nav-title-chevron"}))
    title))

(defn project-section
  "Render a collapsible project section."
  [{:keys [id name icon items collapsed?]} collapsed-projects]
  (let [is-collapsed? (contains? collapsed-projects id)]
    (el/div {:key id
             :class "nav-section"}
      (nav-section-header
        {:title name
         :icon icon
         :collapsible? true
         :collapsed? is-collapsed?
         :on-toggle (fn [_]
                      #?(:cljs
                         (binding [rtc/*execution-context* runtime]
                           (swap! sig/nav-collapsed-projects
                                  (fn [s]
                                    (if (contains? s id)
                                      (disj s id)
                                      (conj s id)))))
                         :clj nil))})
      (when-not is-collapsed?
        (el/div {:class "nav-section-items"}
          (ifor-each #(or (:id %) (str (:uuid %))) items
            (fn [item] (nav-item (assoc item :project id)))))))))

;; =============================================================================
;; Sidebar Header (Logo + Search)
;; =============================================================================

(defn logo-mark
  "Simmis SVG logo mark — the colorful S with circles.
   Uses currentColor for the S path so it adapts to dark/light mode.
   Requires Spindel browser.cljs to use createElementNS for SVG elements."
  []
  (el/simple-element :svg
    {:class "nav-logo-svg"
     :viewBox "70.5 44.5 12 12"
     :width "20"
     :height "20"
     :fill "none"
     :xmlns "http://www.w3.org/2000/svg"
     :aria-label "Simmis"
     :role "img"}
    [(el/simple-element :g {:transform "translate(1.8712205,0.00581173)"}
       [(el/simple-element :g {:transform "translate(0.99999996,1)"}
          [(el/simple-element :g {:aria-label "S"}
               [(el/simple-element :path
                  {:d "m 78.699502,52.061635 c 0,0.79648 -0.306153,1.416896 -0.918459,1.861248 -0.612306,0.444352 -1.437588,0.666528 -2.475846,0.666528 -0.53244,0 -1.024947,-0.03773 -1.477521,-0.113184 -0.452574,-0.07546 -0.829719,-0.180256 -1.131435,-0.3144 v -1.081536 c 0.319464,0.134144 0.714357,0.255712 1.184679,0.364704 0.479196,0.108992 0.971703,0.163488 1.477521,0.163488 0.70992,0 1.24236,-0.129952 1.59732,-0.389856 0.363834,-0.259904 0.545751,-0.612032 0.545751,-1.056384 0,-0.293439 -0.06656,-0.540767 -0.199665,-0.741983 -0.13311,-0.201216 -0.363834,-0.385664 -0.692172,-0.553344 -0.319464,-0.176064 -0.767601,-0.360512 -1.344411,-0.553344 0.715983,-0.844409 0.354384,-0.460014 0.838593,-0.842592 0.559062,0.192832 1.029384,0.402432 1.410966,0.6288 0.390456,0.217984 0.683298,0.48208 0.878526,0.792288 0.204102,0.310208 0.306153,0.700064 0.306153,1.169567 z"
                   :fill "currentColor"}
                  [])])
           (el/simple-element :g {:transform "translate(-0.04755337,-0.12681021)"}
              [(el/simple-element :circle {:cx "75.521111" :cy "49.80471" :r "0.85652328" :fill "var(--mono-bg-base, #ffffff)" :stroke "currentColor" :stroke-width "0.35"} [])
               (el/simple-element :circle {:cx "73.450874" :cy "47.613285" :r "0.7" :fill "#00ccff"} [])
               (el/simple-element :circle {:cx "74.084923" :cy "46.29763" :r "0.7" :fill "#00aa00"} [])
               (el/simple-element :circle {:cx "75.432281" :cy "45.72699" :r "0.7" :fill "#ffcc00"} [])
               (el/simple-element :circle {:cx "78.301353" :cy "46.218376" :r "0.7" :fill "#d40000"} [])
               (el/simple-element :circle {:cx "76.878685" :cy "45.826038" :r "0.7" :fill "#ff6600"} [])
               (el/simple-element :circle {:cx "73.926414" :cy "49.008202" :r "0.7" :fill "#0000ff"} [])])])])]))

(defn- fmt-cut [d]
  #?(:cljs (let [pad #(if (< % 10) (str "0" %) (str %))]
             (str (inc (.getMonth d)) "/" (.getDate d) " "
                  (pad (.getHours d)) ":" (pad (.getMinutes d))))
     :clj ""))

(defn- time-strip [gref]
  ;; GlobalCut control under the search box. Now = collapsed dot; a past
  ;; cut colours the strip and offers Back-to-Now. The slider spans the
  ;; last 30 days → as-of that instant (commit-snapping is a refinement).
  #?(:cljs
     (let [as-of (:as-of gref)
           now (js/Date.now)
           span (* 30 24 60 60 1000)
           pos (if as-of
                 (max 0 (min 1000 (js/Math.round (* 1000 (/ (- (.getTime as-of) (- now span)) span)))))
                 1000)]
       (el/div {:class (str "time-strip" (when as-of " time-strip--past"))}
         ;; The strip IS Timelines — titled here rather than repeated as a nav
         ;; item, since a perspective you reach by two routes reads as two
         ;; things. Touching it (click or scrub) opens the view, so the control
         ;; and its canvas are one destination.
         (el/div {:class "time-strip-title"} "Timelines")
         (el/div {:class "time-strip-row time-strip-row--open"
                  :title "Open Timelines — history, now, and proposed futures"
                  :on-click (fn [e]
                              (binding [rtc/*execution-context* runtime]
                                (sig/open-or-activate-tab!
                                  :timelines nil {:title "Timelines"
                                                  :new-column? (or (.-metaKey e)
                                                                   (.-ctrlKey e))})))}
           (el/span {:class "time-strip-dot"} "●")
           (el/span {:class "time-strip-label"}
             (if as-of (str "Viewing " (fmt-cut as-of)) "Now"))
           (when as-of
             (el/button {:class "time-strip-now"
                         ;; the row opens Timelines; this button must not
                         :on-click (fn [e]
                                     (.stopPropagation e)
                                     (binding [rtc/*execution-context* runtime]
                                       (reset! sig/global-ref nil)))}
               "Back to Now"))
           (vc/icon "chevron-right"))
         (el/input {:type "range" :min "0" :max "1000" :value (str pos)
                    :class "time-strip-slider"
                    :on-input (fn [e]
                                (binding [rtc/*execution-context* runtime]
                                  (let [v (js/parseInt (.-value (.-target e)) 10)]
                                    (if (>= v 998)
                                      (reset! sig/global-ref nil)
                                      (reset! sig/global-ref
                                              {:as-of (js/Date. (+ (- now span)
                                                                   (* span (/ v 1000))))}))
                                    ;; Scrubbing here is coarse by construction —
                                    ;; ~200px over 30 days is 3.5 hours per pixel,
                                    ;; so you cannot land on a commit with it. Open
                                    ;; Timelines, where the rail is commit-granular:
                                    ;; the gesture then teaches its own destination
                                    ;; instead of quietly doing something imprecise.
                                    (sig/open-or-activate-tab!
                                      :timelines nil {:title "Timelines"}))))})))
     :clj nil))

(defn render-sidebar-header
  "Render the sidebar header with logo, search, and the global time strip."
  [gref]
  (el/div {:class "nav-header"}
    ;; Logo/branding
    (el/div {:class "nav-logo"}
      (logo-mark)
      (el/span {:class "nav-logo-text"} "imm.is"))
    ;; Search input
    (el/div {:class "nav-search"}
      (vc/icon "search" {:class "nav-search-icon"})
      (el/input {:type "text"
                 :class "nav-search-input"
                 :placeholder "Search..."
                 :value #?(:cljs @sig/nav-search-query :clj "")
                 :on-input (fn [e]
                              #?(:cljs
                                 (binding [rtc/*execution-context* runtime]
                                   (reset! sig/nav-search-query
                                           (str/lower-case (.-value (.-target e)))))
                                 :clj nil))}))
    (time-strip gref)))

;; =============================================================================
;; Sidebar Footer (User, Settings, Status)
;; =============================================================================

(defn user-initials
  "Get initials from a user name or email."
  [user]
  (let [name-str (or (:name user) (:email user) "U")]
    (if-let [parts (seq (clojure.string/split name-str #"\s+"))]
      (apply str (map #(first %) (take 2 parts)))
      (subs name-str 0 1))))

(defn render-sidebar-footer
  "Render the sidebar footer with user menu, settings, and connection status."
  [connection-status current-user]
  (el/div {:class "nav-footer"}
    ;; Connection status indicator
    (el/div {:class (vc/class-names "nav-status"
                                    (case connection-status
                                      :connected "nav-status--connected"
                                      :connecting "nav-status--connecting"
                                      :disconnected "nav-status--disconnected"
                                      nil))}
      (el/span {:class "nav-status-dot"})
      (el/span {:class "nav-status-text"}
        (case connection-status
          :connected "Connected"
          :connecting "Connecting..."
          :disconnected "Disconnected"
          "Unknown")))

    ;; Admin button (only for admins)
    (when (= (:role current-user) "admin")
      (el/button {:class "nav-footer-btn"
                  :title "Admin Dashboard"
                  :on-click (fn [_]
                              #?(:cljs
                                 (sig/open-or-activate-tab! :admin nil {:title "Admin"})
                                 :clj nil))}
        (vc/icon "shield")))

    ;; Settings button
    (el/button {:class "nav-footer-btn"
                :title "Settings"
                :on-click (fn [_]
                            #?(:cljs
                               (sig/open-or-activate-tab! :settings nil {:title "Settings"})
                               :clj nil))}
      (vc/icon "settings"))

    ;; User avatar/menu
    (el/button {:class "nav-footer-btn nav-footer-user"
                :title (or (:name current-user) (:email current-user) "User menu")}
      (el/span {:class "nav-avatar"}
        (user-initials current-user)))))

;; =============================================================================
;; Main Navigation Sidebar
;; =============================================================================

;; The sidebar is decomposed into independent spins so a signal change
;; re-runs only the section that depends on it:
;;
;;   render-nav-sidebar          tracks current-user (the login gate)
;;   ├─ render-sidebar-header-spin    tracks nav-search-query
;;   ├─ render-views-section-spin     tracks active-nav-keys
;;   ├─ render-memory-section-spin    tracks user-rooms + nav-collapsed-projects
;;   │   └─ render-kb-subsection-spin (one spin per KB) tracks that KB's
;;   │                                per-KB db signal + nav-search-query
;;   │                                + nav-collapsed-projects
;;   ├─ render-chats-section-spin     tracks user-rooms + collapse + search
;;   ├─ render-contacts-section-spin  tracks user-rooms + collapse + search
;;   └─ render-sidebar-footer-spin    tracks connection-status + current-user
;;
;; A page added to KB X bumps only X's per-KB db signal, which re-runs
;; ONLY that KB's render-kb-subsection-spin — not the whole sidebar.

(defn- search-matches?
  "True when `title` matches the (lower-cased) search query, or no query."
  [search-q title]
  (or (empty? search-q)
      (str/includes? (str/lower-case (str title)) search-q)))

#?(:cljs
   (defn render-sidebar-header-spin
     "Header spin — re-renders on search query or global time-ref change."
     []
     (spin
       (let [_ (track sig/nav-search-query)
             gref (iv/get-new (track sig/global-ref))]
         (render-sidebar-header gref)))))

#?(:cljs
   (defn render-views-section-spin
     "PERSPECTIVES — the questions that are not about one resource.

      The test for belonging here: does answering it otherwise mean visiting N
      places? \"What needs me\" spans every team; \"what did we spend\" spans every
      book. A wiki's pages never qualified, which is why resource types moved
      into Memory (doc/archive/navigation-redesign.md).

      Naming the group is what lets it GROW without becoming a pile: Compliance
      and whatever else is cross-cutting has an obvious home, and anything
      resource-shaped obviously does not. The honest ceiling is around six —
      past that this wants two tiers, not a longer list.

      Timelines is deliberately ABSENT: it is titled over the strip above and
      opens when you touch it. A perspective reachable by two routes reads as
      two things.

      The tension this leaves — a global Accounting view AND per-team costs —
      does not resolve and should not. The global entry is the index, the
      in-view figure is the instance. What keeps it from being double work is
      that the global view must be assembled from the same per-scope query,
      never a parallel aggregate (the discipline used for Tasks)."
     []
     (spin
       (let [active   (iv/get-new (track sig/active-nav-keys))
             ;; these anchors are singletons — no id to match on, so the tab
             ;; TYPE alone identifies them
             on?      (fn [t] (contains? active [t nil]))
             n-tasks  (count (:tasks @sig/tasks-data))
             item     (fn [k icon label badge]
                        (el/div {:key (name k)
                                 :class (vc/class-names "nav-item nav-item--prominent"
                                                        (when (on? k) "active"))
                                 :on-click (fn [e]
                                             (let [cmd-key? (or (.-metaKey e) (.-ctrlKey e))]
                                               (sig/open-tab! k nil
                                                              {:title label
                                                               :new-column? cmd-key?})))}
                          (vc/icon icon)
                          (el/span {} label)
                          (when badge (el/span {:class "badge"} (str badge)))))]
         (el/div {:class "nav-section"}
           (nav-section-header {:title "Perspectives"})
           (el/div {:class "nav-section-items"}
             ;; what is happening — across the platform, not only my projects
             (item :feed "activity" "Feed" nil)
             ;; what needs ME
             (item :tasks "circle-check" "Tasks" (when (pos? n-tasks) n-tasks))
             ;; what runs WITHOUT me
             (item :schedules "calendar" "Schedules" nil)
             ;; what we owe, own and spent — one book per store, one view
             (item :accounting "wallet" "Accounting" nil)))))))

#?(:cljs
   (defn render-kb-subsection-spin
     "One spin per knowledge base. Tracks this KB's per-DB signal directly
      via `ensure-kb-db-signal!`, plus `nav-search-query` and
      `nav-collapsed-projects` so filtering and expand/collapse stay
      reactive without re-running the rest of the sidebar.

      Adding a page to KB X bumps only X's per-DB signal — and the
      signal-set lives in a non-reactive atom — so this spin owns the
      single reactive dependency on KB X's pages, not the parent
      kbs-section."
     [kb]
     (spin
       (let [kb-scope   (:kb/db-scope kb)
             ;; GlobalCut: the page list resolves through the (scope,ref)
             ;; registry, so at a past cut the sidebar lists the pages that
             ;; existed at T — previously it silently showed Now next to a
             ;; time-traveled page (doc §S3). Track at the spin top; ref=:now
             ;; returns the untouched live signal (hot path unchanged).
             gref       (iv/get-new (track sig/global-ref))
             kb-sig     (db-sig/ensure-view-db-signal! kb-scope (or gref :now))
             live-db    (iv/get-new (track kb-sig))
             collapsed  (iv/get-new (track sig/nav-collapsed-projects))
             search-q   (iv/get-new (track sig/nav-search-query))
             active     (iv/get-new (track sig/active-nav-keys))
             searching? (seq search-q)
             kb-key     (str "kb-" (:kb/id kb))
             kb-id-str  (str (:kb/id kb))
             ;; Page titles are derived only after this KB has been selected.
             ;; The roster intentionally carries metadata, not a snapshot of
             ;; every wiki's contents.
             kb-pages   (when live-db (query-kb-pages live-db kb-scope))
             ;; :active? IN the item — ifor-each diffs on item equality and
             ;; cannot see a closure variable (sharp edge #2).
             ;; Scope-qualified, because a page is (store, uuid) and the uuid
             ;; alone is not unique across KBs — the seed gives every KB's
             ;; "SKILL" page the same id, so this used to highlight all of them
             ;; at once. Must match the key `sig/active-tab-keys` builds.
             kb-pages   (mapv #(assoc % :active? (contains? active
                                                            [:wiki (str (:db-scope %) "/" (:id %))]))
                              (filterv #(search-matches? search-q (:title %))
                                       (or kb-pages [])))
             kb-collapsed? (and (not searching?)
                                (or (nil? live-db)
                                    (contains? collapsed kb-key)))]
         (el/div {:key kb-key :class "nav-subsection"}
           (el/div {:class (vc/class-names "nav-item nav-item--subsection"
                                           (when kb-collapsed? "collapsed"))
                    :on-click (fn [_]
                                (binding [rtc/*execution-context* runtime]
                                  (when-not live-db
                                    (db-sig/connect-kb! kb-scope @web/client))
                                  (swap! sig/nav-collapsed-projects
                                         (fn [s]
                                           (if kb-collapsed?
                                             (disj s kb-key)
                                             (conj s kb-key))))))}
             (vc/icon (if kb-collapsed? "chevron-right" "chevron-down")
                      {:class "nav-title-chevron"})
             (el/span {:style {:flex "1"}} (or (:kb/name kb) "Wiki"))
             ;; Gear icon for KB settings
             (el/button {:class "nav-item-action-btn"
                         :title "KB settings"
                         ;; The whole body needs the ctx: `reset!` on a signal
                         ;; throws without one, and it sits BEFORE `open-tab!`,
                         ;; so the gear did nothing at all — propagation already
                         ;; stopped, tab never opened. Every sibling handler in
                         ;; this file binds; this one did not.
                         :on-click (fn [e]
                                     (.stopPropagation e)
                                     (binding [rtc/*execution-context* runtime]
                                       (reset! sig/admin-data nil)
                                       (sig/open-tab!
                                         :kb-settings
                                         {:kb-id kb-id-str}
                                         {:title (str (:kb/name kb) " Settings")
                                          :new-tab? true})))}
               (vc/icon "settings" {:class "nav-item-action-icon"})))
           ;; WHO can reach it, on its own line — a grant is an access relation,
           ;; not containment, so this wiki is listed once and names its teams
           ;; rather than being repeated under each. Below rather than beside
           ;; the title: the sidebar is narrow and team names truncated to
           ;; "Ops — Maint…", which is worse than no answer.
           (when (seq (:scopes kb))
             (el/div {:class "nav-scope"} (str/join " · " (:scopes kb))))
           (when-not kb-collapsed?
             (el/div {:class "nav-section-items"}
               ;; Branch UI removed deliberately: branches are
               ;; INFRASTRUCTURE, not interface (doc/proposals-and-
               ;; time-travel.md) — fork review lives in the Proposals
               ;; inbox, time travel in the time strip. The branching
               ;; machinery (signals, projection) stays for the
               ;; registry's {:branch} ref arm.
               (if (seq kb-pages)
                 (ifor-each :id kb-pages
                   (fn [page] (nav-item page)))
                 (el/div {:class "nav-empty"} "No pages")))))))))

#?(:cljs
   ;; ONE form per reader-conditional branch — three bare defns here meant only
   ;; the first was emitted and the other two silently did not exist.
   (do
   (defn- kb-scope-index
     "kb-id → the team names that can reach it, from each room's grant list."
     [rooms]
     (reduce (fn [m r]
               (reduce (fn [m kid] (update m (str kid) (fnil conj []) (:room/name r)))
                       m (:room/knowledge-bases r)))
             {} rooms))

   (defn- drive-rows
     "One row per DRIVE, not per attachment. The roster carries a drive once per
      room it is granted to (chat-remote fans out over rooms), so the same drive
      arrives several times; collapse those into a single row whose scopes list
      every team."
     [drives]
     (->> (group-by :id drives)
          (mapv (fn [[id ds]]
                  (let [d (first ds)]
                    {:id id :kind :drive :title (:name d)
                     :room-id (some :room-id ds)
                     :scopes (vec (distinct (keep :room-name ds)))})))))

   (defn render-memory-section-spin
     "ONE list of everything the workspace remembers — wikis and drives together,
      mail later.

      Not grouped by resource TYPE, because that is the distinction
      `doc/kb-unification.md` exists to erase: wiki, drive, mail and books are
      converging on one substrate, so heading the sidebar with their storage
      kinds advertises an implementation detail as the primary structure.

      Not nested under teams either. A grant is an ACCESS relation, not
      containment — a wiki granted to four teams is one thing four teams can
      reach, not four things. Nesting would list it four times, own it nowhere,
      and (not coincidentally) put four spins on one per-KB signal. Scope is
      therefore a COLUMN: each resource appears once and says who can see it.

      Each KB keeps its own nested spin, so a page added to one wiki re-runs only
      that wiki's subtree."
     []
     (spin
       (let [user-rooms (iv/get-new (track sig/user-rooms))
             collapsed  (iv/get-new (track sig/nav-collapsed-projects))
             active     (iv/get-new (track sig/active-nav-keys))
             ;; mail + web are singletons — the tab TYPE alone identifies them
             on?        (fn [t] (contains? active [t nil]))
             kbs-data   (when (map? user-rooms) (:knowledge-bases user-rooms))
             rooms      (when (map? user-rooms) (:rooms user-rooms))
             drives     (when (map? user-rooms) (:drives user-rooms))
             scope-idx  (kb-scope-index rooms)
             ;; scopes ride IN the item — ifor-each diffs on item equality and
             ;; cannot see a closure variable
             kb-items   (->> kbs-data
                             (mapv #(assoc % :scopes (get scope-idx (str (:kb/id %)) [])))
                             (sort-by #(str (:kb/name %))))
             drv-items  (sort-by :title (drive-rows drives))]
         (el/div {:class "nav-section"}
           (nav-section-header
             {:title "Memory"
              :collapsible? true
              :collapsed? (contains? collapsed "kbs")
              :on-toggle (fn [_]
                           (binding [rtc/*execution-context* runtime]
                             (swap! sig/nav-collapsed-projects
                                    (fn [s]
                                      (if (contains? s "kbs")
                                        (disj s "kbs")
                                        (conj s "kbs"))))))})
           (when-not (contains? collapsed "kbs")
             (el/div {:class "nav-section-items"}
               (when (seq kb-items)
                 ;; render-fn returns spins → ifor-each returns a spin → await it
                 (await (ifor-each :kb/id kb-items
                          (fn [kb] (render-kb-subsection-spin kb)))))
               (when (seq drv-items)
                 (ifor-each :id drv-items
                   (fn [drv]
                     (el/div {:key (:id drv) :class "nav-subsection"}
                       (if (:room-id drv)
                         (el/div {:class "nav-item"
                                  :on-click (fn [_]
                                              (binding [rtc/*execution-context* runtime]
                                                (sig/open-or-activate-tab!
                                                  :files
                                                  {:room-id (:room-id drv)}
                                                  {:title (:title drv)})))}
                           (vc/icon "folder")
                           (el/span {:class "nav-item-label"} (:title drv)))
                         ;; no grant yet — visible so it can be found, not
                         ;; browsable until a team can reach it
                         (el/div {:class "nav-item nav-item--muted"
                                  :title "Attach to a team (Room Settings) to browse"}
                           (vc/icon "folder")
                           (el/span {:class "nav-item-label"} (:title drv))))
                       (el/div {:class "nav-scope"}
                               (if (seq (:scopes drv))
                                 (str/join " · " (:scopes drv))
                                 "no team"))))))
               ;; Mail and web captures are MEMORY, not top-level destinations.
               ;; They were sitting in the views group beside Tasks, which is
               ;; the resource-type axis doc/kb-unification.md exists to erase —
               ;; a mail account and a wiki are two shapes of the same
               ;; substrate, so they belong in one list.
               ;;
               ;; Both are singletons per user rather than things you create:
               ;; web captures already live in ONE per-party store
               ;; (`runtimes.web-intake`), and mail opens onto the account list.
               (el/div {:key "mail" :class "nav-subsection"}
                 (el/div {:class (vc/class-names "nav-item" (when (on? :mail) "active"))
                          :on-click (fn [_]
                                      (binding [rtc/*execution-context* runtime]
                                        (sig/open-or-activate-tab!
                                          :mail nil {:title "Mail"})))}
                   (vc/icon "mail")
                   (el/span {:class "nav-item-label"} "Mail"))
                 (el/div {:class "nav-scope"} "everywhere"))
               (el/div {:key "web-captures" :class "nav-subsection"}
                 (el/div {:class (vc/class-names "nav-item" (when (on? :web-captures) "active"))
                          :on-click (fn [_]
                                      (binding [rtc/*execution-context* runtime]
                                        (sig/open-or-activate-tab!
                                          :web-captures nil {:title "Web"})))}
                   (vc/icon "globe")
                   (el/span {:class "nav-item-label"} "Web"))
                 (el/div {:class "nav-scope"} "yours only"))
               (when-not (or (seq kb-items) (seq drv-items))
                 (el/div {:class "nav-empty"} "Nothing yet"))
               ;; ONE entry point. Two type-named buttons ("New Wiki", "New
               ;; Drive") put the resource-type axis back in through the
               ;; actions right after we took it out of the headings — and they
               ;; could never cover CONNECTING an existing platform, which is
               ;; not a "new" anything.
               ;;
               ;; "Add" is the only verb honest for both: you cannot create a
               ;; Gmail account here and you cannot connect a blank wiki.
               ;; Connecting sets an UPSTREAM on a local memory rather than
               ;; pointing at a remote one — the same shape dvergr's geschichte
               ;; substrate uses for repos — so the replica stays forkable,
               ;; searchable and time-travellable like everything else.
               (el/div {:key "add-memory"
                        :class "nav-item nav-item--action"
                        :on-click (fn [_]
                                    (binding [rtc/*execution-context* runtime]
                                      (sig/open-or-activate-tab!
                                        :add-memory nil {:title "Add memory"})))}
                 (vc/icon "plus")
                 (el/span {} "Add memory"))))))))))


#?(:cljs
   (defn render-chats-section-spin
     "Chats section spin. Tracks the room roster, collapse state and search
      query — each room item fully encodes what nav-item needs, so the
      inner ifor-each render-fn returns plain vnodes (no nested spin)."
     []
     (spin
       ;; ORDER MATTERS among these tracks, not just position-in-body. A resume
       ;; re-executes FROM THE TRACK POINT, so a signal tracked LAST resumes
       ;; after the earlier bindings and leaves them stale. `layout-columns`
       ;; changes on every tab switch — tracked last it re-ran this spin with a
       ;; stale `user-rooms` and the sidebar rendered "No chat rooms" while the
       ;; signal held three. The most frequently changing signal goes FIRST so
       ;; everything downstream is re-read.
       (let [active     (iv/get-new (track sig/active-nav-keys))
             user-rooms (iv/get-new (track sig/user-rooms))
             collapsed  (iv/get-new (track sig/nav-collapsed-projects))
             search-q   (iv/get-new (track sig/nav-search-query))
             unread-map (iv/get-new (track sig/unread-counts))
             rooms-data (when (map? user-rooms) (:rooms user-rooms))
             rooms-data (or rooms-data (when (vector? user-rooms) user-rooms))
             all-rooms  (when rooms-data
                          (->> rooms-data
                               (mapv (fn [r]
                                       (let [rid (str (:room/id r))
                                             u   (get unread-map rid 0)]
                                         ;; :unread and :active? live IN the item so
                                         ;; ifor-each's item-equality diff re-renders
                                         ;; them — a closure variable is invisible
                                         ;; to that diff (sharp edge #2).
                                         {:id rid
                                          :uuid (:room/id r)
                                          :type :chat
                                          :title (or (:room/name r) "Chat")
                                          :db-scope (:room/content-db-scope r)
                                          :active? (contains? active [:chat rid])
                                          ;; IN the item, not read from a closure —
                                          ;; ifor-each diffs on item equality.
                                          ;; The noun explains why a non-Team row
                                          ;; sits under a "Teams" heading — so it
                                          ;; is redundant once the room's own NAME
                                          ;; already says it ("Christian's
                                          ;; Assistants" needs no ASSISTANT chip).
                                          ;; Suppressed by what the name contains
                                          ;; rather than by room type, so a
                                          ;; telegram room someone calls "Telegram
                                          ;; bridge" drops its chip too.
                                          :kind-label (let [noun (when-not (vc/team-like? (:room/type r))
                                                                   (vc/room-noun (:room/type r)))]
                                                        (when (and noun
                                                                   (not (str/includes?
                                                                         (str/lower-case (or (:room/name r) ""))
                                                                         (str/lower-case noun))))
                                                          noun))
                                          :unread (when (pos? u) u)})))
                               (filterv #(search-matches? search-q (:title %)))))]
         (el/div {:class "nav-section"}
           (nav-section-header
             ;; "Teams" over "Chats": the row is a group of people and agents
             ;; with shared memory, not a message log. Rows whose type makes that
             ;; heading wrong carry their own noun (`:kind-label`).
             {:title "Teams"
              :collapsible? true
              :collapsed? (contains? collapsed "chats")
              :on-toggle (fn [_]
                           (binding [rtc/*execution-context* runtime]
                             (swap! sig/nav-collapsed-projects
                                    (fn [s]
                                      (if (contains? s "chats")
                                        (disj s "chats")
                                        (conj s "chats"))))))})
           (when-not (contains? collapsed "chats")
             (el/div {:class "nav-section-items"}
               (if (seq all-rooms)
                 (ifor-each :id all-rooms
                   (fn [room] (nav-item room)))
                 (el/div {:class "nav-empty"} "No teams yet"))
               (el/div {:key "new-room"
                        :class "nav-item nav-item--action"
                        :on-click (fn [_]
                                    ;; Same reasoning as "Add memory": a team can
                                    ;; also ARRIVE — dvergr's telegram channel
                                    ;; already creates one when its bot joins a
                                    ;; group — so the entry point has to carry
                                    ;; connect as well as create.
                                    (sig/open-or-activate-tab! :add-team nil {:title "Add team"}))}
                 (vc/icon "plus")
                 (el/span {} "Add team")))))))))

#?(:cljs
   (defn render-contacts-section-spin
     "Contacts section spin. Tracks the contact roster, collapse state and
      search query. Every party you can reach — humans + agents, explicit
      + derived."
     []
     (spin
       (let [user-rooms    (iv/get-new (track sig/user-rooms))
             collapsed     (iv/get-new (track sig/nav-collapsed-projects))
             search-q      (iv/get-new (track sig/nav-search-query))
             contacts-data (when (map? user-rooms) (:contacts user-rooms))
             contacts-data (when (seq contacts-data)
                             (filterv #(search-matches? search-q (:display-name %))
                                      contacts-data))]
         (el/div {:class "nav-section"}
           (nav-section-header
             {:title "Contacts"
              :collapsible? true
              :collapsed? (contains? collapsed "contacts")
              :on-toggle (fn [_]
                           (binding [rtc/*execution-context* runtime]
                             (swap! sig/nav-collapsed-projects
                                    (fn [s]
                                      (if (contains? s "contacts")
                                        (disj s "contacts")
                                        (conj s "contacts"))))))})
           (when-not (contains? collapsed "contacts")
             (el/div {:class "nav-section-items"}
               (if (seq contacts-data)
                 (ifor-each :id contacts-data
                   (fn [c] (contact-item c)))
                 (el/div {:class "nav-empty"} "No contacts yet"))
               (el/div {:key "new-contact"
                        :class "nav-item nav-item--action"
                        :on-click (fn [_]
                                    (sig/open-or-activate-tab! :new-contact nil
                                                               {:title "New Contact"}))}
                 (vc/icon "plus")
                 (el/span {} "New Contact")))))))))

#?(:cljs
   (defn render-sidebar-footer-spin
     "Footer spin — tracks connection-status and current-user."
     []
     (spin
       (let [connection-status (iv/get-new (track sig/connection-status))
             current-user      (iv/get-new (track sig/current-user))]
         (render-sidebar-footer connection-status current-user)))))

(defn render-nav-sidebar
  "Render the full navigation sidebar as a tree of independent spins.

   Chromeless design: all navigation, search, user controls and status
   live in this sidebar — no separate header or status bar.

   This is the spindel reactive showcase: the shell spin tracks only
   current-user (the login gate). Every section is its own spin owning
   its reactive scope (see the section-spin docstrings above), so a
   signal change re-runs the minimal subtree — and a page added to a KB
   re-runs only that one KB's spin."
  []
  #?(:cljs
     (spin
       (let [current-user (iv/get-new (track sig/current-user))]
         ;; First-time roster load + push subscription. The dirty-bit
         ;; subscription keeps sig/user-rooms fresh after server-side
         ;; roster changes (KB create/share, contact add, room
         ;; membership). See user_rooms_sync.cljs.
         (when (some? current-user)
           (when (nil? @sig/user-rooms)
             (urs/refresh-user-rooms! (:id current-user)))
           (urs/subscribe! (:id current-user))
           ;; Subscribe to my private @mention notification stream. Idempotent.
           (mns/subscribe! (:id current-user))
           ;; Seed unread badges + per-room notification levels. Idempotent.
           (mns/load-unread!)
           (mns/load-notify-prefs!)
           ;; Subscribe to KB branching events (single global topic, client
           ;; filters by KB-access). Idempotent.
           (br-sync/subscribe!)
           ;; register the aggregate invalidations on the SAME subscription
           (psync/install!))
         (el/aside {:class "nav-sidebar"}
           ;; Header: Logo + Search
           (await (render-sidebar-header-spin))
           ;; Scrollable content area
           (el/div {:class "nav-content"}
             (await (render-views-section-spin))
             (await (render-chats-section-spin))
             (await (render-memory-section-spin))
             (await (render-contacts-section-spin)))
           ;; Footer: User, Settings, Status
           (await (render-sidebar-footer-spin)))))
     :clj
     ;; Server-side: static shell only. The sidebar is client-only; this
     ;; branch just keeps the .cljc namespace JVM-compilable.
     (el/aside {:class "nav-sidebar"}
       (el/div {:class "nav-content"}))))
