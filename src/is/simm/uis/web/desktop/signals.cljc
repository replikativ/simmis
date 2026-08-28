(ns is.simm.uis.web.desktop.signals
  "Application signals for the spindel reactive UI.

   Signals are the sources of truth for UI state. They can be tracked
   by tasks for reactive updates.

   Architecture:
   - All signals are created at the top level with explicit runtime
   - Database signals are in db-signal.cljc (for Datahike integration)
   - Tasks use `track` effect to read signals reactively
   - When signals change, tasks that track them are re-executed

   Usage:
     ;; In a spin, track signals reactively:
     (spin
       (let [cols-iv (track layout-columns)
             cols (iv/get-new cols-iv)]
         ...))

     ;; Update signals directly:
     (swap! nav-collapsed-projects conj project-id)"
  (:require [is.simm.uis.web.desktop.db-signal :as db]
            #?(:cljs [is.simm.uis.web.desktop.datahike-query :as dq])
            #?(:cljs [org.replikativ.spindel.signal :refer [->SignalRef]])
            #?(:cljs [org.replikativ.spindel.engine.core :as rtc])
            #?(:cljs [is.simm.uis.web.desktop.runtime :refer [runtime]]))
  #?(:cljs (:require-macros [org.replikativ.spindel.signal :refer [signal]])))

;; =============================================================================
;; Database Signals (from db-signal.cljc)
;; =============================================================================

;; Re-export the local database signal.
;; This holds the actual Datahike DB value, updated via konserve-sync callback.
(def local-db db/local-db)

;; Re-export the DB summary signal.
;; Contains {:max-tx N :timestamp T} - useful for change detection.
(def local-db-signal db/local-db-signal)

;; =============================================================================
;; Current User Signal
;; =============================================================================

#?(:cljs (def current-user
           "Signal holding the authenticated user info.
            Shape: {:id string :email string :name string :role string} or nil."
           (signal runtime nil)))

;; =============================================================================
;; Settings & Admin Data Signals
;; =============================================================================

#?(:cljs (def settings-data
           "Signal holding loaded settings data.
            Shape: {:profile {...} :env-vars [...] :budget {...} :ui-prefs {:ui-pref/syntax :clojure}} or nil."
           (signal runtime nil)))

#?(:cljs (def syntax-pref
           "Signal for preferred code block syntax view: :clojure or :superficie.
            Initialised from :ui-prefs in settings-data after login.
            Kept separate so toggling it only re-renders the wiki editor, not the whole app."
           (signal runtime :clojure)))

#?(:cljs (def commit-graph-data
           "Signal: scope-str → yggdrasil commit-graph
            {:nodes {id {:parent-ids :meta{:timestamp :branch}}} :branches :roots}.
            Loaded lazily by the history subway."
           (signal runtime nil)))

#?(:cljs (def drive-data
           "Signal: room-id-str → {:drive {...} :tree [...]} for the
            files panel (loaded via load-room-drive!)."
           (signal runtime {})))

#?(:cljs (def files-upload-status
           "Signal: room-id-str → \"3/7\" while a folder import runs."
           (signal runtime {})))

#?(:cljs (def admin-data
           "Signal holding loaded admin dashboard data.
            Shape: {:users [...] :stats {...}} or nil."
           (signal runtime nil)))

#?(:cljs (def screens-results
           "Signal: screens-gallery data per room —
            {<room-id> {:query .. :items [{:at :blob-id :text :score}] :loading? bool}}."
           (signal runtime {})))

#?(:cljs (def recordings-results
           "Signal: the caller's OWN screen recordings (owner-scoped, not per
            room) — {:sessions [{:session :started-at :mime :segments [...]}]
            :loading? bool}."
           (signal runtime {})))

#?(:cljs (def web-captures-results
           "Signal: the caller's OWN captured web pages (owner-scoped) —
            {:items [{:id :url :title :host :at :text :blob-id}] :query :loading?
             :host <active host filter or nil>}."
           (signal runtime {})))

#?(:cljs (def mail-data
           "Signal for the self-tracking mail browser. IMAP remains the source
            of truth; this is only UI selection and remote snapshot state."
           (signal runtime {:accounts nil
                            :account-id nil
                            :folders []
                            :folder "INBOX"
                            :messages []
                            :message nil
                            :query ""
                            :loading? false
                            :error nil})))

#?(:cljs (def unread-counts
           "Signal: per-room unread message counts — {<room-id-str> <count>}.
            Session-local: bumped by the notify stream (message-notify-sync) when
            a message lands in a room you are not viewing, cleared when you open
            it. Durable read cursors are a follow-up (mentions-notifications
            design). Rendered as badges in the nav room list."
           (signal runtime {})))

#?(:cljs
   (defn bump-room-unread!
     "Increment the unread badge for `room-id-str` (a new message arrived)."
     [room-id-str]
     (when room-id-str
       (binding [rtc/*execution-context* runtime]
         (swap! unread-counts update (str room-id-str) (fnil inc 0))))))


;; Defined below, next to `active-tab-keys` (its only dependency); every
;; mutation that moves the highlight calls it, and several of those are
;; defined above it.
#?(:cljs (declare refresh-active-nav-keys!))

#?(:cljs
   (defn mark-room-read!
     "Clear the unread badge for `room-id-str` (you are now viewing it)."
     [room-id-str]
     (when room-id-str
       (binding [rtc/*execution-context* runtime]
         (when (pos? (get @unread-counts (str room-id-str) 0))
           (swap! unread-counts assoc (str room-id-str) 0))))))

#?(:cljs (def notify-prefs
           "Signal: per-room notification level — {<room-id-str> :all|:mentions|:none}.
            Explicit prefs only; unset rooms default :mentions. Seeded on login,
            updated by the room-settings Notifications control (message-notify-sync)."
           (signal runtime {})))

#?(:cljs (def screen-sharing
           "Signal: set of room-id strings with an active local screen
            share (screen-share/start! capture loop feeding agents)."
           (signal runtime #{})))

#?(:cljs (def video-call-info
           "Signal holding minted jitsi meeting credentials, keyed by room:
            {<room-id-str> {:url .. :room .. :jwt ..}}. Filled by
            columns/load-video-token-into-signal!."
           (signal runtime {})))

#?(:cljs (def room-app-status
           "Signal holding each room's static-site status, keyed by room:
            {<room-id-str> {:slug .. :has-app? bool}}. Filled by
            columns/load-room-app-status-into-signal!; drives the chat
            header's Open-app button (opens /apps/<slug>/)."
           (signal runtime {})))

#?(:cljs (def schedules-data
           "Signal for the schedules agenda: {:schedules [...] :loaded-at ms}
            or nil (not yet loaded — the view triggers a load)."
           (signal runtime nil)))

#?(:cljs (def proposals-data
           "Signal for the proposals inbox: {:proposals [...] :loaded-at ms}
            or nil (not yet loaded). Bumped by the :branching/event sync
            (proposal/filed|resolved) and by view-triggered reload."
           (signal runtime nil)))

#?(:cljs (def feed-data
           "Signal for the Feed: {:items [...] :loaded-at ms} | {:error str}
            or nil (not yet loaded). Heterogeneous rows — system announcements
            and room activity — unified server-side by is.simm.ops.feed."
           (signal runtime nil)))

#?(:cljs (def accounting-data
           "Signal for the Accounting perspective:
            {:books [{:room :room-name :accounts [...] :error?}] :loaded-at ms}
            or nil (not yet loaded). One entry per book the party can read —
            there is no single ledger, so this is an index over per-team books."
           (signal runtime nil)))

#?(:cljs (def tasks-data
           "Signal for the Tasks aggregate:
            {:tasks [...] :include-done? bool :loaded-at ms} | {:error str …}
            or nil (not yet loaded — the view triggers a load).

            The rows are heterogeneous by design (wiki pages, landable ForkSets,
            dvergr dispatches — see is.simm.ops.tasks); the server unifies them,
            so this signal holds one flat list rather than a source-keyed map."
           (signal runtime nil)))

#?(:cljs (def global-ref
           "The workspace-wide time reference (doc/proposals-and-time-travel.md
            §1: a GlobalCut). nil = Now (live). {:as-of #inst} = a reader-side
            cut — every view resolves its db to as-of(T) and renders read-only.
            (Per-column pins + proposal refs layer on top later.)"
           (signal runtime nil)))

#?(:cljs (def context-footers
           "Signal for per-column context-footer UI state.
            Shape: {col-id {:collapsed? bool :height px}}.
            Absent col-id means the default: collapsed."
           (signal runtime {})))

(defn toggle-context-footer!
  "Toggle a column's context footer between collapsed (the default) and
   expanded."
  [col-id]
  #?(:cljs
     (binding [rtc/*execution-context* runtime]
       (swap! context-footers update-in [col-id :collapsed?]
              (fn [collapsed?] (not (if (nil? collapsed?) true collapsed?)))))
     :clj nil))

(defn set-context-footer-height!
  "Persist a column's context footer height (px) so drag-resize survives
   re-renders."
  [col-id height]
  #?(:cljs
     (binding [rtc/*execution-context* runtime]
       (swap! context-footers assoc-in [col-id :height] height))
     :clj nil))

#?(:cljs (def user-rooms
           "Signal holding the current user's rooms from system DB.
            Shape: {:rooms [...] :agents [...] :knowledge-bases [...]} or nil."
           (signal runtime nil)))

#?(:cljs (def nav-search-query
           "Signal holding the current nav search query string."
           (signal runtime "")))

;; =============================================================================
;; Connection Status Signal
;; =============================================================================

#?(:cljs (def connection-status
           "Signal holding server connection status.

            Values:
            - :disconnected - Not connected to server
            - :connecting   - Connection attempt in progress
            - :connected    - Successfully connected to server"
           (signal runtime :disconnected)))

#?(:cljs (def nav-collapsed-projects
           "Signal holding set of collapsed project IDs in nav sidebar."
           (signal runtime #{})))

;; =============================================================================
;; Chat Signals
;; =============================================================================

#?(:cljs (def agent-responding?
           "Signal indicating whether the Vár agent is processing a response.
            Used to show 'Thinking...' indicator in the chat."
           (signal runtime false)))

#?(:cljs (def room-runs
           "Reactive Run projection keyed by Simmis room UUID string.

            Shape: {room-id {:active [run ...] :recent [run ...]}}. Dvergr is
            the durable/live authority; this is only the UI projection and is
            deliberately owned by the Spindel execution context so UI forks do
            not share an ambient mutable Run registry."
           (signal runtime {})))

#?(:cljs (def chat-scroll-windows
           "Signal holding scroll window state per room.

            Shape: {room-uuid {:start N :end M}}

            Window indices are message positions in the sorted message list.
            :end is exclusive (like subvec).

            Default window shows last 30 messages."
           (signal runtime {})))

#?(:cljs (def chat-reply-targets
           "Signal holding the active reply target per room UUID.

            Keys are room UUIDs for room composers and `[room-uuid root-uuid]`
            for focused thread composers, so side-by-side projections remain
            independent. Values are bounded UI projections (`:id`, `:thread-root-id`,
            `:author-name`, `:content`), not message state. The durable
            relationship is written as the outgoing message's canonical parent
            edge; Dvergr validates/materializes its root at the Room boundary."
           (signal runtime {})))

(defn set-chat-reply-target!
  [context-key target]
  #?(:cljs
     (binding [rtc/*execution-context* runtime]
       (swap! chat-reply-targets assoc context-key target))
     :clj nil))

(defn clear-chat-reply-target!
  [context-key]
  #?(:cljs
     (binding [rtc/*execution-context* runtime]
       (swap! chat-reply-targets dissoc context-key))
     :clj nil))

;; =============================================================================
;; KB branching signals
;; =============================================================================
;; Backed by yggdrasil + the server-side branching ns (slice α). Branches
;; replicate via the existing kabel store sync; events flow via the
;; kabel.pubsub topic :branching/event consumed by branching_sync.cljs.
;; See doc/archive/branching-systematic-design.md.

#?(:cljs (def active-kb-branch
           "Per-KB active branch override. Map of
              kb-id-str (db-scope as string) → branch-keyword
            Missing entries default to `:db` (trunk). Switching the
            active branch is a local UI affordance; the server doesn't
            track this per-user state."
           (signal runtime {})))

#?(:cljs (def kb-branches
           "Per-KB known-branches registry. Map of
              kb-id-str → {branch-kw {:parent  branch-kw
                                       :author  party-keyword|nil
                                       :created-at inst|nil
                                       :last-tx-at inst|nil
                                       :status :open|:merged|:discarded}}
            Populated initially by `list-kb-branches!` on KB open and
            updated reactively by the kabel.pubsub :branching/event
            subscription."
           (signal runtime {})))

#?(:cljs (def projected-branch-dbs
           "Map of [kb-id-str branch-kw] → db-value, cached projections of
            the local replicated conn at the named branch's HEAD. CLJS
            `dh/branch-as-db` is async (returns a channel), so the editor
            can't project synchronously inside its spin. Instead a small
            go-block runs the projection and `swap!`s the result here;
            the editor tracks this signal and renders when the projection
            lands. Eventually consistent — same SI semantics as trunk's
            db-signal."
           (signal runtime {})))

;; =============================================================================
;; Status Bar Signal
;; =============================================================================

#?(:cljs (def status
           "Signal holding status bar state.

            State shape:
            {:visible?   boolean   ;; Whether status bar is shown
             :type       keyword   ;; :idle, :loading, :success, :error
             :message    string    ;; Main message text
             :details    string    ;; Optional detailed error text
             :error-type keyword   ;; :network, :validation, :server, :not-found, :permission, :unknown
             :expanded?  boolean   ;; Whether details are expanded
             :progress   number}   ;; Optional progress 0-100"
           (signal runtime {:visible? false
                            :type :idle
                            :message ""
                            :details nil
                            :error-type nil
                            :expanded? false
                            :progress nil})))

;; =============================================================================
;; Type/Property Functions
;; =============================================================================

;; =============================================================================
;; Block Functions
;; =============================================================================

;; =============================================================================
;; Modal Functions
;; =============================================================================

;; =============================================================================
;; Status Bar Functions
;; =============================================================================

(defn show-loading!
  "Show loading status with message."
  ([message] (show-loading! message nil))
  ([message progress]
   #?(:cljs
      (binding [rtc/*execution-context* runtime]
        (reset! status {:visible? true
                        :type :loading
                        :message message
                        :details nil
                        :error-type nil
                        :expanded? false
                        :progress progress}))
      :clj nil)))

(defn update-progress!
  "Update progress for current loading status."
  [progress]
  #?(:cljs
     (binding [rtc/*execution-context* runtime]
       (swap! status assoc :progress progress))
     :clj nil))

(defn show-success!
  "Show success status with message. Auto-dismisses after 3 seconds."
  [message]
  #?(:cljs
     (binding [rtc/*execution-context* runtime]
       (reset! status {:visible? true
                       :type :success
                       :message message
                       :details nil
                       :error-type nil
                       :expanded? false
                       :progress nil})
       ;; Auto-dismiss after 3 seconds
       (js/setTimeout #(binding [rtc/*execution-context* runtime]
                         (swap! status assoc :visible? false)) 3000))
     :clj nil))

(defn show-error!
  "Show error status with message and optional details."
  ([message] (show-error! message nil nil))
  ([message details] (show-error! message details nil))
  ([message details error-type]
   #?(:cljs
      (binding [rtc/*execution-context* runtime]
        (reset! status {:visible? true
                        :type :error
                        :message message
                        :details details
                        :error-type (or error-type :unknown)
                        :expanded? false
                        :progress nil}))
      :clj nil)))

(defn hide-status!
  "Hide the status bar."
  []
  #?(:cljs
     (binding [rtc/*execution-context* runtime]
       (swap! status assoc :visible? false))
     :clj nil))

(defn toggle-status-expanded!
  "Toggle expanded state of status details."
  []
  #?(:cljs
     (binding [rtc/*execution-context* runtime]
       (swap! status update :expanded? not))
     :clj nil))

;; =============================================================================
;; Connection Status Functions
;; =============================================================================

(defn set-connection-status!
  "Set the connection status signal.
   Called by main.cljs connect-with-retry! function."
  [status-kw]
  #?(:cljs
     (binding [rtc/*execution-context* runtime]
       (reset! connection-status status-kw))
     :clj nil))

(defn connected?
  "Check if currently connected to server."
  []
  #?(:cljs
     (binding [rtc/*execution-context* runtime]
       (= @connection-status :connected))
     :clj false))

;; =============================================================================
;; Chat Functions
;; =============================================================================

(def ^:const CHAT_WINDOW_SIZE
  "Default number of timeline ROWS in the virtual scroll window.

   A run of tool calls is ONE row (a dot strip), so this counts what the
   reader actually sees — not how many calls the agent happened to make."
  30)

;; RAF throttle state for scroll (plain atom, not a signal)
#?(:cljs (defonce scroll-raf-pending (atom false)))

(defn get-chat-window
  "Get the scroll window for a room. Returns {:start N :end M}.
   If no window exists, returns nil (caller should create default)."
  [room-uuid]
  #?(:cljs
     (binding [rtc/*execution-context* runtime]
       (get @chat-scroll-windows room-uuid))
     :clj nil))

(defn set-chat-window!
  "Set the scroll window for a room."
  [room-uuid window]
  #?(:cljs
     (binding [rtc/*execution-context* runtime]
       (swap! chat-scroll-windows assoc room-uuid window))
     :clj nil))

(defn init-chat-window-at-end!
  "Initialize chat window to show the last CHAT_WINDOW_SIZE messages.
   Called when opening a chat room."
  [room-uuid total-messages]
  #?(:cljs
     (binding [rtc/*execution-context* runtime]
       (let [end total-messages
             start (max 0 (- end CHAT_WINDOW_SIZE))]
         (swap! chat-scroll-windows assoc room-uuid {:start start :end end})))
     :clj nil))

(defn grow-chat-window-up!
  "Extend the chat window upward by `step` items (scroll-back paging).
   Only :start is stored — the tail is ALWAYS live (render computes
   :end from the current total), so incoming messages are never hidden
   behind a frozen window end. (The previous fixed {:start :end} design
   froze the tail: messages arriving after a grow rendered beyond :end
   and the room looked dead while telegram kept working.)"
  [room-uuid total-messages step]
  #?(:cljs
     (binding [rtc/*execution-context* runtime]
       (swap! chat-scroll-windows
              (fn [windows]
                (let [stored (get windows room-uuid)
                      start (if (map? stored)
                              (:start stored (max 0 (- total-messages CHAT_WINDOW_SIZE)))
                              (max 0 (- total-messages CHAT_WINDOW_SIZE)))]
                  (assoc windows room-uuid {:start (max 0 (- start step))})))))
     :clj nil))

(defn follow-chat-end!
  "Shrink a room's chat window back to the default tail view (drops
   paged-in history). Called when the user scrolls back to the bottom."
  [room-uuid]
  #?(:cljs
     (binding [rtc/*execution-context* runtime]
       (swap! chat-scroll-windows assoc room-uuid :end))
     :clj nil))

(defn scroll-chat-window!
  "Scroll the chat window by delta items. Negative = scroll up, positive = scroll down."
  [room-uuid delta total-messages]
  #?(:cljs
     (binding [rtc/*execution-context* runtime]
       (swap! chat-scroll-windows
              (fn [windows]
                (let [stored (get windows room-uuid)
                      ;; Handle :end sentinel or missing window
                      {:keys [start end]} (if (or (nil? stored) (= stored :end))
                                            {:start (max 0 (- total-messages CHAT_WINDOW_SIZE))
                                             :end total-messages}
                                            stored)
                      window-size (- end start)
                      new-start (-> (+ start delta)
                                    (max 0)
                                    (min (max 0 (- total-messages window-size))))
                      new-end (min (+ new-start window-size) total-messages)]
                  (assoc windows room-uuid {:start new-start :end new-end})))))
     :clj nil))

;; (Old fork action functions — set-current-fork!, create-thread!,
;; create-exploration!, switch-fork!, merge-exploration!,
;; discard-exploration!, update-exploration-diff! — removed; they were
;; half-implemented scaffolding from the prior fork-tree design. The
;; new branching API will land in is.simm.ops.branch-ops per Phase 3.)

;; =============================================================================
;; Column Layout Signals
;; =============================================================================

;; Generate initial IDs at load time (these will be stable for the session)
(def ^:private initial-col-id (str (random-uuid)))
(def ^:private initial-tab-id (str (random-uuid)))

#?(:cljs (def layout-columns
           "Signal holding the column layout state.

            State shape:
            [{:id       string     ;; Unique column ID
              :width    number     ;; Width as fraction (0-1)
              :tabs     [{:id      string    ;; Unique tab ID
                          :type    keyword   ;; :home, :wiki, :chat, :chat-thread, :run-history, :run-inspector, :video
                          :title   string    ;; Display title
                          :data    map}]     ;; Type-specific data (page-uuid, room-id, etc.)
              :active-tab string}] ;; ID of active tab in this column

            Default: Single column with home tab."
           (signal runtime [{:id initial-col-id
                             :width 1.0
                             :tabs [{:id initial-tab-id
                                     :type :chat
                                     :title "Assistants"
                                     :data {:room-id "personal-ai-placeholder"
                                            :room-name "Assistants"}}]
                             :active-tab initial-tab-id}])))

#?(:cljs (def drag-state
           "Signal holding drag and drop state.

            State shape:
            {:dragging?   boolean
             :source-col  string   ;; Column ID being dragged from
             :source-tab  string   ;; Tab ID being dragged
             :drop-target {:type :tab-bar | :column-edge | :new-column
                           :col-id string
                           :position :before | :after | nil}}"
           (signal runtime nil)))

#?(:cljs (def active-column-id
           "Signal holding the ID of the currently active/focused column.
            Navigation actions open content in the active column.
            Nil means no column is explicitly active (falls back to first column)."
           (signal runtime nil)))

;; =============================================================================
;; Column Layout Functions
;; =============================================================================

(defn gen-id
  "Generate a unique ID string."
  []
  (str (random-uuid)))

(defn default-layout
  "Return the default single-column home layout."
  []
  [{:id (gen-id)
    :width 1.0
    :tabs [{:id (gen-id)
            :type :home
            :title "Home"
            :data {}}]
    :active-tab nil}])

(defn ensure-layout!
  "Ensure layout has at least one column. Reset to default if empty."
  []
  #?(:cljs
     (binding [rtc/*execution-context* runtime]
       (when (empty? @layout-columns)
         (reset! layout-columns (default-layout))))
     :clj nil))

;; =============================================================================
;; Focus changes, for observers outside the render tree
;; =============================================================================
;;
;; The URL is a PROJECTION of the focused tab, not a second source of truth for
;; it. Rather than mirror layout state into another signal — which would put a
;; second tracker in a tree that already owns `layout-columns`, the documented
;; double-tracking failure above — focus changes are announced, and whoever
;; cares (the router) reads the layout and writes the URL imperatively.
;;
;; Inverted dependency on purpose: `signals` knows nothing about routing. The
;; router registers itself here, so there is no cycle between the two.

#?(:cljs (defonce ^:private focus-listeners (atom {})))

(defn on-focus-change!
  "Register `f` under `k`, called as `(f reason)` after any mutation that
   changes WHICH tab is focused. `reason` is `:navigate` | `:passive` — see
   `notify-focus-change!`. Re-registering under the same key replaces."
  [k f]
  #?(:cljs (swap! focus-listeners assoc k f) :clj nil))

(defn- notify-focus-change!
  "Announce a focus change, and WHY.

   `reason` is `:navigate` — the user asked to look at something, so a URL
   observer should leave a history entry — or `:passive`, a consequence of
   something else (column focus moved, a tab closed) which must not. Get this
   backwards and Back becomes an undo stack for window management.

   Never lets an observer's failure break a layout mutation: a broken URL
   writer must not make tabs unclickable."
  [reason]
  #?(:cljs (doseq [[k f] @focus-listeners]
             (try (f reason)
                  (catch :default e
                    (js/console.error "[signals] focus listener failed:" k e))))
     :clj nil))

(defn focused-tab
  "The active tab of the active column — the ONE tab the URL speaks for.

   Falls back to the first column when no column is explicitly active, which is
   what `active-column-id` documents as its nil meaning. Pure, so the router can
   ask without touching signals."
  [cols active-col-id]
  (let [col (or (some #(when (= (:id %) active-col-id) %) cols)
                (first cols))]
    (some #(when (= (:id %) (:active-tab col)) %) (:tabs col))))

(defn set-active-column!
  "Set the active column by ID.
   Called when user clicks in a column or when a new column is created.

   This updates both the signal AND directly manipulates the DOM to add/remove
   the 'active' class. This avoids triggering a full re-render just for a CSS
   class change, which would disrupt ongoing interactions like TipTap editing."
  [col-id]
  #?(:cljs
     (binding [rtc/*execution-context* runtime]
       ;; Update the signal (for any code that needs to query the active column)
       (reset! active-column-id col-id)
       ;; Direct DOM manipulation for the active class - avoids re-render
       (when-let [container (js/document.getElementById "columns-container")]
         ;; Remove active class from all columns
         (doseq [col-el (array-seq (.querySelectorAll container ".column.active"))]
           (.remove (.-classList col-el) "active"))
         ;; Add active class to the new active column
         (when-let [active-el (.querySelector container (str ".column[data-col-id=\"" col-id "\"]"))]
           (.add (.-classList active-el) "active")))
       ;; Focus moved between columns, so the focused TAB changed even though no
       ;; tab did. This mutator deliberately bypasses the render path (see the
       ;; DOM-class note above), so an observer wired only into rendering would
       ;; never hear about it — announce explicitly. PASSIVE: looking at another
       ;; column is not navigating anywhere.
       (notify-focus-change! :passive))
     :clj nil))

(defn get-active-column-id
  "Get the active column ID, falling back to first column if none set."
  [columns]
  #?(:cljs
     (let [active @active-column-id]
       (if (and active (some #(= (:id %) active) columns))
         active
         (:id (first columns))))
     :clj (:id (first columns))))

(defn find-column-index
  "Find index of column by ID."
  [columns col-id]
  (first (keep-indexed (fn [i col] (when (= (:id col) col-id) i)) columns)))

(defn find-tab-in-column
  "Find tab index in a column by tab ID."
  [column tab-id]
  (first (keep-indexed (fn [i tab] (when (= (:id tab) tab-id) i)) (:tabs column))))

(defn open-tab!
  "Open content in a tab.

   Args:
   - tab-type: :home, :wiki, :chat, :chat-thread, :run-history, :run-inspector, :video
   - tab-data: {:page-uuid uuid} for wiki, {:room-id string} for chat, etc.
   - opts:
     - :col-id - Target column ID (default: active column)
     - :title - Tab title
     - :new-column? - Open in a new column to the right
     - :new-tab? - Open in a new tab (default: false, navigates in current tab)"
  [tab-type tab-data & [{:keys [col-id title new-column? new-tab?]}]]
  #?(:cljs
     (binding [rtc/*execution-context* runtime]
       ;; For chat tabs, initialize window to :end sentinel
       ;; This tells the render to start at the end of messages
       (when (#{:chat :chat-thread} tab-type)
         (when-let [room-id (:room-id tab-data)]
           (let [room-uuid (uuid room-id)
                 root-id (:thread-root-id tab-data)
                 context-key (if root-id
                               [room-uuid (uuid (str root-id))]
                               room-uuid)]
             (swap! chat-scroll-windows assoc context-key :end))))

       (cond
         ;; Create new column with this tab
         new-column?
         (let [new-col-id (gen-id)
               new-tab {:id (gen-id)
                        :type tab-type
                        :title (or title (name tab-type))
                        :data tab-data}]
           (swap! layout-columns
                  (fn [cols]
                    (let [new-width (/ 1.0 (inc (count cols)))
                          adjusted (mapv #(assoc % :width new-width) cols)]
                      (conj adjusted {:id new-col-id
                                      :width new-width
                                      :tabs [new-tab]
                                      :active-tab (:id new-tab)}))))
           ;; New column becomes active
           (set-active-column! new-col-id))

         ;; Create new tab in existing column (use active column if no col-id)
         new-tab?
         (let [new-tab {:id (gen-id)
                        :type tab-type
                        :title (or title (name tab-type))
                        :data tab-data}
               cols @layout-columns
               target-col-id (or col-id (get-active-column-id cols))]
           (swap! layout-columns
                  (fn [cols]
                    (let [target-idx (or (find-column-index cols target-col-id) 0)]
                      (update-in cols [target-idx]
                                 (fn [col]
                                   (-> col
                                       (update :tabs conj new-tab)
                                       (assoc :active-tab (:id new-tab)))))))))

         ;; Default: Navigate in current tab (replace active tab content)
         ;; Uses active column if no col-id specified
         :else
         (let [cols @layout-columns
               target-col-id (or col-id (get-active-column-id cols))]
           (swap! layout-columns
                  (fn [cols]
                    (let [target-idx (or (find-column-index cols target-col-id) 0)]
                      (update-in cols [target-idx]
                                 (fn [col]
                                   (let [active-id (:active-tab col)]
                                     (update col :tabs
                                             (fn [tabs]
                                               (mapv (fn [tab]
                                                       (if (= (:id tab) active-id)
                                                         (assoc tab
                                                                :type tab-type
                                                                :title (or title (name tab-type))
                                                                :data tab-data)
                                                         tab))
                                                     tabs)))))))))))
       (refresh-active-nav-keys!)
       (notify-focus-change! :navigate))
     :clj nil))

(defn refresh-singleton-tab!
  "Close and reopen a singleton tab to refresh its content.
   Used after save operations in settings/admin tabs."
  [tab-type opts]
  #?(:cljs
     (binding [rtc/*execution-context* runtime]
       (let [cols @layout-columns
             existing (some (fn [col]
                              (some (fn [tab]
                                      (when (= (:type tab) tab-type)
                                        {:col-id (:id col) :tab-id (:id tab)}))
                                    (:tabs col)))
                            cols)]
         (when existing
           ;; Remove old tab and add fresh one in same column
           (swap! layout-columns
                  (fn [cols]
                    (let [col-idx (some #(when (= (:id (nth cols %)) (:col-id existing)) %)
                                       (range (count cols)))]
                      (if col-idx
                        (update cols col-idx
                                (fn [col]
                                  (let [new-tab {:id (gen-id)
                                                 :type tab-type
                                                 :title (or (:title opts) (name tab-type))
                                                 :data nil}
                                        tabs (vec (remove #(= (:id %) (:tab-id existing)) (:tabs col)))]
                                    (-> col
                                        (assoc :tabs (conj tabs new-tab))
                                        (assoc :active-tab (:id new-tab))))))
                        cols)))))))
     :clj nil))

(declare set-active-tab! set-active-column!)

#?(:cljs
   (def active-nav-keys
     "What the sidebar highlights: the `active-tab-keys` set, republished as its
      OWN signal.

      Why not just track `layout-columns` in the nav? Because
      `render-columns-spin` already owns it, and a second tracker of the same
      signal makes the nav spin run twice — the second run finding the
      `user-rooms` interval already consumed and rendering \"No chat rooms\"
      while the signal held three. That is the double-tracking failure the chat
      timeline hit before (see CLAUDE.md \"single-owner tracking\").

      A separate signal gives the nav sole ownership, and it changes only when
      the SELECTION changes — not on every column resize or reorder, which
      `layout-columns` also carries."
     (signal runtime #{})))

#?(:cljs
   (defn active-tab-keys
     "The set of nav targets currently open in an ACTIVE tab, as
      `[type identifier]` pairs — e.g. `[:wiki \"<page-uuid>\"]`,
      `[:chat \"<room-id>\"]`, `[:files \"<room-id>\"]`, `[:proposals nil]`.

      One entry per column, since every column shows one active tab at a time
      and the sidebar should mark all of them, not just the focused column.

      Derived rather than stored: the layout already knows what is open, and a
      second source of truth for \"what is selected\" would drift from it the
      first time a tab is closed or moved."
     [cols]
     (into #{}
           (keep (fn [col]
                   (when-let [t (some #(when (= (:id %) (:active-tab col)) %)
                                      (:tabs col))]
                     (let [d (:data t)]
                       [(:type t)
                        ;; A PAGE IS (store, uuid) — the uuid alone does not
                        ;; identify one. The seed assigns deterministic uuids per
                        ;; page ROLE, so every KB's "SKILL" page is
                        ;; 00000000-…-000000000400; keying on the uuid alone lit
                        ;; up every SKILL in the sidebar whenever any one of them
                        ;; was open (measured across 5 KB scopes). The same
                        ;; collision arises without the seed — a page copied
                        ;; between KBs, a forked KB, a `dh://` reference — so the
                        ;; scope belongs in the key regardless of how ids are
                        ;; minted. Same reason `/page/<scope>/<uuid>` needs two
                        ;; segments (see `routes.cljc`).
                        ;;
                        ;; Rooms, files and proposals are keyed by ids that are
                        ;; unique in the SYSTEM db, so they need no qualifier.
                        (if-let [pu (:page-uuid d)]
                          (str (:db-scope d) "/" pu)
                          (some-> (or (:uuid d) (:room-id d) (:db-scope d)) str))]))))
           cols)))

#?(:cljs
   (defn refresh-active-nav-keys!
     "Republish the selection for the sidebar. Called from every mutation that
      changes WHICH tab is active; width and ordering changes are deliberately
      not, since they do not move the highlight. Cheap and idempotent — it
      writes only when the set actually differs, so a no-op mutation does not
      wake the nav."
     []
     (let [next (active-tab-keys @layout-columns)]
       (when (not= next @active-nav-keys)
         (reset! active-nav-keys next)))))

(defn open-or-activate-tab!
  "Open a tab of the given type, or activate it if one already exists.
   Singleton tabs like :settings and :admin should use this to avoid duplicates."
  [tab-type tab-data opts]
  #?(:cljs
     (binding [rtc/*execution-context* runtime]
       (let [cols @layout-columns
             ;; Search all columns for an existing tab of this type
             existing (some (fn [col]
                              (some (fn [tab]
                                      (when (= (:type tab) tab-type)
                                        {:col-id (:id col) :tab-id (:id tab)}))
                                    (:tabs col)))
                            cols)]
         (if existing
           ;; Activate the existing tab
           (do
             (set-active-tab! (:col-id existing) (:tab-id existing))
             (set-active-column! (:col-id existing)))
           ;; Create new tab
           (open-tab! tab-type tab-data (assoc opts :new-tab? true)))))
     :clj nil))

(defn close-tab!
  "Close a tab. If it's the last tab in a column, close the column too.
   Also disconnects KB connections when no tabs use that KB anymore."
  [col-id tab-id]
  #?(:cljs
     (binding [rtc/*execution-context* runtime]
       ;; Find the tab's db-scope before closing (for KB cleanup)
       (let [cols @layout-columns
             closing-tab
             (some (fn [col]
                     (when (= (:id col) col-id)
                       (some (fn [tab] (when (= (:id tab) tab-id) tab)) (:tabs col))))
                   cols)
             closing-tab-scope (get-in closing-tab [:data :db-scope])
             ;; Release the closed tab's cached data (Issues 2/3): the
             ;; per-room full-timeline vector / per-(page,scope,ref) block
             ;; entries in the query cache, the room's drive snapshot, and
             ;; the scope's (scope,ref) view-db signals. These otherwise
             ;; accumulate for the whole session.
             _ (let [{:keys [type data]} closing-tab]
                 (case type
                   :wiki (when-let [pu (:page-uuid data)]
                           (dq/evict-cache-for-entity! pu))
                   :chat (when-let [rid (:room-id data)]
                           (dq/evict-cache-for-entity! (uuid rid)))
                   :files (when-let [rid (:room-id data)]
                            (swap! drive-data dissoc (str rid)))
                   nil))]
         (swap! layout-columns
                (fn [cols]
                  (let [col-idx (find-column-index cols col-id)]
                    (if-not col-idx
                      cols
                      (let [col (nth cols col-idx)
                            remaining-tabs (vec (remove #(= (:id %) tab-id) (:tabs col)))]
                        (if (empty? remaining-tabs)
                          ;; Remove the column entirely
                          (let [new-cols (vec (concat (subvec cols 0 col-idx)
                                                      (subvec cols (inc col-idx))))
                                ;; Redistribute widths
                                n (count new-cols)]
                            (if (zero? n)
                              (default-layout)  ;; Reset if all closed
                              (mapv #(assoc % :width (/ 1.0 n)) new-cols)))
                          ;; Update the column with remaining tabs
                          (let [was-active? (= (:active-tab col) tab-id)
                                new-active (if was-active?
                                             (:id (first remaining-tabs))
                                             (:active-tab col))]
                            (assoc-in cols [col-idx]
                                      (-> col
                                          (assoc :tabs remaining-tabs)
                                          (assoc :active-tab new-active))))))))))
         ;; Clean up KB connection if no tabs use this scope anymore — but
         ;; keep KBs the user owns/has access to connected for the session,
         ;; so the sidebar nav stays live for agent-side writes even when no
         ;; wiki tab is open. (See nav.cljc post-load-rooms eager connect.)
         (when closing-tab-scope
           (let [all-tabs (mapcat :tabs @layout-columns)
                 scope-still-used? (some #(= (get-in % [:data :db-scope]) closing-tab-scope) all-tabs)
                 owned-scope? (some #(= (str (:kb/db-scope %)) closing-tab-scope)
                                    (:knowledge-bases @user-rooms))]
             (when-not (or scope-still-used? owned-scope?)
               (db/disconnect-kb! closing-tab-scope)
               (db/evict-view-db-for-scope! closing-tab-scope)))))
       ;; Ensure we always have a layout
       (ensure-layout!)
       ;; Closing the focused tab moves focus to whatever remains (or to
       ;; another column, when this one went with it), so the URL must follow.
       ;; After `ensure-layout!`, which may have rebuilt the default layout.
       ;; PASSIVE: closing a tab is not navigating somewhere, and pushing here
       ;; would make Back re-show a URL whose tab no longer exists.
       (refresh-active-nav-keys!)
       (notify-focus-change! :passive))
     :clj nil))

(defn close-tab-of-type!
  "Close the singleton tab of `tab-type`, if one is open.

   The counterpart to `open-or-activate-tab!`, and it exists for the same
   reason: a dialog tab has no handle on its own identity, so a view that
   finished its job could not dismiss itself. The New Team dialog therefore
   stayed open after a SUCCESSFUL create — indistinguishable, from the user's
   side, from having done nothing at all."
  [tab-type]
  #?(:cljs
     (binding [rtc/*execution-context* runtime]
       (when-let [found (some (fn [col]
                                (some (fn [tab]
                                        (when (= (:type tab) tab-type)
                                          {:col-id (:id col) :tab-id (:id tab)}))
                                      (:tabs col)))
                              @layout-columns)]
         (close-tab! (:col-id found) (:tab-id found))))
     :clj nil))

(defn set-active-tab!
  "Set the active tab in a column."
  [col-id tab-id]
  #?(:cljs
     (binding [rtc/*execution-context* runtime]
       (swap! layout-columns
              (fn [cols]
                (let [col-idx (find-column-index cols col-id)]
                  (if col-idx
                    (assoc-in cols [col-idx :active-tab] tab-id)
                    cols))))
       (refresh-active-nav-keys!)
       (notify-focus-change! :navigate))
     :clj nil))

(defn move-tab!
  "Move a tab from one column to another."
  [from-col-id tab-id to-col-id & [{:keys [position]}]]
  #?(:cljs
     (binding [rtc/*execution-context* runtime]
       (swap! layout-columns
              (fn [cols]
                (let [from-idx (find-column-index cols from-col-id)
                      to-idx (find-column-index cols to-col-id)]
                  (if (and from-idx to-idx)
                    (let [from-col (nth cols from-idx)
                          tab (first (filter #(= (:id %) tab-id) (:tabs from-col)))
                          was-active? (= (:active-tab from-col) tab-id)
                          ;; Remove from source
                          remaining-tabs (vec (remove #(= (:id %) tab-id) (:tabs from-col)))
                          cols-after-remove
                          (if (empty? remaining-tabs)
                            ;; Remove empty column
                            (vec (concat (subvec cols 0 from-idx)
                                         (subvec cols (inc from-idx))))
                            ;; Update the column - fix active-tab if needed
                            (-> cols
                                (assoc-in [from-idx :tabs] remaining-tabs)
                                (assoc-in [from-idx :active-tab]
                                          (if was-active?
                                            (:id (first remaining-tabs))
                                            (:active-tab from-col)))))
                          ;; Recalculate to-idx if we removed a column before it
                          to-idx' (if (and (empty? remaining-tabs) (< from-idx to-idx))
                                    (dec to-idx)
                                    (find-column-index cols-after-remove to-col-id))]
                      (if (and tab to-idx')
                        ;; Add to target
                        (-> cols-after-remove
                            (update-in [to-idx' :tabs] conj tab)
                            (assoc-in [to-idx' :active-tab] (:id tab)))
                        cols-after-remove))
                    cols)))))
     :clj nil))

(defn create-column-with-tab!
  "Create a new column from a tab (for drag to edge)."
  [from-col-id tab-id position]
  #?(:cljs
     (binding [rtc/*execution-context* runtime]
       (swap! layout-columns
              (fn [cols]
                (let [from-idx (find-column-index cols from-col-id)
                      from-col (when from-idx (nth cols from-idx))
                      tab (when from-col
                            (first (filter #(= (:id %) tab-id) (:tabs from-col))))]
                  (if tab
                    (let [was-active? (= (:active-tab from-col) tab-id)
                          ;; Remove from source
                          remaining-tabs (vec (remove #(= (:id %) tab-id) (:tabs from-col)))
                          cols-after-remove
                          (if (empty? remaining-tabs)
                            (vec (concat (subvec cols 0 from-idx)
                                         (subvec cols (inc from-idx))))
                            ;; Update the column - fix active-tab if needed
                            (-> cols
                                (assoc-in [from-idx :tabs] remaining-tabs)
                                (assoc-in [from-idx :active-tab]
                                          (if was-active?
                                            (:id (first remaining-tabs))
                                            (:active-tab from-col)))))
                          ;; Create new column
                          new-col {:id (gen-id)
                                   :width 0  ;; Will be recalculated
                                   :tabs [tab]
                                   :active-tab (:id tab)}
                          ;; Insert at position
                          new-cols (case position
                                     :start (vec (cons new-col cols-after-remove))
                                     :end (conj cols-after-remove new-col)
                                     cols-after-remove)
                          ;; Redistribute widths
                          n (count new-cols)]
                      (mapv #(assoc % :width (/ 1.0 n)) new-cols))
                    cols)))))
     :clj nil))

(defn resize-column!
  "Resize a column by adjusting its width fraction."
  [col-id new-width]
  #?(:cljs
     (binding [rtc/*execution-context* runtime]
       (swap! layout-columns
              (fn [cols]
                (let [col-idx (find-column-index cols col-id)
                      n (count cols)]
                  (if (and col-idx (> n 1))
                    (let [old-width (:width (nth cols col-idx))
                          delta (- new-width old-width)
                          ;; Distribute delta to other columns proportionally
                          others-total (- 1.0 old-width)
                          scale (if (> others-total 0)
                                  (/ (- others-total delta) others-total)
                                  1)]
                      (mapv (fn [col i]
                              (if (= i col-idx)
                                (assoc col :width new-width)
                                (update col :width * scale)))
                            cols (range)))
                    cols)))))
     :clj nil))

;; Drag state functions
(defn start-drag!
  "Start dragging a tab."
  [col-id tab-id]
  #?(:cljs
     (binding [rtc/*execution-context* runtime]
       (reset! drag-state {:dragging? true
                           :source-col col-id
                           :source-tab tab-id
                           :drop-target nil}))
     :clj nil))

(defn update-drop-target!
  "Update the current drop target during drag."
  [target]
  #?(:cljs
     (binding [rtc/*execution-context* runtime]
       (swap! drag-state assoc :drop-target target))
     :clj nil))

(defn end-drag!
  "End dragging and execute the drop action."
  []
  #?(:cljs
     (binding [rtc/*execution-context* runtime]
       (let [{:keys [source-col source-tab drop-target]} @drag-state]
         (when drop-target
           (case (:type drop-target)
             :tab-bar
             (move-tab! source-col source-tab (:col-id drop-target))

             :new-column
             (create-column-with-tab! source-col source-tab (:position drop-target))

             nil))
         (reset! drag-state nil)))
     :clj nil))

(defn cancel-drag!
  "Cancel the current drag operation."
  []
  #?(:cljs
     (binding [rtc/*execution-context* runtime]
       (reset! drag-state nil))
     :clj nil))
