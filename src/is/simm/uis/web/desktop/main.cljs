(ns is.simm.uis.web.desktop.main
  "Main entry point for the Simmis wiki application.

   Initialization sequence:
   1. Connect to server (kabel) with automatic reconnection
   2. Initialize database signals
   3. Initialize router
   4. Mount the app shell with view routing

   Routes:
   - #/ or empty -> Home (pages list)
   - #/page/{uuid} -> Page editor"
  (:require [is.simm.uis.web.desktop.runtime :refer [runtime]]
            [org.replikativ.spindel.engine.core :as rtc]
            [org.replikativ.spindel.dom.elements :as el]
            [org.replikativ.spindel.dom.browser :as browser]
            [org.replikativ.spindel.dom.render :as render]
            [org.replikativ.spindel.effects.track :refer [track]]
            [org.replikativ.spindel.incremental.interval :as iv]
            [org.replikativ.spindel.distributed.core :as dist]
            [is.simm.uis.web.desktop.router :as router]
            [is.simm.uis.web.desktop.db-signal :as db-signal]
            [is.simm.uis.web.desktop.signals :as sig]
            [is.simm.uis.web.desktop.views.status-bar :as status-bar]
            [is.simm.uis.web.desktop.views.chat :as chat]
            [is.simm.uis.web.desktop.views.nav :as nav]
            [is.simm.uis.web.desktop.views.columns :as columns]
            [is.simm.uis.web.desktop.block-editor :as block-editor]
            [is.simm.uis.web.desktop.block-remote :as remote]
            [is.simm.uis.web.desktop.settings-remote :as settings-remote]
            [is.simm.uis.web.desktop.refs :as refs]
            [org.replikativ.spindel.effects.await :refer [await] :include-macros true]
            ;; Kabel connection - use shared client from web.cljs
            [superv.async :refer [S restarting-supervisor] :refer-macros [go-try go-super <?]]
            [clojure.core.async :refer [go <! promise-chan put! alts! timeout]]
            [is.simm.distributed-scope :refer [connect-distributed-scope]]
            [is.simm.runtimes.web :as web]
            [datahike.api :as dh]
            [datahike.reference :as dh-ref])
  (:require-macros [org.replikativ.spindel.spin.cps :refer [spin]]
                   [org.replikativ.spindel.dom.elements :as el]))

;; =============================================================================
;; Configuration
;; =============================================================================

(def ^:const SERVER-URL web/url)
(def ^:const SERVER-ID web/server-id)

;; =============================================================================
;; Lucide Icon Initialization
;; =============================================================================

(defn init-lucide-observer!
  "Set up a MutationObserver to refresh Lucide icons when new elements are added.

   This is needed because spindel's delta rendering adds elements directly to
   the DOM, and Lucide needs to be notified to replace <i data-lucide='...'>
   placeholders with SVG icons."
  [container]
  (let [;; Debounce the createIcons call to batch multiple mutations
        pending (atom false)
        refresh-icons!
        (fn []
          ;; Check if lucide is loaded (it may load asynchronously from CDN)
          (when-let [lucide (aget js/window "lucide")]
            (when-not @pending
              (reset! pending true)
              (js/requestAnimationFrame
                (fn []
                  (reset! pending false)
                  (.createIcons lucide #js {:nameAttr "data-lucide"}))))))]
    ;; Create observer for new nodes with data-lucide attribute
    (let [observer (js/MutationObserver.
                     (fn [mutations]
                       (let [needs-refresh?
                             (some (fn [mutation]
                                     (or
                                       ;; Check added nodes for data-lucide
                                       (some (fn [node]
                                               (when (.-querySelector node)
                                                 (.querySelector node "[data-lucide]")))
                                             (.-addedNodes mutation))
                                       ;; Check if an attribute changed to data-lucide
                                       (and (= (.-type mutation) "attributes")
                                            (= (.-attributeName mutation) "data-lucide"))))
                                   mutations)]
                         (when needs-refresh?
                           (refresh-icons!)))))]
      (.observe observer container
                #js {:childList true
                     :subtree true
                     :attributes true
                     :attributeFilter #js ["data-lucide"]})
      ;; Initial call to handle any existing icons
      (refresh-icons!)
      observer)))

;; =============================================================================
;; Runtime State
;; =============================================================================

;; Runtime is imported from runtime.cljc (shared across the app)
(defonce render-handle (atom nil))

;; =============================================================================
;; Hot Reload Support
;; =============================================================================

(declare make-app-spin)  ;; Forward declaration for hot reload

(defn ^:dev/before-load stop!
  "Called before hot reload. Stops the current render."
  []
  (js/console.log "[Hot Reload] Stopping current render...")
  (when-let [{:keys [stop!]} @render-handle]
    (stop!)))

(defn ^:dev/after-load reload!
  "Called after hot reload. Remounts the app with new code."
  []
  (js/console.log "[Hot Reload] Remounting app with new code...")
  (let [container (js/document.getElementById "app")
        discharge (browser/make-dom-discharge js/document)]
    (when container
      (binding [rtc/*execution-context* runtime]
        (let [app-spin (make-app-spin)]
          (reset! render-handle
                  (render/render-spin! container app-spin discharge))))))
  (js/console.log "[Hot Reload] Done!"))

;; Register runtime for spin-remote distributed functions
(dist/register-context! :default runtime)

(defonce ^:private active-connection-session (atom nil))

(defn- stop-connection-session!
  []
  (when-let [session @active-connection-session]
    (web/stop-session! session))
  (reset! active-connection-session nil))

(defn connect-with-retry!
  "Connect to server with automatic reconnection on failure.
   Uses restarting-supervisor to retry on connection errors.
   Shows connection status in the status bar.
   Returns immediately; signals first-connect-ch when initially connected."
  [kabel-client first-connect-ch retry-count session-supervisor]
  (restarting-supervisor
    (fn [S]
      (go-super S
        (sig/set-connection-status! :connecting)
        (js/console.log "[Connection] Connecting to" SERVER-URL "...")

        ;; Show connecting status (different message for retry vs first connect)
        (let [retries @retry-count
              msg (if (zero? retries)
                    "Connecting to server..."
                    (str "Reconnecting to server (attempt " (inc retries) ")..."))]
          (sig/show-loading! msg))

        ;; Connect to server
        (<? S (connect-distributed-scope S kabel-client SERVER-URL))
        (js/console.log "[Connection] Connected!")
        (sig/set-connection-status! :connected)
        (reset! retry-count 0)

        ;; Show success briefly, then hide
        (if (zero? @retry-count)
          (sig/hide-status!)  ;; First connect - just hide, init will continue
          (sig/show-success! "Reconnected to server"))

        ;; Signal first connection (only delivers once)
        (put! first-connect-ch :connected)

        ;; Keep the supervisor alive - it will restart on any error
        ;; The kabel connection errors will propagate up and trigger restart
        ;; Note: Reconnection sync is handled by konserve-sync internally
        (<? S (promise-chan))))  ;; Block forever (until error)

    :delay 5000           ;; 5 second delay between retries
    :retries nil          ;; Infinite retries
    :supervisor session-supervisor
    :log-fn (fn [level msg]
              (sig/set-connection-status! :disconnected)
              (swap! retry-count inc)
              ;; superv.async reports some background failures (notably
              ;; :stale-error-in-supervisor) at :info while carrying the real
              ;; throwable in the message. Preserve the semantic severity in
              ;; DevTools instead of disguising an error as console.log.
              (cond
                (or (= level :error) (:error msg))
                (do
                  (js/console.error "[Connection] Error:" (pr-str msg))
                  ;; Show error in status bar with retry info
                  (sig/show-error!
                    "Connection lost - retrying in 5s"
                    (str msg)
                    :network))

                (= level :warn)
                (js/console.warn "[Connection]" (pr-str msg))

                :else
                (js/console.log "[Connection]" (pr-str msg))))))


;; =============================================================================
;; App Rendering
;; =============================================================================

(defn render-columns-spin
  "Sub-spin for columns. Tracks only **structural** signals — layout
  and per-tab UI state. Data signals (kb-states, room-states, local-db,
  etc.) are NOT tracked here: their downstream consumers (page-editor,
  chat-room render, context-footer wiki branch) self-track exactly the
  per-KB / per-room signal they need. A write to KB X therefore does
  not re-run this spin; it only re-fires the spin(s) tracking X's
  signal.

  Still tracked:
  - layout-columns, active-column-id  (column layout & focus)
  - local-db                          (shared DB — used by non-KB tabs)
  - chat-windows                      (scroll windows for chat tabs)
  - settings-data, admin-data         (settings/admin tab content)

  Removed:
  - kb-states (replaced by per-KB signals; wiki tabs self-track)"
  []
  (spin
    (let [layout-columns-iv (track sig/layout-columns)
          layout-columns (iv/get-new layout-columns-iv)
          active-column-id @sig/active-column-id
          local-db-iv (track sig/local-db)
          local-db (iv/get-new local-db-iv)
          chat-windows-iv (track sig/chat-scroll-windows)
          chat-windows (iv/get-new chat-windows-iv)
          settings-data-iv (track sig/settings-data)
          settings-data (iv/get-new settings-data-iv)
          admin-data-iv (track sig/admin-data)
          admin-data (iv/get-new admin-data-iv)
          ;; Context-footer state must be tracked HERE (not inside
          ;; render-column): a mid-spin track resume re-executes the
          ;; message-timeline query whose delta interval is empty on an
          ;; unchanged db — the chat list rendered blank on footer
          ;; toggle. Top-level tracking re-creates the column spins
          ;; fresh, same as chat-windows.
          footer-states-iv (track sig/context-footers)
          footer-states (iv/get-new footer-states-iv)]
      (await (columns/render-columns-container
              layout-columns active-column-id local-db chat-windows
              settings-data admin-data nil footer-states)))))

(defn render-status-bar-spin
  "The app's user-visible error/loading surface.

   `sig/status` had NO readers and `status-bar-view` was never mounted, so
   `show-error!` / `show-loading!` / `show-success!` wrote into a void — every
   failure they reported was invisible. Its own spin, tracking only `sig/status`,
   so a status change does not re-run nav or the columns."
  []
  (spin
    (let [status (iv/get-new (track sig/status))]
      (status-bar/status-bar-view
       {:status status
        :on-dismiss (fn [] (binding [rtc/*execution-context* runtime]
                             (sig/hide-status!)))
        :on-toggle-expand (fn [] (binding [rtc/*execution-context* runtime]
                                   (sig/toggle-status-expanded!)))}))))

(defn render-app
  "Render the chromeless app shell with nav sidebar and columns.
   Each child has its own spin so signal changes only re-run the spin
   that depends on the changed signal. render-nav-sidebar is itself a
   self-tracking spin tree (see views/nav.cljc)."
  []
  (spin
    (let [nav-vnode (await (nav/render-nav-sidebar))
          cols-vnode (await (render-columns-spin))
          status-vnode (await (render-status-bar-spin))]
      (el/div {:class "app-shell app-shell--chromeless"}
        nav-vnode
        cols-vnode
        status-vnode))))

;; =============================================================================
;; Main Spin
;; =============================================================================

(defn make-app-spin
  "Create the main app spin. Tracks nothing itself — child spins
   (nav/render-nav-sidebar, render-columns-spin) own their reactive
   scopes. This is critical: spindel's invalidate-created-spins! marks
   the parent's child spins dirty whenever the parent re-runs, so the
   parent must NOT track signals that children own — otherwise child
   re-runs cascade through the parent on every change.

   Settings bootstrap (load on first mount) runs via a separate kickoff
   when current-user is set — avoids tracking current-user here."
  []
  (let [current-user @sig/current-user
        settings-data @sig/settings-data]
    ;; One-shot settings load on first mount (no need to track settings-data
    ;; here — once loaded, the settings-data signal change triggers any
    ;; child spin that tracks it).
    (when (and current-user (nil? settings-data))
      (let [s (settings-remote/load-settings! web/server-id (:id current-user))]
        (s (fn [result]
             (binding [rtc/*execution-context* runtime]
               (reset! sig/settings-data result)
               (when-let [syn (get-in result [:ui-prefs :ui-pref/syntax])]
                 (reset! sig/syntax-pref syn))))
           (fn [err]
             (js/console.error "[main] Failed to load settings on boot:" err))))))
  (spin
    (await (render-app))))


;; =============================================================================
;; Click Handler
;; =============================================================================

(defn- room-target-kbs
  "Return the list of KB entries (from the user-rooms payload) we
   should search when resolving a [[link]] clicked from inside a chat
   room. Prefers explicit `:room/knowledge-bases` attachments when the
   room has any; otherwise falls back to ALL user-KBs so out-of-the-box
   behavior still resolves chat links to existing pages without forcing
   the user to manually attach a KB to every room first.

   Once `:room/knowledge-bases` is populated (via room-settings), it
   becomes authoritative — only those KBs are searched."
  [room-id]
  (let [ur (binding [rtc/*execution-context* runtime] @sig/user-rooms)
        rooms-list (when (map? ur) (:rooms ur))
        kbs-list (when (map? ur) (:knowledge-bases ur))
        attached-ids (some (fn [r]
                             (when (= (str (:room/id r)) (str room-id))
                               (set (:room/knowledge-bases r))))
                           (or rooms-list []))]
    (cond
      (seq attached-ids)
      ;; :room/knowledge-bases carries KB IDs (grant rows); accept the
      ;; db-scope too for robustness. (Comparing only db-scope against
      ;; ids matched NOTHING once attachments existed — chat [[links]]
      ;; silently fell through and created orphan scopeless pages.)
      (filterv #(or (contains? attached-ids (str (:kb/id %)))
                    (contains? attached-ids (str (:kb/db-scope %))))
               (or kbs-list []))

      :else
      (or kbs-list []))))

(defn- find-page-in-room-kbs
  "Resolve `page-name` against the KBs reachable from `room-id` using
   the already-broadcast :knowledge-bases payload on the client.
   Returns `{:uuid ... :db-scope ...}` for the first match, or nil.
   No server roundtrip — page titles + uuids + db-scopes are already
   on the client. See `room-target-kbs` for the resolution policy."
  [page-name room-id]
  (some (fn [kb]
          (some (fn [page]
                  (when (= (:title page) page-name)
                    {:uuid (:uuid page)
                     :db-scope (:db-scope page)}))
                (:kb/pages kb)))
        (room-target-kbs room-id)))

(defn- first-room-kb-db-scope
  "Return the :db-scope (string) of the first KB we'd search for this
   room (per `room-target-kbs`). Used as the default target when
   creating a new page from a chat-side [[link]] whose title doesn't
   match any existing page."
  [room-id]
  (some-> (room-target-kbs room-id) first :kb/db-scope))

(defn handle-page-reference-click
  "Handle click on a page reference link.
   Finds or creates the page and opens it in the source column's tab.
   Ctrl+click opens in a new column.

   Resolution order, when known:
   1. Chat-side click (`room-id` provided, `db-scope` not) — search
      the room's attached KBs locally via `:knowledge-bases` payload.
      No server roundtrip. New page falls back to the room's first
      attached KB.
   2. Wiki-side click (`db-scope` provided) — find/create within that
      specific KB via the existing remote path.
   3. Neither — system-DB fallback (legacy path)."
  [page-name new-column? source-col-id & [{:keys [db-scope room-id]}]]
  (go
    (cond
      ;; Chat-side: resolve against room's attached KBs from the
      ;; already-broadcast payload.
      (and room-id (not db-scope))
      (if-let [match (find-page-in-room-kbs page-name room-id)]
        (sig/open-tab! :wiki {:page-uuid (:uuid match)
                              :db-scope (:db-scope match)}
                       {:title page-name
                        :new-column? new-column?
                        :col-id source-col-id})
        (let [target-scope (first-room-kb-db-scope room-id)
              new-uuid (random-uuid)]
          (when target-scope
            (<! (block-editor/ensure-page-remote! new-uuid page-name target-scope)))
          (sig/open-tab! :wiki (cond-> {:page-uuid new-uuid}
                                 target-scope (assoc :db-scope target-scope))
                         {:title page-name
                          :new-column? new-column?
                          :col-id source-col-id})))

      ;; Wiki-side or legacy: existing remote flow.
      :else
      ;; spin-remote yields either the page UUID, nil (genuine not-found), or an
      ;; ExceptionInfo (the RPC FAILED — e.g. :not-authorized from a stale-token
      ;; socket that reconnected anonymously). These three MUST be distinguished:
      ;; treating a failure as "not-found" mints a brand-new page and DUPLICATES
      ;; the one we merely couldn't resolve (the Giselle bug).
      (let [raw (<! (remote/find-page-by-title-remote! page-name db-scope))]
        (cond
          ;; Found it — open the existing page.
          (uuid? raw)
          (sig/open-tab! :wiki (cond-> {:page-uuid raw}
                                 db-scope (assoc :db-scope db-scope))
                         {:title page-name
                          :new-column? new-column?
                          :col-id source-col-id})

          ;; RPC failed. Do NOT create a page. If it's an auth denial, recover
          ;; the session (refresh + reload); otherwise surface it and abort so
          ;; the user can retry rather than forking a duplicate.
          (instance? js/Error raw)
          (when-not (web/maybe-reauth-on-rpc-error! raw)
            ;; Not a session failure (would have reauthed) — a genuine denial or
            ;; app error. Surface it instead of silently doing nothing, and do
            ;; NOT mint a duplicate page.
            (js/console.warn "[handle-page-reference-click] find-page-by-title failed; not creating a page:" raw)
            (binding [rtc/*execution-context* runtime]
              (sig/show-error! (str "Couldn't resolve \"" page-name
                                    "\" to a wiki page — this link doesn't point to a"
                                    " specific database. (Cross-wiki links need the"
                                    " dh:// form; older bare [[links]] may not resolve.)"))))

          ;; Genuine not-found (nil/false) — create it.
          :else
          (let [new-uuid (random-uuid)]
            (<! (block-editor/ensure-page-remote! new-uuid page-name db-scope))
            (sig/open-tab! :wiki (cond-> {:page-uuid new-uuid}
                                   db-scope (assoc :db-scope db-scope))
                           {:title page-name
                            :new-column? new-column?
                            :col-id source-col-id})))))))

(defn- open-dh-reference!
  "Open a cross-database `[[dh://…]]` link. The URI already carries the target
   store id (= db-scope) and the entity uuid, so navigation is a DIRECT open —
   no resolution, no guessing. Grants are enforced server-side on the actual
   read; an ungranted/unsynced store surfaces a normal error (never a logout,
   post-B). `title` is the link's display text (for the tab)."
  [uri title new-column? source-col-id]
  ;; Delegates to `refs/open!` — the ONE place that knows how a reference
  ;; becomes a tab. A dashboard row and a [[dh://…]] link in a wiki page are
  ;; the same act, and were two code paths until the perspectives needed it.
  (let [;; textContent arrives as "[[Display]]" — strip the brackets for the tab label.
        title (some-> title (.replace (js/RegExp. "^\\[\\[|\\]\\]$" "g") ""))]
    (refs/open! uri {:title (not-empty title)
                     :new-column? new-column?
                     :col-id source-col-id})))

(defn handle-app-click
  "Handle clicks via data-action delegation, page references, and user mentions.
   Handles both wiki (.page-reference, .user-reference) and chat (.page-ref, .mention) styles."
  [e]
  (let [target (.-target e)
        new-column? (or (.-metaKey e) (.-ctrlKey e))
        ;; Find source column from click location
        column-el (.closest target ".column")
        ;; NB: read data-* via .getAttribute, never (.-foo (.-dataset el)) — the
        ;; latter's property name is MUNGED under :advanced (release), so every
        ;; such read returns nil in the deployed bundle (works only in dev).
        source-col-id (when column-el (.getAttribute column-el "data-col-id"))
        ;; Extract the active tab in the source column so we can route
        ;; the [[link]] resolution against the right context:
        ;;   - :wiki tab → its :db-scope (search that KB directly)
        ;;   - :chat tab → its :room-id (search the room's attached KBs
        ;;                 via the user-rooms broadcast payload)
        active-tab-data (when source-col-id
                          (binding [rtc/*execution-context* runtime]
                            (some (fn [col]
                                    (when (= (:id col) source-col-id)
                                      (first (filter #(= (:id %) (:active-tab col))
                                                     (:tabs col)))))
                                  @sig/layout-columns)))
        source-db-scope (when (= (:type active-tab-data) :wiki)
                          (get-in active-tab-data [:data :db-scope]))
        source-room-id (when (#{:chat :chat-thread} (:type active-tab-data))
                         (get-in active-tab-data [:data :room-id]))
        ;; Check for page reference click - wiki uses .page-reference, chat uses .page-ref
        wiki-ref-el (.closest target ".page-reference")
        chat-ref-el (.closest target ".page-ref")
        page-ref-el (or wiki-ref-el chat-ref-el)
        ;; Check for user mention click - wiki uses .user-reference, chat uses .mention
        wiki-user-el (.closest target ".user-reference")
        chat-user-el (.closest target ".mention")
        user-ref-el (or wiki-user-el chat-user-el)]
    (cond
      ;; Handle page reference click - navigate in the column where click originated
      page-ref-el
      (let [dh-uri (.getAttribute page-ref-el "data-ref")]   ; data-ref = a dh:// URI
        (if (seq dh-uri)
          ;; Cross-database link: open the target directly (URI carries scope+uuid).
          (do (.preventDefault e)
              (open-dh-reference! dh-uri (.-textContent page-ref-el)
                                  new-column? source-col-id))
          ;; Same-KB link by title (wiki .page-reference / chat .page-ref).
          (let [page-name (or (.getAttribute page-ref-el "data-page-name")
                             (.getAttribute page-ref-el "data-page"))]
            (when page-name
              (.preventDefault e)
              (handle-page-reference-click page-name new-column? source-col-id
                                           {:db-scope source-db-scope
                                            :room-id source-room-id})))))

      ;; Handle user mention click - open the party's profile (the @mention
      ;; destination), NOT a wiki page named after the handle.
      user-ref-el
      (let [user-name (or (.getAttribute user-ref-el "data-user-name")
                         (.getAttribute user-ref-el "data-user"))]
        (when user-name
          (.preventDefault e)
          (sig/open-tab! :profile {:handle user-name}
                         {:title (str "@" user-name)
                          :new-column? new-column?
                          :col-id source-col-id})))

      ;; Handle data-action clicks
      :else
      (let [action-el (.closest target "[data-action]")
            action (when action-el (.getAttribute action-el "data-action"))]
        (case action
          "navigate-page"
          (when-let [uuid-str (.getAttribute action-el "data-page-uuid")]
            (let [page-uuid (uuid uuid-str)
                  title (or (.getAttribute action-el "data-page-title") "Page")]
              ;; Thread source-db-scope through so the wiki tab's
              ;; live-title lookup can find the page in the right KB
              ;; signal. Without :db-scope the renderer can't resolve
              ;; the per-KB signal and falls back to the stored title.
              (sig/open-tab! :wiki (cond-> {:page-uuid page-uuid}
                                     source-db-scope (assoc :db-scope source-db-scope))
                             {:title title
                              :new-column? new-column?
                              :col-id source-col-id})))

          "toggle"
          (when-let [block-uuid-str (.getAttribute action-el "data-id")]
            (let [block-uuid (uuid block-uuid-str)]
              (js/console.log "[TOGGLE-CLICK] Toggling block:" (str block-uuid))
              (remote/toggle-block-collapsed-remote! block-uuid source-db-scope)))

          nil)))))

;; =============================================================================
;; Initialization
;; =============================================================================

(defn start-main!
  "Start the main application after authentication is complete.
   Called by web.cljs after successful login/auto-login."
  [session]
  (let [kabel-client (:client session)]
    (when-not kabel-client
      (js/console.error "[Main] No kabel client available! Auth may not have completed."))
    (when kabel-client
      (stop-connection-session!)
      (let [first-connect-ch (promise-chan)
            retry-count (atom 0)
            session-supervisor (:supervisor session)
            connection-run (connect-with-retry! kabel-client first-connect-ch retry-count
                                                session-supervisor)]
        (reset! active-connection-session (assoc session :connection-run connection-run))
        (js/console.log "[Main] ===== Initializing Simmis Wiki =====")
        (dist/set-system-peer! kabel-client)
        (js/console.log "[Main] Step 1: Starting connection with auto-retry...")

        (go-try session-supervisor
          (js/console.log "[Main] Step 1b: Waiting for authentication...")
          (let [deadline (timeout 15000)
                [auth-result auth-port] (alts! [(:auth-result session) deadline] :priority true)]
            (cond
              (= auth-port deadline)
              (do
                (js/console.error "[Main] Authentication timed out")
                (sig/show-error! "Authentication timed out" "Please reload or sign in again." :network))

              (not (web/current-session? session))
              (js/console.warn "[Main] Ignoring replaced authentication session")

              (not= :authenticated (:status auth-result))
              (do
                (js/console.error "[Main] Authentication failed:" (clj->js (:error auth-result)))
                (sig/show-error! "Authentication failed" "Refreshing your session…" :network))

              :else
              (do
                (js/console.log "[Main] Step 1b: Authenticated!")
                ;; Distributed-scope readiness depends on registration traffic
                ;; that Kabel intentionally holds behind authentication. Reuse
                ;; the bootstrap deadline so connection setup cannot hang after
                ;; auth either.
                (let [[_ connect-port] (alts! [first-connect-ch deadline] :priority true)]
                  (cond
                    (= connect-port deadline)
                    (do
                      (js/console.error "[Main] Connection bootstrap timed out")
                      (sig/show-error! "Connection timed out" "Please reload and try again." :network))

                    (not (web/current-session? session))
                    (js/console.warn "[Main] Ignoring replaced connection session")

                    :else
                    (do
                      (js/console.log "[Main] Step 1: Connected!")
                      (js/console.log "[Main] Step 2: Initializing reactive database...")
                      (let [start (js/Date.now)]
                        (<? session-supervisor (db-signal/init-reactive-db! kabel-client))
                        (js/console.log "[Main] Step 2: Database ready! Total time:"
                                        (- (js/Date.now) start) "ms"))

                      (js/console.log "[Main] Step 3: Initializing router...")
                      (router/init! runtime)
                      (js/console.log "[Main] Step 3: Router ready!")

                      (js/console.log "[Main] Step 4: Mounting app...")
                      (let [container (js/document.getElementById "app")
                            discharge (browser/make-dom-discharge js/document)]
                        (when container
                          (.addEventListener container "click" (rtc/make-handler runtime handle-app-click))
                          (init-lucide-observer! container)
                          (binding [rtc/*execution-context* runtime]
                            (let [app-spin (make-app-spin)
                                  handle (render/render-spin! container app-spin discharge)]
                              (reset! render-handle (assoc handle :app-spin app-spin))))))

                      (js/console.log "[Main] ===== Initialization complete ====="))))))))))))

(defn ^:export init
  "Initialize the Simmis wiki application.
   Registers main init callback and triggers auth check via web.cljs."
  []
  ;; Register the main init function for web.cljs to call after auth
  (aset js/window "__simmis_main_init" start-main!)

  ;; web.cljs init handles auth check → login page → start-app! → __simmis_main_init
  (web/init))

;; =============================================================================
;; Debug Helpers
;; =============================================================================

(defn ^:export debug-state
  "Get current application state for debugging."
  []
  {:path (router/current-path)
   :db-loaded? (some? @db-signal/local-db)})
