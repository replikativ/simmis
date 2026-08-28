(ns is.simm.uis.web.desktop.db-signal
  "Client-side database signal for reactive Datahike integration.

   Provides two spindel signals:
   - local-db: The current Datahike database instance
   - local-db-signal: DB summary ({:max-tx N :timestamp T})

   Usage:
   1. Call init-reactive-db! with kabel-peer on boot
   2. Use (track local-db) in spins to reactively query the database"
  #?(:cljs (:require [datahike.api :as d]
                     [datahike.versioning :as dv]
                     [datahike.kabel.connector :as kc]
                     [datahike.optimistic :as opt]
                     [konserve.indexeddb]  ;; Register :indexeddb store backend
                     [clojure.core.async :refer [go <! alts! put! promise-chan timeout] :include-macros true]
                     [org.replikativ.spindel.engine.core :as rtc]
                     [org.replikativ.spindel.signal :refer [->SignalRef]]
                     [is.simm.uis.web.desktop.runtime :refer [runtime]]
                     [is.simm.uis.web.desktop.chat-remote :as chat-remote]))
  #?(:cljs (:require-macros [org.replikativ.spindel.signal :refer [signal]])))

;; ============================================================================
;; Configuration
;; ============================================================================

;; Store UUID - identifies this database for sync between client and server.
;; Used as the pubsub topic for konserve-sync.
(def simmis-store-id #uuid "a1b2c3d4-e5f6-7890-abcd-ef1234567890")

;; Server peer ID - must match server-id in is.simm.runtimes.web
(def server-peer-id #uuid "05a06e85-e7ca-4213-9fe5-04ae511e50a0")

;; ============================================================================
;; Signals (created at top level with explicit runtime)
;; ============================================================================

#?(:cljs (def local-db
           "Signal holding the current Datahike database instance."
           (signal runtime nil)))

#?(:cljs (def local-db-signal
           "Signal holding DB summary {:max-tx N :timestamp T}."
           (signal runtime nil)))

#?(:cljs (def local-conn
           "Atom holding the Datahike connection for local transactions."
           (atom nil)))

#?(:cljs (def local-overlay
           "Atom holding the explicit optimistic overlay for the shared DB."
           (atom nil)))

;; ============================================================================
;; Convenience Accessors
;; ============================================================================

#?(:cljs
   (defn get-db
     "Get the current database value.
      Can be called from anywhere - uses the shared runtime."
     []
     (binding [rtc/*execution-context* runtime]
       @local-db)))

#?(:cljs
   (defn get-db-with-context
     "Get the current database value with explicit execution context.
      Useful when called from go blocks which don't preserve dynamic bindings."
     [ctx]
     (binding [rtc/*execution-context* ctx]
       @local-db)))

#?(:cljs
   (defn get-conn
     "Get the Datahike connection for local transactions.
      Returns nil if not yet initialized."
     []
     @local-conn))

#?(:cljs
   (defn get-overlay
     "Get the shared DB's optimistic overlay."
     []
     @local-overlay))

;; ============================================================================
;; Signal Updates
;; ============================================================================

#?(:cljs
   (defn- update-signals!
     "Update both signals with a new database state."
     [db]
     (binding [rtc/*execution-context* runtime]
       (reset! local-db db)
       (reset! local-db-signal {:max-tx (:max-tx db)
                                :timestamp (js/Date.now)}))))

#?(:cljs
   (defn refresh-db!
     "Refresh the local-db signal with the current database state.
      Call this after local transactions to ensure UI updates immediately."
     []
     (when-let [overlay @local-overlay]
       (update-signals! (opt/db overlay)))))

;; ============================================================================
;; Connection Initialization
;; ============================================================================

;; How long to keep waiting for a delete that has reported itself BLOCKED.
;; This bounds a pathological wait, not a legitimate one — see wipe-idb!.
#?(:cljs (def ^:private blocked-wipe-grace-ms 15000))

#?(:cljs
   (defn- wipe-idb!
     "Delete a client IndexedDB database. Returns a channel yielding
      :ok, :error or :blocked.

      `deleteDatabase` has three callbacks but only two of them END the
      request. `onsuccess` and `onerror` are terminal; `onblocked` means the
      delete is PENDING because some other connection — another tab, or a
      handle this page never closed — still holds the database open. The
      request stays live and `onsuccess` follows once those connections close.

      Reporting `blocked` as if it were an answer is what made the self-heal a
      no-op: the caller carried on and reconnected to a database that had not
      been deleted. So blocking is logged and then WAITED OUT, and only a
      terminal event resolves the channel.

      A delete that is merely slow is given all the time it needs. Only once
      the request has declared itself blocked do we bound the wait, and then
      only to turn a permanent block (a second tab nobody is going to close)
      into a loud :blocked rather than a hang."
     [db-name]
     (let [ch         (promise-chan)
           blocked-ch (promise-chan)
           req        (.deleteDatabase js/indexedDB db-name)]
       (set! (.-onsuccess req) (fn [_] (put! ch :ok)))
       (set! (.-onerror req) (fn [_] (put! ch :error)))
       (set! (.-onblocked req) (fn [_] (put! blocked-ch :blocked)))
       (go
         (let [[v port] (alts! [ch blocked-ch])]
           (if (= port ch)
             v
             (do (js/console.warn "[DB-SIGNAL] IndexedDB delete of" db-name
                                  "is blocked — another tab or an unclosed handle"
                                  "still holds it. Waiting for it to be released.")
                 (let [[v' _] (alts! [ch (timeout blocked-wipe-grace-ms)])]
                   (or v' :blocked)))))))))

#?(:cljs
   (defn connect-with-recovery
     "connect-kabel with a one-shot self-heal: when the connect fails —
      typically a stale/incompatible local replica (the `-blob-exists?
      requires async` class: a sync-mode read cold-misses the memory
      frontend and falls through to async-only IndexedDB, or a store
      layout that no longer matches the server) — wipe the store's
      IndexedDB database and reconnect once for a fresh full sync.
      Returns a channel yielding the conn or the final error.

      The retry happens only if the wipe actually succeeded. Reconnecting to a
      replica we failed to delete just reproduces the original failure, and
      reporting the second error would name the wrong cause — so a failed wipe
      surfaces the ORIGINAL connect error instead."
     [config idb-name]
     (go
       (let [conn (<! (kc/connect-kabel config {:sync? false}))]
         (if-not (instance? js/Error conn)
           conn
           (do (js/console.warn "[DB-SIGNAL] connect failed for" idb-name
                                "— wiping local replica and resyncing:"
                                (.-message conn))
               (let [wiped (<! (wipe-idb! idb-name))]
                 (if-not (= :ok wiped)
                   (do (js/console.error
                        "[DB-SIGNAL] self-heal ABORTED for" idb-name
                        "— the local replica could not be deleted (" (name wiped) ")."
                        (if (= :blocked wiped)
                          "Close other tabs on this origin and reload."
                          "The original connect error stands.")
                        "Original error:" (.-message conn))
                       conn)
                   (let [conn2 (<! (kc/connect-kabel config {:sync? false}))]
                     (when (instance? js/Error conn2)
                       (js/console.error "[DB-SIGNAL] resync after wipe also failed for"
                                         idb-name ":" (.-message conn2)))
                     conn2)))))))))

#?(:cljs
   (defn init-reactive-db!
     "Initialize the reactive database system on the client.
      Call this after connecting to server (kabel peer must be started).

      kabel-peer: The kabel client peer atom (from is.simm.runtimes.web/client)
      Returns a channel that yields :ready when complete."
     [kabel-peer]
     (js/console.log "[DB-SIGNAL] Starting init-reactive-db!...")

     (go
       (let [;; Build config for connect-kabel
             ;; Uses tiered store: memory frontend for fast reads, IndexedDB backend for persistence
             ;; Note: Uses :frontend-config/:backend-config to avoid collision with :backend :tiered
             ;; Store :id is used as the sync topic for konserve-sync
             config {:store {:backend :tiered
                             :frontend-config {:backend :memory :id simmis-store-id}
                             :backend-config {:backend :indexeddb :name "simmis-client" :id simmis-store-id}
                             :id simmis-store-id}
                     :writer {:backend :kabel
                              :peer-id server-peer-id
                              :local-peer kabel-peer}
                     :schema-flexibility :write
                     :keep-history? true}

             _ (js/console.log "[DB-SIGNAL] Calling connect-kabel...")
             conn (<! (connect-with-recovery config "simmis-client"))
             _ (js/console.log "[DB-SIGNAL] connect-kabel returned:" (some? conn))]

         ;; Store connection for local transactions
         (reset! local-conn conn)
         (js/console.log "[DB-SIGNAL] Connection stored for local transactions")

         ;; Wire the conn into datahike.optimistic. The listener fires on:
         ;;   - overlay add (after opt/transact!)
         ;;   - overlay drop (after dispatch resolves or rejects)
         ;;   - @conn advance (KabelWriter or konserve-sync echo)
         ;; Each fire delivers the *effective* db (overlay applied on top
         ;; of @conn). Pushing that into local-db means UI spins see the
         ;; optimistic state immediately and the durable state once the
         ;; server confirms — same signal, no flicker.
         (let [overlay (opt/open conn)]
           (when-let [old-overlay @local-overlay]
             (opt/close! old-overlay))
           (reset! local-overlay overlay)
           (opt/listen! overlay ::db-signal
                        (fn [{:keys [db-after]}]
                          (when-not (= (:max-tx db-after)
                                       (:max-tx (binding [rtc/*execution-context* runtime]
                                                  @local-db)))
                            (js/console.log "[DB-SIGNAL] effective-db update, max-tx:"
                                            (:max-tx db-after)))
                          (update-signals! db-after)))
           (opt/listen-status! overlay ::db-status
                               (fn [{:keys [status error ov-id]}]
                                 (case status
                                   :rejected
                                   (js/console.error "[DB-SIGNAL] optimistic write rejected:"
                                                     (str ov-id) error)
                                   :reconciliation-stalled
                                   (js/console.warn "[DB-SIGNAL] optimistic write is committed but sync is stalled:"
                                                    (str ov-id))
                                   nil)))

           ;; Initialize signals with current effective-db (= @conn at boot).
           (update-signals! (opt/db overlay))
           (js/console.log "[DB-SIGNAL] Initialized with max-tx:"
                           (:max-tx (opt/db overlay))))

         :ready))))

;; ============================================================================
;; Per-Room Database Connections
;; ============================================================================

;; Signal holding room connection status:
;; {scope-str → {:conn conn :overlay overlay :db db :rev revision}}
;; Tracked by make-app-spin so the whole render tree re-evaluates when a room connects.
#?(:cljs (def room-states
           "Signal holding all room connection states.
            Each value owns an explicit overlay and its current snapshot."
           (signal runtime {})))

;; Atom for idempotent connection tracking
#?(:cljs (defonce room-connecting (atom #{})))

#?(:cljs
   (defn- prepare-store
     "Return a channel yielding `{:ready? true}` after the server has registered
      `scope`, or `{:error err}`. Runs the remote spin outside a render spin so a render
      invalidation cannot cancel store preparation mid-flight."
     [scope]
     (let [ch (promise-chan)]
       (binding [rtc/*execution-context* runtime]
         (let [s (chat-remote/prepare-store! server-peer-id (str scope))]
           (s (fn [_] (put! ch {:ready? true}))
              (fn [err] (put! ch {:error err})))))
       ch)))

#?(:cljs
   (defn connect-room!
     "Lazily connect to a room's Datahike database.
      Idempotent — won't connect if already connected or in progress.
      Connection result is stored in room-states signal."
     [scope-uuid kabel-peer]
     (let [scope-str (str scope-uuid)]
       (when-not (or (get (binding [rtc/*execution-context* runtime] @room-states) scope-str)
                     (contains? @room-connecting scope-str))
         (swap! room-connecting conj scope-str)
         (go
           (try
             (if-let [prepare-error (:error (<! (prepare-store scope-str)))]
               (js/console.error "[ROOM-CONN] Server preparation failed for room:"
                                 scope-str prepare-error)
               (let [scope-id (if (uuid? scope-uuid) scope-uuid (uuid scope-str))
                   config {:store {:backend :tiered
                                   :frontend-config {:backend :memory :id scope-id}
                                   :backend-config {:backend :indexeddb
                                                    :name (str "simmis-room-" scope-str)
                                                    :id scope-id}
                                   :id scope-id}
                           :writer {:backend :kabel
                                    :peer-id server-peer-id
                                    :local-peer kabel-peer}
                           :schema-flexibility :write
                           :keep-history? true}
                   conn (<! (connect-with-recovery config (str "simmis-room-" scope-str)))]
                 (if (instance? js/Error conn)
                   (js/console.error "[ROOM-CONN] connect-kabel failed for room:" scope-str (.-message conn))
                   (do
                   (js/console.log "[ROOM-CONN] Connected to room:" scope-str "max-tx:" (:max-tx (d/db conn)))
                   ;; Wire into datahike.optimistic. The listener fires on
                   ;; overlay add (opt/transact!), overlay drop (server reply),
                   ;; and @conn advance — pushing the effective-db into the
                   ;; signal each time. Subsumes the prior conn-watch.
                   ;;
                   ;; Keep the overlay revision beside the snapshot so even an
                   ;; effective no-op transition has explicit signal identity.
                   (let [overlay (opt/open conn)]
                   ;; Overlay listeners receive ordered snapshot transitions,
                   ;; not Datahike TxReports. `:db-after` is authoritative.
                     (opt/listen! overlay ::room-state
                                  (fn [{:keys [db-after revision]}]
                                    (binding [rtc/*execution-context* runtime]
                                      (swap! room-states update scope-str
                                             (fn [s] (assoc s :db db-after
                                                            :rev revision))))))
                     (binding [rtc/*execution-context* runtime]
                       (swap! room-states assoc scope-str
                              {:conn conn :overlay overlay :db (opt/db overlay) :rev 0})))))))
             (catch :default e
               (js/console.error "[ROOM-CONN] Error connecting to room:" scope-str e)))
           (swap! room-connecting disj scope-str))))))

#?(:cljs
   (defn get-room-db
     "Get the current DB value for a room scope from the room-states signal.
      Returns nil if not connected."
     [scope-uuid]
     (get-in (binding [rtc/*execution-context* runtime] @room-states)
             [(str scope-uuid) :db])))

#?(:cljs
   (defn disconnect-room!
     "Disconnect from a room's database. Cleans up overlay state."
     [scope-uuid]
     (let [scope-str (str scope-uuid)]
       (when-let [{:keys [conn overlay]} (get (binding [rtc/*execution-context* runtime] @room-states)
                                              scope-str)]
         (js/console.log "[ROOM-CONN] Disconnecting room:" scope-str)
         (opt/close! overlay)
         (d/release conn)
         (binding [rtc/*execution-context* runtime]
           (swap! room-states dissoc scope-str))))))

#?(:cljs
   (defn get-room-conn
     "Get the Datahike connection for a room scope. Returns nil if not connected."
     [scope-uuid]
     (get-in (binding [rtc/*execution-context* runtime] @room-states)
             [(str scope-uuid) :conn])))

#?(:cljs
   (defn get-room-overlay
     "Get the optimistic overlay for a room scope."
     [scope-uuid]
     (get-in (binding [rtc/*execution-context* runtime] @room-states)
             [(str scope-uuid) :overlay])))

;; ============================================================================
;; Per-KB Database Connections
;; ============================================================================
;;
;; Architecture:
;;
;; Each connected KB has a private spindel signal carrying its current
;; effective-db value. `kb-db-signals` is a regular atom mapping
;; scope-str → signal. Readers call `ensure-kb-db-signal!` to retrieve
;; (or lazily mint) the signal for a scope, then `(track sig)` on it
;; locally. This means a write to KB X fires *only* the consumers that
;; tracked X's signal — page editors for OTHER KBs, and parent spins
;; that don't track any KB, stay completely quiet.
;;
;; A separate `kb-roster` signal carries just the set of connected
;; scope-strs for places that need to enumerate KBs (e.g. nav sidebar).
;; It fires only on connect/disconnect, not on per-KB writes.
;;
;; Connections themselves live in a plain (non-reactive) atom `kb-conns`
;; — looking up "what's the conn for KB X" must not subscribe a render
;; to every other KB's lifecycle.

#?(:cljs (defonce kb-conns
           ;; Plain atom (NOT a signal): {scope-str → datahike-conn}.
           ;; The conn registry has no reactive consumers — reads are
           ;; one-shot from event handlers and write paths.
           (atom {})))

#?(:cljs (defonce kb-overlays
           ;; Plain atom: {scope-str -> datahike.optimistic/Overlay}.
           (atom {})))

;; --- Per-KB signal infrastructure -----------------------------------------

#?(:cljs (defonce ^:private kb-db-signals
           ;; Regular atom (NOT a signal): {scope-str → spindel-signal of db}.
           ;; Holding signals in a plain atom means looking up a per-KB
           ;; signal doesn't subscribe to "which KBs exist" — that
           ;; concern is `kb-roster`'s.
           (atom {})))

#?(:cljs (def kb-roster
           "Signal carrying the set of currently-connected KB scope-strs.
            Fires only on connect/disconnect; per-KB writes do NOT touch
            it. Use this when you need to enumerate connected KBs
            (e.g. nav sidebar)."
           (signal runtime #{})))

#?(:cljs (def kb-heads
           "Signal: {scope-str → max-tx}. A change TOKEN for consumers that must
            react to a write in ANY replica — the timeline rail is the one that
            exists — and cannot get there by tracking the per-KB signals.

            They cannot because `iv/get-new` is a delta read: a spin tracking N
            per-KB signals re-runs when ONE of them fires and reads nil for the
            other N-1 (spindel sharp edge #1). And the obvious escape, an
            `add-watch` that folds them into an aggregate, is closed off too —
            signal watches are egress-only and must not swap other signals.

            So the shape is: track THIS to learn that something moved, then
            plain-deref the per-KB signals to read what it moved to. Deref is
            safe here precisely because this signal is written AFTER the per-KB
            signal at every site, so a consumer woken by it never reads a db
            older than the head it was told about. Carrying max-tx rather than a
            counter also means a no-op write does not wake anyone."
           (signal runtime {})))

#?(:cljs
   (defn- note-kb-head!
     "Record `scope-str`'s current max-tx. Call INSIDE the same
      `rtc/*execution-context*` binding that just updated the per-KB signal, and
      after it — see `kb-heads` for why the order is load-bearing."
     [scope-str db]
     (when-let [mt (:max-tx db)]
       (swap! kb-heads assoc scope-str mt))))

#?(:cljs
   (defn ensure-kb-db-signal!
     "Return the spindel signal for `scope`'s db value, creating it
     lazily on first call. Idempotent under concurrent access via
     swap!. The signal's value is the effective-db (or nil if the
     KB hasn't connected yet — fills in once connect-kb! completes)."
     [scope]
     (let [k (str scope)]
       (if-let [existing (get @kb-db-signals k)]
         existing
         (let [s (binding [rtc/*execution-context* runtime]
                   (signal runtime nil))]
           ;; Idempotent install: if another caller raced ahead, prefer
           ;; their signal so all readers converge on a single instance.
           (get (swap! kb-db-signals
                       (fn [m] (if (contains? m k) m (assoc m k s))))
                k))))))

#?(:cljs (defonce view-db-signals
           ;; [(str scope) ref] → signal holding the effective db for a
           ;; non-Now ref (as-of / branch / proposal). The Now path never
           ;; touches this — see ensure-view-db-signal!.
           (atom {})))
#?(:cljs (defonce ^:private view-db-done (atom #{})))  ; keys successfully projected

#?(:cljs
   (defn evict-view-db-for-scope!
     "Drop all (scope, ref) view-db signals for `scope` (on tab close)."
     [scope]
     (let [sk (str scope)
           stale (filterv (fn [[s _]] (= s sk)) (keys @view-db-signals))]
       (when (seq stale)
         (swap! view-db-signals #(apply dissoc % stale))
         (swap! view-db-done #(reduce disj % stale))))))

#?(:cljs
   (defn ensure-view-db-signal!
     "THE (scope, ref) → effective-db signal (doc §5). `ref` is
      nil / :now → the untouched existing per-KB (or shared) signal, so the
      hot path is unchanged and regression-free. A non-Now ref mints a
      per-(scope,ref) signal filled by a projector:
        {:as-of #inst}  → (d/as-of replica-db T), CLIENT-LOCAL, one-shot
                          (a past cut is immutable).
        {:forkset id
         :branches {…}} → (d/branch-as-db conn branch), the workspace as it
                          WOULD be if that ForkSet landed.
      Both retry until the base conn is connected. Drive refs are still remote.

      The forkset ref carries its own `{scope-str → branch-kw}` map, which looks
      redundant — the ForkSet already knows its forks. It is not: `signals`
      requires THIS namespace, so this namespace cannot read `sig/proposals-data`
      where that mapping lives. Passing it in the ref keeps the dependency
      pointing one way and keeps this function a pure projector rather than
      something that reaches back into app state."
     [scope ref]
     (cond
       (or (nil? ref) (= ref :now))
       (if scope (ensure-kb-db-signal! scope) local-db)

       (:forkset ref)
       (let [sk (str scope)
             branch (get (:branches ref) sk)]
         (if-not branch
           ;; This ForkSet does not touch this scope, so the honest answer is
           ;; the live one: entering a future that changes the Handbook must not
           ;; blank every OTHER wiki in the sidebar.
           (if scope (ensure-kb-db-signal! scope) local-db)
           (let [k [sk ref]
                 _ (let [stale (filterv (fn [[s r]] (and (= s sk) (not= r ref)))
                                        (keys @view-db-signals))]
                     (when (seq stale)
                       (swap! view-db-signals #(apply dissoc % stale))
                       (swap! view-db-done #(reduce disj % stale))))
                 sig (or (get @view-db-signals k)
                         (let [s (binding [rtc/*execution-context* runtime] (signal runtime nil))]
                           (get (swap! view-db-signals
                                       (fn [m] (if (contains? m k) m (assoc m k s)))) k)))]
             (when-not (contains? @view-db-done k)
               (swap! view-db-done conj k)
               (go
                 (if-let [conn (get @kb-conns sk)]
                   (try
                     ;; async on CLJS — `branch-as-db` returns a channel, and
                     ;; the branch must be a KEYWORD. Verified against a branch
                     ;; minted AFTER this client booted: it resolves from the
                     ;; local replica with no reload and no RPC, because
                     ;; konserve-sync registers KB stores with no branch
                     ;; restriction and so ships every branch's head.
                     (let [bv (<! (dv/branch-as-db conn (keyword branch) {:sync? false}))]
                       (if bv
                         (binding [rtc/*execution-context* runtime] (reset! sig bv))
                         ;; nil = not replicated YET, which is not the same as
                         ;; empty. Releasing the latch retries rather than
                         ;; rendering the future as an empty wiki.
                         (swap! view-db-done disj k)))
                     (catch :default e
                       (swap! view-db-done disj k)
                       (js/console.warn "[view-db] branch-as-db failed" sk branch e)))
                   (swap! view-db-done disj k))))
             sig)))

       (:as-of ref)
       (let [sk (str scope)
             k [sk ref]
             ;; Evict any OTHER non-now entry for this scope: one as-of
             ;; signal per scope at a time. A continuous scrub otherwise
             ;; mints a signal + engine node + done-marker per tick and
             ;; never releases them. The evicted signal loses its observer
             ;; when the view re-tracks to the new ref → spindel GC-reaps
             ;; its node at quiescence.
             _ (let [stale (filterv (fn [[s r]] (and (= s sk) (not= r ref)))
                                    (keys @view-db-signals))]
                 (when (seq stale)
                   (swap! view-db-signals #(apply dissoc % stale))
                   (swap! view-db-done #(reduce disj % stale))))
             sig (or (get @view-db-signals k)
                     (let [s (binding [rtc/*execution-context* runtime] (signal runtime nil))]
                       (get (swap! view-db-signals
                                   (fn [m] (if (contains? m k) m (assoc m k s)))) k)))]
         (when-not (contains? @view-db-done k)
           (swap! view-db-done conj k)
           ;; Fill ASYNCHRONOUSLY (go block), never synchronously inside the
           ;; consuming render body: a sync reset! of a signal the body is
           ;; about to track re-resumes that body immediately after it
           ;; completes, and the first run's deltas are lost against the
           ;; advanced dom caches (sharp-edge §1 — consume-once deltas).
           ;; This mirrors the branch-projection go-block idiom exactly.
           (go
             (if-let [conn (get @kb-conns (str scope))]
               (try
                 (let [av (d/as-of @conn (:as-of ref))]
                   (binding [rtc/*execution-context* runtime] (reset! sig av)))
                 (catch :default e
                   (swap! view-db-done disj k)
                   (js/console.warn "[view-db] as-of failed" (str scope) (:as-of ref) e)))
               (swap! view-db-done disj k))))  ; conn not ready — retry next call
         sig)

       :else
       ;; unknown ref kind → fall back to the live signal (never break render)
       (if scope (ensure-kb-db-signal! scope) local-db))))

#?(:cljs
   (defn kb-db-signal
     "Return the per-KB db signal for `scope` if it exists, else nil.
     Non-creating — use `ensure-kb-db-signal!` when you want lazy
     materialisation."
     [scope]
     (get @kb-db-signals (str scope))))

;; Atom for idempotent connection tracking (prevents double-connect)
#?(:cljs (defonce kb-connecting (atom #{})))

#?(:cljs
   (defn connect-kb!
     "Lazily connect to a KB's Datahike database.
      Idempotent — won't connect if already connected or in progress.
      The conn is stored in `kb-conns` and the effective-db value flows
      into the per-KB signal (`ensure-kb-db-signal!`), driving any spins
      that track it."
     [scope-uuid kabel-peer]
     (let [scope-str (str scope-uuid)]
       (when-not (or (get @kb-conns scope-str)
                     (contains? @kb-connecting scope-str))
         (swap! kb-connecting conj scope-str)
         (go
           (try
             (if-let [prepare-error (:error (<! (prepare-store scope-str)))]
               (js/console.error "[KB-CONN] Server preparation failed for KB:"
                                 scope-str prepare-error)
               (let [scope-id (if (uuid? scope-uuid) scope-uuid (uuid scope-str))
                 config {:store {:backend :tiered
                                 :frontend-config {:backend :memory :id scope-id}
                                 :backend-config {:backend :indexeddb
                                                  :name (str "simmis-kb-" scope-str)
                                                  :id scope-id}
                                 :id scope-id}
                         :writer {:backend :kabel
                                  :peer-id server-peer-id
                                  :local-peer kabel-peer}
                         :schema-flexibility :write
                         :keep-history? true}
                 conn (<! (connect-with-recovery config (str "simmis-kb-" scope-str)))]
                 (if (instance? js/Error conn)
                   (js/console.error "[KB-CONN] connect-kabel failed for KB:" scope-str (.-message conn))
                   (do
             (js/console.log "[KB-CONN] Connected to KB:" scope-str "max-tx:" (:max-tx (d/db conn)))
             ;; Wire into datahike.optimistic. Listener fires on overlay
             ;; add/drop and @conn advance. We push the effective-db value
             ;; directly into the per-KB signal: spindel's `not=` dedup
             ;; correctly suppresses no-op fires (the db value compares
             ;; equal when nothing changed), and propagates when the db
             ;; genuinely differs. No `:rev` counter needed.
                     (let [overlay (opt/open conn)
                           db-sig (ensure-kb-db-signal! scope-str)]
               ;; Overlay listeners receive ordered snapshot transitions.
               ;; `:db-after` is the effective DB; `:base-max-tx` is the
               ;; durable synchronization watermark.
                       (opt/listen! overlay ::kb-state
                                    (fn [{:keys [db-after base-max-tx]}]
                                      (binding [rtc/*execution-context* runtime]
                                        (reset! db-sig db-after)
                                ;; Track the durable base head, not the
                                ;; effective DB's synthetic overlay max-tx.
                                        (swap! kb-heads assoc scope-str base-max-tx))))
                       (opt/listen-status! overlay ::kb-status
                                           (fn [{:keys [status error ov-id]}]
                                             (case status
                                               :rejected
                                               (js/console.error "[KB] optimistic write rejected:"
                                                                 (str ov-id) error)
                                               :reconciliation-stalled
                                               (js/console.warn "[KB] committed optimistic write is waiting for sync:"
                                                                (str ov-id))
                                               nil)))
               ;; Seed the per-KB signal and register the conn.
               (binding [rtc/*execution-context* runtime]
                 (reset! db-sig (opt/db overlay))
                 (swap! kb-conns assoc scope-str conn)
                 (swap! kb-overlays assoc scope-str overlay)
                 (swap! kb-roster conj scope-str)
                 (note-kb-head! scope-str @conn)))))))
             (catch :default e
               (js/console.error "[KB-CONN] Error connecting to KB:" scope-str e)))
           (swap! kb-connecting disj scope-str))))))

#?(:cljs
   (defn get-kb-db
     "Get the current DB value for a KB scope from its per-KB signal.
      Returns nil if not connected."
     [scope-uuid]
     (when-let [sig (kb-db-signal scope-uuid)]
       (binding [rtc/*execution-context* runtime] @sig))))

#?(:cljs
   (defn disconnect-kb!
     "Disconnect from a KB's database. Cleans up overlay state and the
     per-KB signal. Safe to call multiple times."
     [scope-uuid]
     (let [scope-str (str scope-uuid)]
       (when-let [conn (get @kb-conns scope-str)]
         (js/console.log "[KB-CONN] Disconnecting KB:" scope-str)
         (when-let [overlay (get @kb-overlays scope-str)]
           (opt/close! overlay))
         (d/release conn)
         ;; Clear the per-KB signal value so any spin still tracking it
         ;; sees `nil` and can render an unmounted/loading state.
         (when-let [db-sig (kb-db-signal scope-str)]
           (binding [rtc/*execution-context* runtime]
             (reset! db-sig nil)))
         (swap! kb-db-signals dissoc scope-str)
         (swap! kb-conns dissoc scope-str)
         (swap! kb-overlays dissoc scope-str)
         (binding [rtc/*execution-context* runtime]
           (swap! kb-roster disj scope-str)
           ;; Drop the head too, or the rail keeps a disconnected KB's
           ;; commits on screen with no db behind them to re-read.
           (swap! kb-heads dissoc scope-str))))))

#?(:cljs
   (defn get-kb-conn
     "Get the Datahike connection for a KB scope. Returns nil if not connected."
     [scope-uuid]
     (get @kb-conns (str scope-uuid))))

#?(:cljs
   (defn get-kb-overlay
     "Get the optimistic overlay for a KB scope."
     [scope-uuid]
     (get @kb-overlays (str scope-uuid))))

;; ============================================================================
;; Dynamic Database Test (for testing KabelWriter create/connect/transact/delete)
;; ============================================================================

#?(:cljs
   (defn test-dynamic-database!
     "Test dynamic database creation, connection, transaction, and deletion.
      Call from browser console after app is loaded.

      kabel-peer: The kabel client peer atom (from is.simm.runtimes.web/client)"
     [kabel-peer]
     (let [test-store-id (random-uuid)]
       (js/console.log "=== Testing Dynamic Database ===" )
       (js/console.log "Test store-id:" (str test-store-id))

       (go
         (try
           ;; Step 1: Create database
           (js/console.log "\nStep 1: Creating database...")
           ;; Note: Uses :frontend-config/:backend-config to avoid collision with :backend :tiered
           ;; Store :id is used as the sync topic for konserve-sync
           (let [create-config {:store {:backend :tiered
                                        :frontend-config {:backend :memory :id test-store-id}
                                        :backend-config {:backend :indexeddb :name (str "test-idb-" test-store-id) :id test-store-id}
                                        :id test-store-id}
                                :writer {:backend :kabel
                                         :peer-id server-peer-id
                                         :local-peer kabel-peer}
                                :schema-flexibility :write
                                :keep-history? true}
                 create-result (<! (d/create-database create-config))]
             (js/console.log "Create result:" (pr-str create-result))

             (when (:success create-result)
               ;; Step 2: Connect to database
               (js/console.log "\nStep 2: Connecting to database...")
               (let [conn (<! (kc/connect-kabel create-config {:sync? false}))]
                 (js/console.log "Connected! max-tx:" (:max-tx @(:wrapped-atom conn)))

                 ;; Step 3: Transact schema first
                 (js/console.log "\nStep 3a: Transacting schema...")
                 (let [schema-result (<! (d/transact! conn [{:db/ident :test/name
                                                             :db/valueType :db.type/string
                                                             :db/cardinality :db.cardinality/one}]))]
                   (js/console.log "Schema transacted, max-tx:" (get-in schema-result [:db-after :max-tx]))

                   ;; Step 3b: Transact data
                   (js/console.log "\nStep 3b: Transacting data...")
                   (let [tx-result (<! (d/transact! conn [{:db/id -1 :test/name "Hello Dynamic DB!"}]))]
                     (js/console.log "Transaction result - max-tx:" (get-in tx-result [:db-after :max-tx]))
                     (js/console.log "Datoms added:" (count (:tx-data tx-result)))

                     ;; Step 4: Query the data
                     (js/console.log "\nStep 4: Querying data...")
                     (let [db (d/db conn)
                           results (d/q '[:find ?name :where [_ :test/name ?name]] db)]
                       (js/console.log "Query results:" (pr-str results))

                       ;; Step 5: Delete database
                       (js/console.log "\nStep 5: Deleting database...")
                       (let [delete-result (<! (d/delete-database create-config))]
                         (js/console.log "Delete result:" (pr-str delete-result))

                         (js/console.log "\n=== Test Complete ===")
                         {:create create-result
                          :transact tx-result
                          :query results
                          :delete delete-result})))))))
           (catch :default e
             (js/console.error "Test error:" e)
             e))))))
