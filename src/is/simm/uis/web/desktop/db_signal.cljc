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
                     [clojure.core.async :refer [go <! alts! put! chan close! sliding-buffer promise-chan] :include-macros true]
                     [org.replikativ.spindel.engine.core :as rtc]
                     [org.replikativ.spindel.signal :refer [->SignalRef]]
                     [is.simm.uis.web.desktop.runtime :refer [runtime]]
                     [is.simm.uis.web.desktop.connection-epoch :as epoch]
                     [is.simm.uis.web.desktop.replica-io :as replica-io]
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
           "Atom holding the shared DB's replica conn: for reads and watches.
            NEVER transact on it (nor on any replica conn here): its writer is
            datahike's KabelWriter. Raw database RPCs are explicitly denied;
            they are not application writes and cannot enforce room operations.
            Writes are authorized application RPCs; see `optimistic-write`."
           (atom nil)))

#?(:cljs (def local-overlay
           "Atom holding the explicit optimistic overlay for the shared DB."
           (atom nil)))

#?(:cljs
   (declare room-states room-connecting kb-conns kb-overlays kb-db-signals
            kb-connecting kb-roster kb-heads replica-sync authorized-roster
            ensure-kb-db-signal! evict-view-db-for-scope! note-kb-head! update-signals! prepare-store))

#?(:cljs
   (defn replica-config [kind scope kabel-peer]
     (let [scope-id (if (uuid? scope) scope (uuid (str scope)))
           name (case kind :system "simmis-client"
                           :room (str "simmis-room-" scope)
                           :kb (str "simmis-kb-" scope))]
       {:store {:backend :tiered
                :frontend-config {:backend :memory :id scope-id}
                :backend-config {:backend :indexeddb :name name :id scope-id}
                :id scope-id}
        :writer {:backend :kabel :peer-id server-peer-id :local-peer kabel-peer}
        :branch :db :schema-flexibility :write :keep-history? true})))

#?(:cljs
   (defn- replica-key [kind scope]
     (if (= kind :system) [:system] [kind (str scope)])))

#?(:cljs
   (defn- canonical? []
     (identical? (rtc/home-context runtime)
                 (rtc/home-context (or rtc/*execution-context* runtime)))))

#?(:cljs
   (defn- live-replica [kind scope]
     (binding [rtc/*execution-context* runtime]
       (case kind :system @local-conn
                  :room (get-in @room-states [(str scope) :conn])
                  :kb (get @kb-conns (str scope))))))

#?(:cljs
   (defn- forget-replica! [kind scope {:keys [conn] :as handle}]
     (replica-io/close-overlay! handle)
     (binding [rtc/*execution-context* runtime]
       (when (identical? conn (live-replica kind scope))
         (case kind
           :system (do (reset! local-conn nil) (reset! local-overlay nil)
                       (reset! local-db nil) (reset! local-db-signal nil))
           :room (swap! room-states dissoc (str scope))
           :kb (let [scope (str scope)]
                 (swap! kb-conns dissoc scope)
                 (swap! kb-overlays dissoc scope)
                 (when-let [s (get @kb-db-signals scope)] (reset! s nil))
                 (swap! kb-db-signals dissoc scope)
                 (swap! kb-roster disj scope)
                 (swap! kb-heads dissoc scope)
                 (evict-view-db-for-scope! scope)))
         (let [key (replica-key kind scope)]
           (swap! replica-sync #(if (identical? conn (get-in % [key :conn])) (dissoc % key) %)))))))

#?(:cljs
   (defn- publish-replica! [kind scope {:keys [conn] :as handle} current?]
     (binding [rtc/*execution-context* runtime]
       (when (current?)
         (let [overlay (replica-io/overlay! handle)
               scope (str scope)
               live? #(identical? conn (live-replica kind scope))]
           (opt/listen! overlay ::replica-state
                        (fn [{:keys [db-after revision base-max-tx]}]
                          (binding [rtc/*execution-context* runtime]
                            (when (live?)
                              (case kind
                                :system (update-signals! db-after live?)
                                :room (swap! room-states update scope assoc :db db-after :rev revision)
                                :kb (do (reset! (ensure-kb-db-signal! scope) db-after)
                                        (when (live?) (swap! kb-heads assoc scope base-max-tx))))))))
           (opt/listen-status! overlay ::replica-status
                               (fn [{:keys [status error]}]
                                 (when (live?)
                                   (case status
                                     :rejected (js/console.error "[replica] Optimistic write rejected:" error)
                                     :reconciliation-stalled (js/console.warn "[replica] Committed write is waiting for sync")
                                     nil))))
           (case kind
             :system (do (reset! local-conn conn)
                         (when (current?) (reset! local-overlay overlay))
                         (when (current?) (update-signals! (opt/db overlay) live?)))
             :room (swap! room-states assoc scope {:conn conn :overlay overlay :db (opt/db overlay) :rev 0})
             :kb (do
                   (swap! kb-conns assoc scope conn)
                   (when (current?) (swap! kb-overlays assoc scope overlay))
                   (when (current?) (reset! (ensure-kb-db-signal! scope) (opt/db overlay)))
                   (when (current?) (note-kb-head! scope @conn))
                   (when (current?) (swap! kb-roster conj scope)))))))))

#?(:cljs
   (defn- open-replica! [kind scope kabel-peer connection generation valid?]
     (binding [rtc/*execution-context* runtime]
       (swap! replica-sync assoc (replica-key kind scope) {:connection connection :status :pending}))
     (replica-io/open!
      (replica-config kind scope kabel-peer)
      {:generation generation :valid? valid?
       :prepare! (when-not (= kind :system) #(prepare-store (str scope)))
       :publish! #(publish-replica! kind scope %1 %2)
       :unpublish! #(forget-replica! kind scope %)})))

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
     "The shared DB's replica conn (reads and watches only — see
      `local-conn`). Returns nil if not yet initialized."
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
     [db current?]
     (binding [rtc/*execution-context* runtime]
       (when (current?) (reset! local-db db))
       (when (current?)
         (reset! local-db-signal {:max-tx (:max-tx db)
                                 :timestamp (js/Date.now)})))))

#?(:cljs
   (defn refresh-db!
     "Refresh the local-db signal with the current database state.
      Call this after local transactions to ensure UI updates immediately."
     []
     (when-let [overlay @local-overlay]
       (let [conn @local-conn]
         (update-signals! (opt/db overlay) #(and conn (identical? conn @local-conn)))))))

;; ============================================================================
;; Connection Initialization
;; ============================================================================

#?(:cljs
   (defn init-reactive-db!
     "Bootstrap the shared replica through a physical store owner. If the SAME
      peer authenticates a new epoch during initial sync, follow that desired
      generation; never wipe IndexedDB or overlap the older constructor."
     [kabel-peer]
     (let [initial (epoch/snapshot) session-id (:session-id initial)
           changes (chan (sliding-buffer 1)) key (random-uuid)
           request! (fn [connection]
                      (open-replica! :system simmis-store-id kabel-peer connection (:epoch connection)
                                     #(epoch/current? connection)))]
       (binding [rtc/*execution-context* runtime]
         (add-watch epoch/connection key
                    (fn [_ _ _ connection]
                      (when (canonical?) (put! changes {:connection connection})))))
       (go
         (try
           (if-not (:epoch initial)
             (ex-info "Shared replica bootstrap requires authentication" {})
             (loop [connection initial result (request! initial)]
               (let [[value port] (alts! [changes result] :priority true)
                     next-connection (if (= port changes) (:connection value) (epoch/snapshot))]
                 (cond
                   (or (nil? next-connection) (not= session-id (:session-id next-connection)))
                   (ex-info "Shared replica bootstrap belongs to a retired session" {})

                   (not= connection next-connection)
                   (recur next-connection (request! next-connection))

                   (= port changes)
                   (recur connection result)

                   (and (= :ready (:status value)) (epoch/current? connection)
                        (identical? (get-in value [:resource :conn]) @local-conn))
                   (do
                     (binding [rtc/*execution-context* runtime]
                       (swap! replica-sync assoc [:system]
                              {:connection connection :conn @local-conn :status :ready}))
                     :ready)

                   :else
                   (let [error (if (instance? js/Error value) value
                                   (ex-info "Shared replica did not confirm readiness" {}))]
                     (binding [rtc/*execution-context* runtime]
                       (swap! replica-sync assoc [:system]
                              {:connection connection :status :error :error (ex-message error)}))
                     error)))))
           (finally
             (binding [rtc/*execution-context* runtime] (remove-watch epoch/connection key))
             (close! changes)))))))

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
#?(:cljs (defonce room-connecting (atom {})))

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
   (defn- connect-scope! [kind scope-uuid kabel-peer]
     (let [scope (str scope-uuid) connection (epoch/snapshot)
           attempts (case kind :room room-connecting :kb kb-connecting)
           token (random-uuid)
           current? #(and (epoch/current? connection) (= token (get-in @attempts [scope :token])))]
       (when (and (canonical?) (:epoch connection)
                  (= connection (:connection @authorized-roster))
                  (contains? (get @authorized-roster (case kind :room :rooms :kb :kbs)) scope)
                  (not (live-replica kind scope)) (not (contains? @attempts scope)))
         (swap! attempts assoc scope {:token token :connection connection :phase :pending})
         (let [result (open-replica! kind scope kabel-peer connection token current?)]
           (go
             (let [value (<! result)]
               (binding [rtc/*execution-context* runtime]
                 (when (= token (get-in @attempts [scope :token]))
                   (let [conn (get-in value [:resource :conn]) ready? (and (current?) (= :ready (:status value))
                                                                         (identical? conn (live-replica kind scope)))]
                     (swap! attempts assoc-in [scope :phase] (if ready? :ready :error))
                     (when (epoch/current? connection)
                       (swap! replica-sync assoc (replica-key kind scope)
                              (if ready?
                                {:connection connection :conn conn :status :ready}
                                {:connection connection :status :error
                                 :error (or (ex-message value) "Replica open was superseded")})))))))))))))

#?(:cljs
   (defn connect-room!
     "Lazy, idempotent open. Per-store ownership fences stale constructors and
      serializes physical retirement before a replacement can open."
     [scope-uuid kabel-peer]
     (connect-scope! :room scope-uuid kabel-peer)))

#?(:cljs
   (defn get-room-db
     "Get the current DB value for a room scope from the room-states signal.
      Returns nil if not connected."
     [scope-uuid]
     (get-in (binding [rtc/*execution-context* runtime] @room-states)
             [(str scope-uuid) :db])))

#?(:cljs
   (defn disconnect-room!
     "Withdraw this scope immediately; its slot retains native ownership until
      drain completes. A queued replacement cannot overtake that retirement."
     [scope-uuid]
     (let [scope (str scope-uuid)
           old (get (binding [rtc/*execution-context* runtime] @room-states) scope)]
       (swap! room-connecting dissoc scope)
       (let [drain (replica-io/retire! (replica-config :room scope nil))]
         (when old (forget-replica! :room scope
                                   {:conn (:conn old) :overlay (atom (:overlay old))}))
         drain))))

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
#?(:cljs (defonce ^:private view-db-done (atom {}))) ; request ownership, including completed projections

#?(:cljs (def replica-sync
           "Transport authentication is not data readiness. Entries identify
            the exact live conn and epoch whose fresh head has been confirmed."
           (signal runtime {})))
#?(:cljs (def ^:private authorized-roster (signal runtime nil)))

#?(:cljs
   (defn evict-view-db-for-scope!
     "Drop all (scope, ref) view-db signals for `scope` (on tab close)."
     [scope]
     (let [sk (str scope)
           stale (into {} (filter (fn [[[s _] _]] (= s sk))) @view-db-signals)]
       (when (seq stale)
         ;; Remove ownership BEFORE notifying consumers. A synchronous render
         ;; may create a replacement, which this eviction must never clear.
         (swap! view-db-signals #(apply dissoc % (keys stale)))
         (swap! view-db-done #(apply dissoc % (keys stale)))
         (doseq [[_ s] stale] (reset! s nil))))))

#?(:cljs
   (defn ensure-view-db-signal!
     "One projection per (scope, ref). Epoch, connection, request and signal
      identity fence asynchronous branch reads. Eviction or reconnect cannot
      let an old result refill a replacement projection. Now reads stay local."
     [scope ref]
     (let [sk (str scope)
           branch (get (:branches ref) sk)
           live #(if scope (ensure-kb-db-signal! scope) local-db)]
       (if (or (nil? ref) (= ref :now)
               (and (not (:as-of ref)) (not (:forkset ref)))
               (and (:forkset ref) (not branch)))
         (live)
         (let [k [sk ref]
               stale (into {} (filter (fn [[[s r] _]] (and (= s sk) (not= r ref)))) @view-db-signals)
               _ (when (seq stale)
                   (swap! view-db-signals #(apply dissoc % (keys stale)))
                   (swap! view-db-done #(apply dissoc % (keys stale)))
                   (doseq [[_ s] stale] (reset! s nil)))
               s (or (get @view-db-signals k)
                     (let [new-signal (binding [rtc/*execution-context* runtime] (signal runtime nil))]
                       (get (swap! view-db-signals
                                   #(if (contains? % k) % (assoc % k new-signal))) k)))
               live-conn #(if scope (get @kb-conns sk) @local-conn)
               conn (live-conn)
               connection (epoch/snapshot)
               prior (get @view-db-done k)]
           (when (and conn connection
                      (not (and (= connection (:connection prior)) (identical? conn (:conn prior)))))
             (let [token (random-uuid)
                   current? #(and (epoch/current? connection) (identical? conn (live-conn))
                                  (identical? s (get @view-db-signals k))
                                  (= token (:token (get @view-db-done k))))
                   release! #(swap! view-db-done
                                    (fn [m] (if (= token (:token (get m k))) (dissoc m k) m)))]
               (swap! view-db-done assoc k {:token token :connection connection :conn conn})
               ;; Never fill a tracked signal synchronously inside a render.
               (go
                 (try
                   (let [value (if (:forkset ref)
                                 (<! (dv/branch-as-db conn (keyword branch) {:sync? false}))
                                 (d/as-of @conn (:as-of ref)))]
                     (when (instance? js/Error value) (throw value))
                     (binding [rtc/*execution-context* runtime]
                       (when (current?)
                         (if value (reset! s value) (release!)))))
                   (catch :default error
                     (binding [rtc/*execution-context* runtime]
                       (when (current?)
                         (release!)
                         (js/console.warn "[view-db] projection failed:" sk ref error))))))))
           s)))))

#?(:cljs
   (defn kb-db-signal
     "Return the per-KB db signal for `scope` if it exists, else nil.
     Non-creating — use `ensure-kb-db-signal!` when you want lazy
     materialisation."
     [scope]
     (get @kb-db-signals (str scope))))

;; Atom for idempotent connection tracking (prevents double-connect)
#?(:cljs (defonce kb-connecting (atom {})))

#?(:cljs
   (defn connect-kb!
     "Lazily open a source-selected KB under its physical store owner."
     [scope-uuid kabel-peer]
     (connect-scope! :kb scope-uuid kabel-peer)))

#?(:cljs
   (defn get-kb-db
     "Get the current DB value for a KB scope from its per-KB signal.
      Returns nil if not connected."
     [scope-uuid]
     (when-let [sig (kb-db-signal scope-uuid)]
       (binding [rtc/*execution-context* runtime] @sig))))

#?(:cljs
   (defn disconnect-kb!
     "Hide projections immediately while owned physical cleanup drains."
     [scope-uuid]
     (let [scope (str scope-uuid) conn (get @kb-conns scope)
           overlay (get @kb-overlays scope)]
       (swap! kb-connecting dissoc scope)
       (let [drain (replica-io/retire! (replica-config :kb scope nil))]
         (when conn (forget-replica! :kb scope {:conn conn :overlay (atom overlay)}))
         drain))))

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

#?(:cljs
   (defn clear-session!
     "New peer/session retirement, not SAME-peer reconnect. Hide all old account
      projections; native slots retain exact resources until disposal is joined."
     []
     (let [drains (replica-io/retire-all!)]
       (reset! room-connecting {})
       (reset! kb-connecting {})
       (binding [rtc/*execution-context* runtime]
         (doseq [[scope value] @room-states]
           (forget-replica! :room scope {:conn (:conn value) :overlay (atom (:overlay value))}))
         (doseq [[scope conn] @kb-conns]
           (forget-replica! :kb scope {:conn conn :overlay (atom (get @kb-overlays scope))}))
         (when-let [conn @local-conn]
           (forget-replica! :system simmis-store-id {:conn conn :overlay (atom @local-overlay)}))
         (doseq [[_ s] @view-db-signals] (reset! s nil))
         (reset! authorized-roster nil)
         (reset! replica-sync {}))
       (reset! view-db-signals {})
       (reset! view-db-done {})
       drains)))

#?(:cljs
   (defn resync-open!
     "After an authoritative roster read, admit requested views and reauthorize
      already-open replicas; never eagerly open the whole authorized roster.
      Preserve exact connections/overlays on SAME-peer reconnect; a new peer must
      first clear-session!. No cache wipe, write retry, or eager wiki connection."
     [connection roster kabel-peer]
     (when (epoch/current? connection)
       (binding [rtc/*execution-context* runtime]
         (let [rooms (into #{} (keep #(some-> (:room/content-db-scope %) str)) (:rooms roster))
               kbs (into #{} (keep #(some-> (:kb/db-scope %) str)) (:knowledge-bases roster))
               pending-rooms (keep (fn [[scope attempt]] (when (not= connection (:connection attempt)) scope)) @room-connecting)
               ;; A viewer may create its nil DB signal before roster admission.
               ;; That is requested work, even though connect-scope! correctly
               ;; declined to start an unauthorized constructor at the time.
               ;; Admit only requested scopes, preserving current pending/error
               ;; attempts and never eagerly opening the whole authorized roster.
               pending-kbs (distinct
                             (concat
                               (keep (fn [[scope attempt]] (when (not= connection (:connection attempt)) scope)) @kb-connecting)
                               (remove #(or (contains? @kb-connecting %) (live-replica :kb %))
                                       (keys @kb-db-signals))))]
           (reset! authorized-roster {:connection connection :rooms rooms :kbs kbs})
           (doseq [scope (keys @room-connecting) :when (and (epoch/current? connection) (not (contains? rooms scope)))]
             (disconnect-room! scope))
           (doseq [scope (keys @kb-connecting) :when (and (epoch/current? connection) (not (contains? kbs scope)))]
             (disconnect-kb! scope))
           (doseq [scope (keys @room-states) :when (and (epoch/current? connection) (not (contains? rooms scope)))]
             (disconnect-room! scope))
           (doseq [scope (keys @kb-conns) :when (and (epoch/current? connection) (not (contains? kbs scope)))]
             (disconnect-kb! scope))
           ;; An open begun on a lost epoch must not hold its latch forever.
           ;; Restart only already-requested scopes authorized by THIS roster.
           (doseq [scope pending-rooms :when (and (epoch/current? connection) (contains? rooms scope))]
             (swap! room-connecting dissoc scope)
             (connect-room! scope kabel-peer))
           (doseq [scope pending-kbs :when (and (epoch/current? connection) (contains? kbs scope))]
             (swap! kb-connecting dissoc scope)
             (connect-kb! scope kabel-peer))
           (let [entries (concat (when-let [conn @local-conn] [[[:system] conn]])
                                 (map (fn [[scope s]] [[:room scope] (:conn s)]) @room-states)
                                 (map (fn [[scope conn]] [[:kb scope] conn]) @kb-conns))]
             (doseq [[key conn] entries]
               (let [[kind scope] key
                     live-conn #(case kind :system @local-conn
                                             :room (get-in @room-states [scope :conn])
                                             :kb (get @kb-conns scope))
                     current? #(and (epoch/current? connection) (identical? conn (live-conn))
                                    (= connection (:connection @authorized-roster))
                                    (case kind :system true
                                               :room (contains? (:rooms @authorized-roster) scope)
                                               :kb (contains? (:kbs @authorized-roster) scope)))
                     prior (get @replica-sync key)]
                 (when (and (current?)
                            (not (and (= connection (:connection prior)) (identical? conn (:conn prior)))))
                   (swap! replica-sync assoc key {:connection connection :conn conn :status :pending})
                   (go
                     (try
                       (let [prepared (if (= kind :system) {:ready? true} (<! (prepare-store scope)))]
                         (when-let [error (:error prepared)] (throw error))
                         (when-not (identical? kabel-peer (:local-peer (:writer @conn)))
                           (throw (ex-info "Replica belongs to a different peer; retire it before session replacement" {:scope scope})))
                         (when (binding [rtc/*execution-context* runtime] (current?))
                           (let [result (<! (kc/resubscribe! conn {:epoch (:epoch connection)}))]
                             (binding [rtc/*execution-context* runtime]
                               (when (current?)
                                 (if (= :ready (:status result))
                                   (do
                                     (swap! replica-sync assoc key {:connection connection :conn conn :status :ready})
                                     (when (and (current?) (= kind :kb))
                                       ;; Mutable future projections must read the
                                       ;; now-confirmed branch heads again.
                                       (doseq [[[sk ref :as k] s] @view-db-signals
                                               :when (and (= sk scope) (:forkset ref))]
                                         (when (and (current?) (identical? s (get @view-db-signals k)))
                                           (swap! view-db-done dissoc k)
                                           (when (and (current?) (identical? s (get @view-db-signals k)))
                                             (reset! s nil))))))
                                   (throw (if (instance? js/Error result) result
                                              (ex-info "Replica catch-up did not confirm readiness" {:result result})))))))))
                       (catch :default error
                         (binding [rtc/*execution-context* runtime]
                           (when (current?)
                             (swap! replica-sync assoc key {:connection connection :conn conn :status :error :error error})
                             (js/console.error "[DB-SIGNAL] Replica catch-up failed:" key error)))))))))))))))

