(ns is.simm.model.db
  "Database connection and initialization"
  (:require [is.simm.runtimes.instance :as instance]
            [datahike.api :as d]
            [datahike.db :as ddb]
            [is.simm.model.schema :as schema]
            [is.simm.model.seed :as seed]
            [is.simm.model.crud :as crud]
            [is.simm.model.store :as store]
            [dvergr.chat.schema :as dvergr-schema]
            [dvergr.substrate.datahike :as sdh]
            ;; IMPORTANT: context must be required before signal operations
            ;; It creates the server-wide execution context at load time
            [is.simm.runtimes.context :as ctx]
            [org.replikativ.spindel.signal :refer [signal] :as sig]
            [org.replikativ.spindel.incremental.deltaable :as deltaable]
            [taoensso.telemere :as log]
            [clojure.core.async :refer [put!]])
  (:import [datahike.db TxReport]))

;; ============================================================================
;; Extend TxReport with signal protocols for uniform integration
;; ============================================================================

;; PDeltaable: Allows signal system to extract deltas from TxReport
(extend-type TxReport
  deltaable/PDeltaable
  (get-deltas [this]
    ;; Return tx-data as the deltas
    (:tx-data this))
  (deltaable? [_]
    true))

;; NOTE: ISignalDeltaView extension commented out - protocol doesn't exist yet in spindel.
;; TODO: Add ISignalDeltaView protocol to signal.cljc or remove this.
#_(extend-type TxReport
  sig/ISignalDeltaView
  (get-new [this]
    ;; The "new" value is the database after the transaction
    (:db-after this))
  (get-old [this]
    ;; The "old" value is the database before the transaction
    (:db-before this))
  (get-delta [this]
    ;; The delta is the transaction data (datoms that changed)
    (:tx-data this)))

(def cfg
  "The global app store. Same flag set as every other simmis/dvergr datom store
   (`dvergr.system.rooms/store-cfg`, `knowledge-bases/kb-datahike-cfg`) so no
   store is missing a create-time-fixed flag a later use would need.

   `:index-config` and `:commit-graph?` are omitted here and supplied only at
   creation (`create-cfg`) — both are create-time-fixed and datahike ADOPTS them
   from the store when the caller omits them, which is what lets one config
   connect to a store built with either layout."
  {:store {:backend :file
           :path (instance/data-path "simmis-v2")
           :id #uuid "a1b2c3d4-e5f6-7890-abcd-ef1234567890"}
   :schema-flexibility :write     ;; allow dynamic property creation
   :keep-history? true            ;; the product's history (wiki/page revisions)
   :crypto-hash? true})           ;; verifiable via datahike.audit/verify-chain

(def create-cfg
  "`cfg` plus the create-time-fixed index layout. Used only at
   `d/create-database`; see `cfg` for why it must not appear on the connect path."
  (assoc cfg
         :index-config {:diff-buf-size sdh/diff-buf-size}
         :commit-graph? true))

(defn- explain-connect-failure!
  "Re-throw a connect failure with the reason it is USUALLY ours.

   `:now nil` on a version check means the running dependency reports no
   version at all, which is what a SOURCE CHECKOUT does — the version is a
   build-time resource. So this is not a real 'the store is from the future'
   situation; it means the `:local` alias is overlaying a sibling repo, and
   datahike's check reads unknown as older (datahike#902, still open).

   The store is fine. Saying so beats a message that accuses the data."
  [^Exception e]
  (let [{:keys [type stored now]} (ex-data e)]
    (if (and (some-> type name (.endsWith "-version")) (nil? now))
      (throw (ex-info
              (str "Dependency version check failed against a SOURCE CHECKOUT, not a bad store. "
                   "The store wants " stored " and the running library reports no version, "
                   "which is what `:local` gives you. Boot with `-M:stack:dev` (Maven "
                   "dependencies + local dvergr), or put datahike#902 on whatever branch "
                   "`../datahike` is currently on.")
              {:type ::local-overlay-version-check :stored stored :original type} e))
      (throw e))))

(defonce conn
  (let [c (if (d/database-exists? cfg)
            ;; A failure connecting to a store that EXISTS is real — surface it.
            ;; This used to be `(catch Exception _ …create…)`, which read every
            ;; failure as "no database yet": a version mismatch, a held lock or a
            ;; corrupt store all fell through to the create path and reappeared
            ;; there with the actual cause discarded. Cost an hour on 2026-07-26.
            (try (d/connect cfg)
                 (catch Exception e (explain-connect-failure! e)))
            (let [new-conn (sdh/provision! {:cfg create-cfg :extra-schema schema/full-schema
                                            :register? false})]
              ;; Install seed data
              (seed/ensure-seed-data! new-conn)
              new-conn))]
    ;; Ensure dvergr schema is present on existing DBs too (idempotent)
    (dvergr-schema/ensure-full-schema! c)
    ;; Install any new schema attributes added since DB creation (idempotent)
    (schema/install-schema! c)
    ;; Project the shared katzen KB schema into category S (idempotent), so the
    ;; knowledge base's type + properties come from the single katzen definition
    ;; (katzen.schema.knowledge) — katzen is the source of truth, S is a
    ;; generated view. `store/project-kb-schema!` owns the binding and the
    ;; guard, and `store/install!` applies the same one to every other store.
    (store/project-kb-schema! c)
    c))

(defn get-conn []
  conn)

(defn get-db []
  @conn)

;; ============================================================================
;; Reactive Database Signal
;; ============================================================================

(defn- initial-tx-report
  "Create a stub TxReport for signal initialization.
   Contains current db with no prior state or deltas."
  [db]
  (ddb/->TxReport nil db [] {} nil))

;; Central signal holding the current TxReport.
;; TxReport implements IDeltaable, so signal system extracts:
;; - :new (snapshot) = TxReport with db-after
;; - :old (old-snapshot) = previous TxReport
;; - :delta = tx-data (via get-deltas)
;;
;; Consumers can access:
;; - (:db-after tx-report) for the current db
;; - (:db-before tx-report) for the previous db
;; - (:tx-data tx-report) for the transaction deltas
;;
;; IMPORTANT: Signal is created within server context so it's registered
;; in the same execution context that remote tasks use.
(defonce db-signal
  (ctx/with-server-context
    (signal (initial-tx-report @conn))))

(defn transact-and-update-signal!
  "Transact to Datahike and update db-signal with the TxReport.
   The signal update triggers all remote tracking tasks.

   This is the primary way to transact on the server - it ensures
   the db-signal is updated atomically with the transaction."
  [connection tx-data]
  (let [tx-report (d/transact connection tx-data)]
    ;; Store TxReport directly - it implements IDeltaable
    (reset! db-signal tx-report)
    (log/log! {:level :debug
               :id ::db-signal-updated
               :msg "Database signal updated after transaction"
               :data {:tx-data-count (count (:tx-data tx-report))
                      :max-tx (:max-tx (:db-after tx-report))}})
    tx-report))

;; ============================================================================
;; Broadcast System
;; ============================================================================

(defonce server-peer (atom nil))

(defn set-server-peer!
  "Set the server peer for broadcasting. Called from runtime initialization."
  [peer]
  (reset! server-peer peer))

(defn broadcast-pages-list-changed!
  "Broadcast page list change to ALL connected clients.
   Used for page creation/deletion to update page lists everywhere.

   action: :created, :updated, or :deleted
   page: the page data (for :created/:updated) or nil
   page-uuid: the page UUID (required for all actions)"
  [action page-uuid page]
  (when-let [peer @server-peer]
    (let [[bus-in _bus-out] (get-in @peer [:volatile :chans])]
      (log/log! {:level :debug
                 :id ::broadcasting-pages-list-change
                 :msg "Broadcasting pages list change to all clients"
                 :data {:action action :page-uuid page-uuid}})
      ;; Send without target-peer to broadcast to all via the middleware
      (put! bus-in {:type ::page-changed
                    :action action
                    :page page
                    :page-uuid page-uuid
                    :timestamp (System/currentTimeMillis)}))))

(defn setup-signal-watcher!
  "Set up Datahike tx listener to update db-signal.
   This ensures the signal is updated for ANY transaction,
   regardless of whether it goes through transact-and-update-signal!."
  [connection]
  (d/listen connection ::signal-update
    (fn [tx-report]
      ;; Datahike listener runs in async thread - must bind execution context
      (ctx/with-server-context
        (reset! db-signal tx-report)
        (log/log! {:level :debug
                   :id ::db-signal-updated-via-listener
                   :msg "Database signal updated via transaction listener"
                   :data {:tx-data-count (count (:tx-data tx-report))
                          :max-tx (:max-tx (:db-after tx-report))}}))))
  (log/log! {:level :info
             :id ::signal-watcher-setup
             :msg "Database signal watcher set up successfully"}))

;; Keep the app-store signal in sync with every transaction on `conn`.
(setup-signal-watcher! conn)
