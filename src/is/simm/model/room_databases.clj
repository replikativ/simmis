(ns is.simm.model.room-databases
  "Per-room Datahike database lifecycle.

   Since the per-room store collapse a room has exactly ONE database: dvergr's
   room (messages) store. simmis no longer creates or opens a store of its own;
   it resolves dvergr's conn and installs its own half via
   `is.simm.model.store/ensure!` — the ONE definition of what a simmis store
   contains, shared with user-created KBs — into that same store. Everything a room
   holds (conversation, wiki, code, books) therefore shares one transaction
   log, one audit chain, and one sync scope, which is what makes a room
   queryable as a whole.

   `:room/content-db-scope` survives as the attribute name so access control,
   client sync and every existing call site keep working; it is now bound to
   `dvergr.system.rooms/room-msgs-store-id` rather than a scope simmis minted.

   Conns come from dvergr rather than `d/connect` for two reasons: it is the
   FORK-AWARE conn (a room running on a forked ctx must not be written through
   a conn pinned to the trunk), and konserve-sync registers at the STORE level,
   so the registered conn has to be the very one the agent writes through."
  (:require [is.simm.model.schema :as schema]
            [is.simm.model.system-db :as system-db]
            [is.simm.model.store :as store]
            [is.simm.runtimes.branching :as branching]
            [dvergr.system.rooms :as drooms]
            [org.replikativ.spindel.engine.core :as rtc]
            [datahike.api :as d]
            [taoensso.telemere :as log]))

;; =============================================================================
;; Resolving the room store
;; =============================================================================

(defn- room-id-for-scope
  "The room whose store is `db-scope`. The mapping lives in the system DB
   because a store id is a hash of the store's path and so cannot be inverted."
  [db-scope]
  (when-let [conn (system-db/get-conn)]
    (d/q '[:find ?rid .
           :in $ ?scope
           :where [?r :room/content-db-scope ?scope] [?r :room/id ?rid]]
         @conn db-scope)))

(defn room-store-conn
  "dvergr's fork-aware conn to the store behind `db-scope`, or nil when the room
   is unknown or not hydrated in this process.

   Resolved on the ROOM's own execution context, not the server's. A dvergr room
   runs on a fork of the daemon root whose composite holds only that room's
   systems, and yggdrasil's registry is context-backed — so looking the store up
   from the server context finds nothing and silently returns nil. No room ctx
   means the room is not hydrated here, and there is no conn to hand out."
  [db-scope]
  (when-let [room-id (room-id-for-scope db-scope)]
    (when-let [room-ctx (drooms/room-ctx-for room-id)]
      (binding [rtc/*execution-context* room-ctx]
        (drooms/room-msgs-conn room-id)))))

;; =============================================================================
;; Lifecycle
;; =============================================================================

(defn connect-room-database
  "The conn for a room's database, with simmis's half guaranteed installed.

   There is no create/connect split any more: dvergr owns store creation, so
   every path into a room store goes through here and gets the same guarantee.
   The install runs once per process per room; the conn is dvergr's, so there is
   no connection cache to keep."
  [db-scope]
  (when-let [conn (room-store-conn db-scope)]
    (let [c (store/ensure! conn db-scope)]
      ;; Register the room store under simmis's OWN yggdrasil name on the SERVER
      ;; context, so it branches exactly the way a KB does. dvergr already
      ;; registers this same store as `room-msgs-<name>` — but on the ROOM ctx,
      ;; and every branching path simmis has (proposal forks, semantic diff,
      ;; `accept-fork!`'s merge) resolves a scope through
      ;; `branching/get-kb-system`, which reads `kb:<scope>` off the SERVER ctx
      ;; and finds nothing there. Without this a book fork could be minted by
      ;; nobody and merged by nobody; with it a room store is just another
      ;; branchable scope. Best-effort: a room whose store will not register is
      ;; still a working room, it just cannot carry a fork.
      (try
        (branching/register-system! c db-scope)
        (catch Exception e
          (log/log! {:level :warn :id ::branching-registration-failed
                     :msg "Room store not branchable — proposals cannot fork it"
                     :data {:db-scope (str db-scope) :error (.getMessage e)}})))
      ;; Tell subscribed clients when this room's BOOK moves. Filtered to
      ;; kontor writes inside the listener — a room store also holds messages,
      ;; and an unfiltered listener would publish per chat message.
      (try
        ((requiring-resolve 'is.simm.model.branching-broadcast/install-book-tx-listener!)
         c db-scope)
        (catch Exception e
          (log/log! {:level :warn :id ::book-listener-failed
                     :msg "Accounting will not update live for this room"
                     :data {:db-scope (str db-scope) :error (.getMessage e)}})))
      c)))

(defn ensure-room-database!
  "Make sure a room's store carries simmis's half. Alias of
   `connect-room-database`, kept for call sites that read as provisioning."
  [db-scope]
  (connect-room-database db-scope))

(defn get-room-db-scope
  "Look up the db-scope UUID for a room from the system DB."
  [room-uuid]
  (when-let [conn (system-db/get-conn)]
    (d/q '[:find ?scope .
           :in $ ?rid
           :where [?r :room/id ?rid] [?r :room/content-db-scope ?scope]]
         @conn room-uuid)))

;; =============================================================================
;; Server Registration (konserve-sync)
;; =============================================================================

(defn register-room-for-sync!
  "Register a room's Datahike DB with the kabel server for remote access.
   Idempotent — skips if the topic is already registered on this peer."
  [db-scope server-peer]
  (if (contains? (get-in @server-peer [:pubsub :topics]) db-scope)
    (log/log! {:level :debug
               :id ::room-already-registered
               :data {:db-scope db-scope}})
    (when-let [room-conn (connect-room-database db-scope)]
      (require 'datahike.kabel.handlers)
      ((resolve 'datahike.kabel.handlers/register-store-for-remote-access!)
       db-scope room-conn server-peer {:branches :trunk})
      (log/log! {:level :info
                 :id ::room-registered-for-sync
                 :msg "Room registered for konserve-sync"
                 :data {:db-scope db-scope}}))))

(defn register-party-rooms-for-sync!
  "Register all rooms containing a given party for sync. Called on login / load-rooms!."
  [party-id server-peer]
  (when-let [conn (system-db/get-conn)]
    (let [db-scopes (d/q '[:find [?scope ...]
                           :in $ ?pid
                           :where
                           [?p :party/id ?pid]
                           [?r :room/parties ?p]
                           [?r :room/content-db-scope ?scope]]
                         @conn party-id)]
      (doseq [scope db-scopes]
        (register-room-for-sync! scope server-peer)))))
