(ns is.simm.model.drives
  "Drives — per-room-attachable file systems (doc/archive/file-system-design.md).

   A drive is a dedicated system type in the shared registry
   (:system/type :fs), mirroring KBs: a registry row in the system DB,
   its own Datahike database holding the fs.node TREE, registered as an
   yggdrasil system (fork/merge/sync conformance for free), attached to
   rooms via :grant/* rows. File CONTENT lives in the content-addressed
   blob store (is.simm.model.blobs); the tree references blobs by hash.

   Drives hold raw documents; KBs hold derived knowledge. The
   auto-indexer (a later intake step) walks a drive and publishes into
   a KB fork with :derived-from qualified refs {drive, node}.

   API-FIRST: all consumers (agent fs_* tools, web panel, intake
   indexer) go through ls / read-file / put-file! / mkdir! / mv! / rm!
   — the datahike fs.node schema is an implementation detail, so a
   CAS-native merkle tree can replace it later as a custom yggdrasil
   system without consumer rewrites."
  (:require [is.simm.runtimes.instance :as instance]
            [dvergr.drive.core :as dcore]
            [is.simm.model.system-db :as system-db]
            [is.simm.model.blobs :as blobs]
            [is.simm.runtimes.branching :as branching]
            [dvergr.substrate.datahike :as sdh]
            [dvergr.system.db :as sdb]
            [datahike.api :as d]
            [clojure.string :as str]
            [taoensso.telemere :as log]))

;; =============================================================================
;; Datahike DB lifecycle (the tree database)
;; =============================================================================

(def fs-node-schema
  "dvergr owns the fs.node tree schema; simmis carried a byte-identical copy
   for months. Aliased so a dvergr change cannot silently diverge here."
  dcore/fs-node-schema)

(defn- drive-datahike-cfg
  "Same branching-capable config as KBs — that IS the yggdrasil
   conformance (d/branch!, branch-as-db, merge-db all work)."
  [db-scope]
  {:store {:backend :file
           :path (instance/data-path "simmis-drives" (str db-scope))
           :id db-scope}
   :keep-history? true
   :branch-history? true
   :schema-flexibility :write
   :crypto-hash? true          ; tamper-evident merkle audit chain (auditability)
   :allow-unsafe-config true})

(defn create-drive-database!
  [db-scope]
  (let [cfg (drive-datahike-cfg db-scope)]
    (when-not (d/database-exists? cfg)
      (let [conn (sdh/provision! {:cfg cfg :extra-schema fs-node-schema
                                  :register? false})]
        (branching/register-kb-conn! conn db-scope)
        (log/log! {:level :info :id ::drive-database-created
                   :data {:db-scope db-scope}})
        conn))))

(defn connect-drive-database
  [db-scope]
  (let [cfg (drive-datahike-cfg db-scope)]
    (when (d/database-exists? cfg)
      (let [conn (sdh/provision! {:cfg cfg :schema? false
                                  :extra-schema fs-node-schema :register? false})]
        (branching/register-kb-conn! conn db-scope)
        conn))))

;; =============================================================================
;; Drive CRUD (registry rows in the system DB)
;; =============================================================================

(defn create-drive!
  "Create a drive: registry row (:system/type :fs), tree DB, yggdrasil
   registration. Returns the drive map."
  [owner-id name]
  (when-let [conn (system-db/get-conn)]
    (let [drive-id (random-uuid)
          db-scope (random-uuid)
          sys-id (sdb/register-system! {:type :fs :name name
                                        ;; PATH, not a bare uuid: dvergr reads
                                        ;; `:system/scope` as a filesystem path
                                        ;; (`register-system-into-current!`), so a
                                        ;; uuid here makes it open a store at a
                                        ;; relative path named after the uuid. It
                                        ;; has bitten `:kb` already; `:fs` is not
                                        ;; registered by dvergr today, so this is
                                        ;; latent — keep it correct anyway.
                                        :scope (instance/data-path "simmis-drives" (str db-scope))
                                        :owner-id owner-id})
          drive {:drive/id drive-id
                 :drive/name name
                 :drive/owner owner-id
                 :drive/created (java.util.Date.)
                 :drive/db-scope db-scope
                 :drive/system-id sys-id}]
      (d/transact conn [drive])
      (create-drive-database! db-scope)
      (log/log! {:level :info :id ::drive-created
                 :data {:drive-id drive-id :name name}})
      drive)))

(defn get-drive [drive-id]
  (when-let [conn (and (some? drive-id) (system-db/get-conn))]
    (when-let [e (d/q '[:find (pull ?d [*]) . :in $ ?id
                        :where [?d :drive/id ?id]]
                      @conn drive-id)]
      (dissoc e :db/id))))

(defn list-drives
  "All drives owned by `owner-id` (nil ⇒ all drives)."
  [& [owner-id]]
  (when-let [conn (system-db/get-conn)]
    (->> (if owner-id
           (d/q '[:find [(pull ?d [*]) ...] :in $ ?o
                  :where [?d :drive/owner ?o]] @conn owner-id)
           (d/q '[:find [(pull ?d [*]) ...]
                  :where [?d :drive/id _]] @conn))
         (mapv #(dissoc % :db/id)))))

;; =============================================================================
;; Room attachment (grants — same seam as KBs, eacl extends it)
;; =============================================================================

(defn attach-drive-to-room!
  ([room-id drive-id] (attach-drive-to-room! room-id drive-id :read-write))
  ([room-id drive-id permission]
   (when (system-db/get-conn)
     (when-let [sys-id (:drive/system-id (get-drive drive-id))]
       (sdb/attach! room-id sys-id permission)
       (log/log! {:level :info :id ::drive-attached
                  :data {:room-id room-id :drive-id drive-id}})))))

(defn detach-drive-from-room!
  [room-id drive-id]
  (when (system-db/get-conn)
    (when-let [sys-id (:drive/system-id (get-drive drive-id))]
      (sdb/detach! room-id sys-id))))

(declare get-room-drives attach-drive-to-room!)

(defn ensure-room-drive!
  "Room's primary drive, provisioning + attaching '<Room name> Drive'
   on first need. Returns the drive map or nil.

   The drive is OWNED BY THE ROOM'S OWNER (falling back to its creator), never
   by whoever triggered the provisioning: `load-room-drive!` used to pass the
   caller, so the first member to open the Files tab owned the room's files —
   could attach them to a room of their own, detach them, and keep that
   authority after leaving. Drives provisioned that way are rewritten to the
   room's owner once, at boot (`migrate-room-provisioned-drive-owners!`)."
  [room-id & {:keys [room-name]}]
  (or (first (get-room-drives room-id))
      (when-let [owner (when-let [conn (and (some? room-id) (system-db/get-conn))]
                         (or (d/q '[:find ?o . :in $ ?rid :where
                                    [?r :room/id ?rid] [?r :room/owner ?oe] [?oe :party/id ?o]]
                                  @conn room-id)
                             (d/q '[:find ?o . :in $ ?rid :where
                                    [?r :room/id ?rid] [?r :room/created-by ?o]]
                                  @conn room-id)))]
        (let [drive (create-drive! owner (str (or room-name "Room") " Drive"))]
          (attach-drive-to-room! room-id (:drive/id drive))
          drive))))

(defn- legacy-room-drive-fixes
  "`[{:drive-eid :drive-id :name :from :to :room}]` for every drive that
   `ensure-room-drive!` provisioned for a room but recorded as owned by someone
   other than that room's owner — the caller who first opened the room's Files
   tab, before P1. Recognised by the provisioning's fingerprint: named
   \"<room name> Drive\" or \"Room Drive\", and FIRST granted to that room
   (provisioning attaches it in the same call). A room without an owner, or a
   drive already owned by it, needs nothing."
  [db]
  (->> (d/q '[:find ?d ?did ?name ?owner ?sid :where
              [?d :drive/id ?did] [?d :drive/name ?name]
              [?d :drive/owner ?owner] [?d :drive/system-id ?sid]]
            db)
       (keep (fn [[d did nm owner sid]]
               (let [[_ room-eid] (->> (d/q '[:find ?t ?r :in $ ?sid
                                              :where [?s :system/id ?sid] [?g :grant/system ?s]
                                              [?g :grant/room ?r] [?g :grant/created ?t]]
                                            db sid)
                                       (sort-by first)
                                       first)]
                 (when room-eid
                   (let [{room-name :room/name room-id :room/id room-owner :room/owner}
                         (d/pull db [:room/name :room/id {:room/owner [:party/id]}] room-eid)
                         to (:party/id room-owner)]
                     (when (and to (not= to owner)
                                (contains? #{(str room-name " Drive") "Room Drive"} nm))
                       {:drive-eid d :drive-id did :system-id sid :name nm
                        :from owner :to to :room room-id}))))))
       vec))

(defn migrate-room-provisioned-drive-owners!
  "One-off, idempotent migration run at server boot: a room's auto-provisioned
   drive that records the wrong owner (`legacy-room-drive-fixes`) is rewritten
   to the room's owner — `:drive/owner`, and the registry's `:system/owner`.
   Authorization reads `:drive/owner` alone; until this ran, access.clj carried
   a name heuristic to answer for these drives. Returns the fixes applied
   (none on a second run) and logs each."
  ([] (some-> (system-db/get-conn) migrate-room-provisioned-drive-owners!))
  ([conn]
   (let [fixes (legacy-room-drive-fixes @conn)]
     (when (seq fixes)
       (d/transact conn
                   (vec (mapcat (fn [{:keys [drive-eid system-id to]}]
                                  [[:db/add drive-eid :drive/owner to]
                                   {:system/id system-id :system/owner [:party/id to]}])
                                fixes)))
       (doseq [{:keys [drive-id name from to room]} fixes]
         (log/log! {:level :info :id ::drive-owner-migrated
                    :msg "Room drive re-owned by the room's owner"
                    :data {:drive-id drive-id :name name :room-id room
                           :from-owner from :to-owner to}})))
     fixes)))

(defn get-room-drives
  "Drives attached to a room, each with :drive/permission from the grant."
  [room-id]
  (when-let [conn (and (some? room-id) (system-db/get-conn))]
    (let [rows (d/q '[:find ?did ?perm
                      :in $ ?rid
                      :where
                      [?r :room/id ?rid] [?g :grant/room ?r]
                      [?g :grant/system ?s] [?g :grant/permission ?perm]
                      [?s :system/id ?sid]
                      [?d :drive/system-id ?sid] [?d :drive/id ?did]]
                    @conn room-id)]
      (when (seq rows)
        (vec (keep (fn [[did perm]]
                     (some-> (get-drive did) (assoc :drive/permission perm)))
                   rows))))))

;; =============================================================================
;; The drive API — the contract every consumer programs against
;; =============================================================================

;; =============================================================================
;; Tree ops — DELEGATED to dvergr.drive.core (the substrate lifted into dvergr;
;; identical fs.node schema + CAS blob keying, verified 2026-07-05). simmis
;; keeps only the registry layer above (named drives, ownership, grants,
;; attach/detach) and hands conns to the shared core.
;; =============================================================================

;; Room-scoped convenience. `room-conn` returns the datahike CONN of the room's
;; drive tree (dvergr's :drive-conn-fn resolver, installed in runtimes/web,
;; routes it back onto OUR registry drives) — the same conn the /drive mount
;; uses, so a file written through it is the file the agent's shell sees.
;; Distinct from `ensure-room-drive!` above, which returns the drive MAP.
(def room-conn      dcore/ensure-room-drive!)
(def store-in-room! dcore/store-in-room!)

(def ls           dcore/ls)
(def mkdir!       dcore/mkdir!)
(def ensure-path! dcore/ensure-path!)
(def put-file!    dcore/put-file!)
(def link-blob!   dcore/link-blob!)
(def stat         dcore/stat)
(def read-file    dcore/read-file)
(def resolve-path dcore/resolve-path)
(def mv!          dcore/mv!)
(def rm!          dcore/rm!)
(def tree         dcore/tree)
