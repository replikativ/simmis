(ns is.simm.model.knowledge-bases
  "Knowledge base CRUD against the system database.

   Knowledge bases are user-owned Datahike databases. Each KB has:
   - A unique ID and display name
   - An owner (user who created it)
   - A db-scope UUID for konserve-sync (maps to a Datahike DB)
   - Shared-with list (users who can access)
   - Tags for project grouping

   KBs can be attached to rooms, giving room agents access."
  (:require [is.simm.model.system-db :as system-db]
            [is.simm.model.schema :as schema]
            [is.simm.model.seed :as seed]
            [is.simm.model.store :as store]
            [dvergr.system.rooms :as srooms]
            [is.simm.model.fractional-index :as frac]
            [is.simm.runtimes.context :as ctx]
            [is.simm.runtimes.branching :as branching]
            [dvergr.chat.schema :as dvergr-schema]
            [dvergr.substrate.datahike :as sdh]
            [dvergr.system.db :as sdb]
            [is.simm.model.references :as refs]
            [datahike.api :as d]
            [datahike.reference :as dh-ref]
            [clojure.string :as str]
            [taoensso.telemere :as log]))

(declare declare-wiki-fulltext!)

;; =============================================================================
;; Datahike DB Lifecycle
;; =============================================================================

(defn new-kb-path
  "A fresh filesystem location for a KB store.

   PATH IS CANONICAL and the store id derives from it (`srooms/store-id`) —
   dvergr's rule, adopted here so a KB and a room store are addressed
   identically and either side can register either kind.

   They used to disagree: simmis wrote a KB's `:system/scope` as a bare uuid,
   while dvergr treats `:system/scope` as a PATH. So when dvergr registered a
   granted simmis KB into a room's composite it opened a store at a relative
   path named after the uuid — a different, empty store. That is why a room's
   agent could not see the wiki: `knowledge_search` unions the room's granted
   KBs, but the simmis ones resolved to the wrong place."
  []
  (str "data/simmis-kbs/" (random-uuid)))

(defn kb-store-path
  "Filesystem path of the KB whose store id is `db-scope`.

   Looked up rather than computed: since the id is a hash of the path it cannot
   be inverted. The path is recorded as the KB's `:system/scope`.

   Falls back to the LEGACY layout for a row that predates this — those KBs used
   the db-scope directly as the path component and as the store id, so the path
   is still derivable for them. Without the fallback a KB with no `:system/id`
   yet (the case `ensure-kb-system!` exists to handle) would be unresolvable."
  [db-scope]
  (or (when-let [conn (system-db/get-conn)]
        (d/q '[:find ?scope .
               :in $ ?ds
               :where [?k :kb/db-scope ?ds] [?k :kb/system-id ?sid]
                      [?s :system/id ?sid] [?s :system/scope ?scope]]
             @conn db-scope))
      (str "data/simmis-kbs/" db-scope)))

(defn- kb-datahike-cfg
  "Datahike config for CONNECTING to a KB's database.

   The flags match every other simmis/dvergr datom store — see
   `dvergr.system.rooms/store-cfg`. A KB may grow to hold any combination of a
   room's content, so it must not be missing a create-time-fixed flag some later
   use would need.

   `:keep-history? true` is the PRODUCT's history — wiki edits, page revisions,
   ledger rows. (It is not what the branching API needs: `d/branch!` works off
   commit records, not temporal indices.)

   `:crypto-hash? true` makes the store verifiable end to end via
   `datahike.audit/verify-chain`.

   Deliberately carries NO `:index-config` and no `:commit-graph?`. Both are
   create-time-fixed and are ADOPTED from the store when the caller omits them,
   so omitting here is what lets this one config connect to a store whatever
   layout it was built with. Creation supplies them — see `kb-create-cfg`.

   No `:branch-history?`: there is no such datahike option. It appears in
   datahike only as a FUNCTION name (`datahike.versioning/branch-history`),
   which walks the commit graph — so `:commit-graph? true` is what actually
   provides branch ancestry, and crypto-hash forces that on.

   No `:allow-unsafe-config` either. It was here to let old KBs adopt
   `:branch-history?` without re-creation — i.e. to support a flag that does
   nothing — and it is dangerous: on that path datahike MERGES a supplied
   `:index-config` over the stored one instead of raising, so a changed value
   silently rewrites an existing KB's representation. Losing it means a genuinely
   mismatched config now fails loudly, which is what we want."
  [path db-scope]
  {:store {:backend :file :path path :id db-scope}
   :keep-history? true
   :crypto-hash? true
   :schema-flexibility :write})

(defn- cfg-for
  "Connect config for an existing KB, resolving its stored path. nil when the KB
   is not in the registry."
  [db-scope]
  (when-let [path (kb-store-path db-scope)]
    (kb-datahike-cfg path db-scope)))


(defn- kb-create-cfg
  "Config for CREATING a KB store. Identical to `kb-datahike-cfg` plus the
   create-time-fixed index layout — see `sdh/diff-buf-size` for why it must not
   appear on the connect path."
  [path db-scope]
  (assoc (kb-datahike-cfg path db-scope)
         :index-config {:diff-buf-size sdh/diff-buf-size}
         ;; required by :crypto-hash?, and what branch ancestry is read from
         :commit-graph? true))

(defn kb-system-id
  "Stable system-id for a KB's datahike scope. Keying by db-scope
   means the same KB resolves to the same registered system across
   re-connects. See is.simm.runtimes.branching."
  [db-scope]
  (branching/kb-system-id db-scope))

(defn create-kb-database!
  "Create the Datahike database for a KB scope.
   Installs the simmis schema so wiki pages, messages, etc. work.

   `at` is the instant the KB is to have COME INTO EXISTENCE at — passed to
   `store/install!`, which stamps schema and seed with it. A seeder that
   back-dates its content passes the earliest instant that content will claim,
   so the store's vocabulary is older than everything written into it and a cut
   anywhere in the narrative resolves. Omitted for a KB a user creates now."
  ([path db-scope] (create-kb-database! path db-scope nil))
  ([path db-scope at]
  (let [cfg (kb-datahike-cfg path db-scope)]
    (when-not (d/database-exists? cfg)
      ;; create + dvergr chat schema + simmis categorical schema, in one idiom
      (let [conn (sdh/provision! {:cfg (kb-create-cfg path db-scope) :register? false})]
        ;; BEFORE `store/ensure!` — see `declare-wiki-fulltext!`. The installer
        ;; ends by governing the store, and a governed store cannot take a new
        ;; scriptum index.
        (declare-wiki-fulltext! conn path)
        ;; simmis's half — the SAME installer a room store gets, so a KB can
        ;; hold a book and a room store can hold wiki pages.
        (store/ensure! conn db-scope at)
        ;; Register with the server's yggdrasil-aware execution context
        ;; so fork-aware code paths can reach this conn.
        (branching/register-kb-conn! conn db-scope)
        ;; Install the branching-broadcast tx listener so :branch/tx-occurred
        ;; events flow to subscribed clients via kabel pubsub.
        (try
          (require 'is.simm.model.branching-broadcast)
          ((resolve 'is.simm.model.branching-broadcast/install-kb-tx-listener!)
           conn db-scope)
          ;; Without this listener the KB still works, but clients never learn
          ;; it changed — edits appear to land and then not propagate, which
          ;; reads as a sync bug rather than a missing listener.
          (catch Exception e
            (log/log! {:level :warn :id ::kb-tx-listener-install-failed
                       :msg "KB tx listener not installed — clients will not see branch events"
                       :data {:db-scope (str db-scope) :error (.getMessage e)}})))
        (log/log! {:level :info
                   :id ::kb-database-created
                   :msg "KB Datahike database created"
                   :data {:db-scope db-scope :at at}})
        conn)))))

;; `widget-code-schema` and a second copy of `page-title-unique-schema` used to
;; live here and be transacted on every KB connect. `is.simm.model.store` is now
;; the one installer for every simmis store, and owns both: the uniqueness
;; binding as `store/page-title-unique-schema`, the attributes as
;; `schema/full-schema` + `store/ensure-late-schema!`.
;;
;; They were left behind as unreferenced defs when the installer landed, which
;; silently stopped `:S.Page/kind` (and the summarizer's room/window backref)
;; from reaching ANY store — dead code that was still load-bearing. Deleted
;; rather than re-wired, so there is one place to add a KB attribute again.

(defn connect-kb-database
  "Connect to an existing KB's Datahike database, ensure late-added schema,
   and (re)register it as a fork-aware yggdrasil system."
  [db-scope]
  (when-let [cfg (cfg-for db-scope)]
    (when (d/database-exists? cfg)
      ;; connect (db exists) + idempotently upsert the late-added attr — no dvergr
      ;; schema reinstall (already present), no create.
      (let [conn (sdh/provision! {:cfg cfg :schema? false :register? false})]
        ;; Before `store/ensure!` for the same reason as on the create path.
        (declare-wiki-fulltext! conn (get-in cfg [:store :path]))
        ;; Same installer as creation and as a room store — one definition of
        ;; what a simmis store contains, applied once per process per store.
        (store/ensure! conn db-scope)
        (branching/register-kb-conn! conn db-scope)
        ;; The tx listener belongs HERE too, not only on the create path.
        ;; It was create-only, so after any server restart every existing KB
        ;; connected WITHOUT one and never told a client it had changed again
        ;; — edits landed and silently failed to propagate until a page
        ;; reload, which reads as a sync bug. `d/listen` is keyed, so
        ;; re-installing is a no-op.
        (try
          ((requiring-resolve 'is.simm.model.branching-broadcast/install-kb-tx-listener!)
           conn db-scope)
          (catch Exception e
            (log/log! {:level :warn :id ::kb-tx-listener-install-failed
                       :msg "KB tx listener not installed — clients will not see its changes"
                       :data {:db-scope (str db-scope) :error (.getMessage e)}})))
        conn))))

;; =============================================================================
;; KB CRUD
;; =============================================================================

(defn create-kb!
  "Create a new knowledge base. Registers it in the shared system registry
   (dvergr :system/* row) so it can be attached to rooms as a grant.
   Returns the KB map.

   `:at` back-dates the STORE's install (see `create-kb-database!`), not the
   registry row: the row lives in the system DB, which no time-travel view
   reads, while the store is what a cut is taken against."
  [owner-id name & {:keys [tags at]}]
  (when-let [conn (system-db/get-conn)]
    (let [kb-id (random-uuid)
          ;; PATH first, id derived from it — dvergr's addressing, so a granted
          ;; simmis KB registers correctly into a room's composite.
          path (new-kb-path)
          db-scope (srooms/store-id path)
          sys-id (sdb/register-system! {:type :kb :name name
                                        :scope path
                                        :owner-id owner-id})
          kb (cond-> {:kb/id kb-id
                      :kb/name name
                      :kb/owner owner-id
                      :kb/created (java.util.Date.)
                      :kb/db-scope db-scope
                      :kb/system-id sys-id}
               (seq tags) (assoc :kb/tags (set tags)))]
      (d/transact conn [kb])
      ;; Create the actual Datahike database for this KB
      (create-kb-database! path db-scope at)
      (log/log! {:level :info
                 :id ::kb-created
                 :msg "Knowledge base created"
                 :data {:kb-id kb-id :name name :owner owner-id :db-scope db-scope
                        :system-id sys-id}})
      kb)))

(defn get-kb
  "Get a KB by ID."
  [kb-id]
  (when-let [conn (system-db/get-conn)]
    (when-let [eid (d/q '[:find ?e . :in $ ?kid :where [?e :kb/id ?kid]]
                        @conn kb-id)]
      (dissoc (d/pull @conn '[*] eid) :db/id))))

(defn get-party-kbs
  "Get all KBs a user owns or has access to."
  [user-id]
  (when-let [conn (system-db/get-conn)]
    (let [;; KBs user owns
          owned (d/q '[:find [(pull ?e [:kb/id :kb/name :kb/owner :kb/created
                                         :kb/db-scope :kb/tags]) ...]
                       :in $ ?uid
                       :where [?e :kb/owner ?uid]]
                     @conn user-id)
          ;; KBs shared with user
          shared (d/q '[:find [(pull ?e [:kb/id :kb/name :kb/owner :kb/created
                                          :kb/db-scope :kb/tags]) ...]
                        :in $ ?uid
                        :where [?e :kb/shared-with ?uid]]
                      @conn user-id)]
      (->> (concat owned shared)
           (map #(dissoc % :db/id))
           (sort-by :kb/name)
           (distinct)
           vec))))

(defn share-kb!
  "Share a KB with a user."
  [kb-id user-id]
  (when-let [conn (system-db/get-conn)]
    (when-let [eid (d/q '[:find ?e . :in $ ?kid :where [?e :kb/id ?kid]]
                        @conn kb-id)]
      (d/transact conn [{:db/id eid :kb/shared-with user-id}])
      (log/log! {:level :info
                 :id ::kb-shared
                 :msg "KB shared with user"
                 :data {:kb-id kb-id :user-id user-id}}))))

(defn unshare-kb!
  "Remove a user's access to a KB."
  [kb-id user-id]
  (when-let [conn (system-db/get-conn)]
    (when-let [eid (d/q '[:find ?e . :in $ ?kid :where [?e :kb/id ?kid]]
                        @conn kb-id)]
      (d/transact conn [[:db/retract eid :kb/shared-with user-id]]))))

(defn delete-kb!
  "Delete a KB (metadata only — Datahike DB cleanup is separate)."
  [kb-id]
  (when-let [conn (system-db/get-conn)]
    (when-let [eid (d/q '[:find ?e . :in $ ?kid :where [?e :kb/id ?kid]]
                        @conn kb-id)]
      (d/transact conn [[:db/retractEntity eid]])
      (log/log! {:level :info
                 :id ::kb-deleted
                 :msg "KB deleted"
                 :data {:kb-id kb-id}}))))

;; =============================================================================
;; Room-KB Attachment — dvergr shared-registry grants (Stage 1b of
;; doc/dvergr-integration-plan.md). A KB attaches to a room as a
;; :grant/* row (subject–relation–resource shaped, the seam eacl
;; extends in Stage 3). dvergr's fork-aware resolvers pick attached
;; KBs up automatically for agents.
;; =============================================================================

(defn ensure-kb-system!
  "Resolve (or lazily create) the shared-registry :system row for a KB.
   Returns the system uuid, or nil if the KB doesn't exist."
  [kb-id]
  (when-let [conn (system-db/get-conn)]
    (when-let [kb (get-kb kb-id)]
      (or (:kb/system-id kb)
          ;; `:system/scope` is a PATH (dvergr's rule) — writing the bare
          ;; db-scope here is what made dvergr open a store at a relative path
          ;; named after the uuid. `kb-store-path` reads the path a sibling row
          ;; already recorded; without one there is nothing safe to register.
          (let [path   (kb-store-path (:kb/db-scope kb))
                sys-id (sdb/register-system! {:type :kb
                                              :name (:kb/name kb)
                                              :scope path
                                              :owner-id (:kb/owner kb)})]
            (d/transact conn [{:kb/id kb-id :kb/system-id sys-id}])
            sys-id)))))

(defn attach-kb-to-room!
  "Attach a KB to a room with `permission` (:read or :read-write,
   default :read-write). Idempotent — re-attaching updates the permission."
  ([room-id kb-id] (attach-kb-to-room! room-id kb-id :read-write))
  ([room-id kb-id permission]
   (when (system-db/get-conn)
     (when-let [sys-id (ensure-kb-system! kb-id)]
       (sdb/attach! room-id sys-id permission)
       (log/log! {:level :info :id ::kb-attached
                  :data {:room-id room-id :kb-id kb-id :permission permission}})))))

(defn detach-kb-from-room!
  "Detach a KB from a room (removes the grant)."
  [room-id kb-id]
  (when (system-db/get-conn)
    (when-let [sys-id (:kb/system-id (get-kb kb-id))]
      (sdb/detach! room-id sys-id))))

(defn room-kb-ids
  "KB ids attached to a room (via grants)."
  [room-id]
  (when-let [conn (system-db/get-conn)]
    (d/q '[:find [?kid ...]
           :in $ ?rid
           :where
           [?r :room/id ?rid] [?g :grant/room ?r]
           [?g :grant/system ?s] [?s :system/id ?sid]
           [?kb :kb/system-id ?sid] [?kb :kb/id ?kid]]
         @conn room-id)))

(defn get-room-kbs
  "Get KBs attached to a room, each with its grant :kb/permission."
  [room-id]
  (when-let [conn (system-db/get-conn)]
    (let [rows (d/q '[:find ?kid ?perm
                      :in $ ?rid
                      :where
                      [?r :room/id ?rid] [?g :grant/room ?r]
                      [?g :grant/system ?s] [?g :grant/permission ?perm]
                      [?s :system/id ?sid]
                      [?kb :kb/system-id ?sid] [?kb :kb/id ?kid]]
                    @conn room-id)]
      (when (seq rows)
        (vec (keep (fn [[kid perm]]
                     (some-> (get-kb kid) (assoc :kb/permission perm)))
                   rows))))))

(defn rooms-using-kb
  "Rooms a KB is attached to (via grants). Returns room maps
   (:room/id :room/name :room/type)."
  [kb-id]
  (when-let [conn (system-db/get-conn)]
    (->> (d/q '[:find [(pull ?r [:room/id :room/name :room/type]) ...]
                :in $ ?kid
                :where
                [?kb :kb/id ?kid] [?kb :kb/system-id ?sid]
                [?s :system/id ?sid] [?g :grant/system ?s]
                [?g :grant/room ?r]]
              @conn kb-id)
         (mapv #(dissoc % :db/id)))))

;; =============================================================================
;; Server Registration (konserve-sync)
;; =============================================================================

(defonce ^:private sync-registration-locks (atom {}))

(defn- sync-registration-lock
  "Return the stable in-process lock for one store scope."
  [db-scope]
  (or (get @sync-registration-locks db-scope)
      (get (swap! sync-registration-locks
                  #(if (contains? % db-scope)
                     %
                     (assoc % db-scope (Object.))))
           db-scope)))

(defn register-kb-for-sync!
  "Register a KB's Datahike DB with the kabel server for remote access.
   Concurrent callers for one scope are serialized and recheck the topic, so
   only one of them opens the store and its full-text secondary index."
  [db-scope server-peer]
  (locking (sync-registration-lock db-scope)
    (if (contains? (get-in @server-peer [:pubsub :topics]) db-scope)
      (log/log! {:level :debug
                 :id ::kb-already-registered
                 :data {:db-scope db-scope}})
      (when-let [kb-conn (connect-kb-database db-scope)]
        (require 'datahike.kabel.handlers)
        ((resolve 'datahike.kabel.handlers/register-store-for-remote-access!)
         db-scope kb-conn server-peer {:branches :trunk})
        (log/log! {:level :info
                   :id ::kb-registered-for-sync
                   :msg "KB registered for konserve-sync"
                   :data {:db-scope db-scope}})))))

(defn register-party-kbs-for-sync!
  "Register all of a party's KBs for sync. Called on login."
  [party-id server-peer]
  (doseq [kb (get-party-kbs party-id)]
    (register-kb-for-sync! (:kb/db-scope kb) server-peer)))

;; =============================================================================
;; Default KB
;; =============================================================================

(defn ensure-default-kb!
  "Ensure user has a default personal KB. Creates one if not."
  [user-id user-name]
  (when-let [conn (system-db/get-conn)]
    (let [existing (d/q '[:find (pull ?e [:kb/id :kb/name :kb/db-scope]) .
                           :in $ ?uid
                           :where [?e :kb/owner ?uid]]
                        @conn user-id)]
      (if existing
        (dissoc existing :db/id)
        (create-kb! user-id (str user-name "'s Wiki"))))))

;; =============================================================================
;; Demo / Template Content
;; =============================================================================


;; =============================================================================
;; katzen binding + Identity pullback (doc/kb-unification.md)
;;
;; One abstract schema (katzen.schema.knowledge), two bindings: simmis binds
;; Entity ≡ S/Page with title on :S.Page/title; dvergr binds Entity → :entity/*
;; with title on :entity/title. Cross-store [[Title]] resolution is the
;; pullback over the Identity VALUE — never over idents, never duplicated.
;; =============================================================================

(def simmis-kb-binding
  "The simmis binding of katzen's knowledge schema: Entity ≡ S/Page."
  {:object "S/Page" :title :S.Page/title})

(def dvergr-kb-binding
  "dvergr's binding (room-internal knowledge graphs): Entity → :entity/*."
  {:object :Entity :title :entity/title})

(defn resolve-title
  "The Identity-pullback fiber for `title` across `stores` — a seq of
   {:name … :conn … :binding …} maps (mixed simmis/dvergr KBs welcome).
   Returns [{:kb name :title title :eid eid :uuid maybe-uuid} …] for every
   store whose bound title attr carries that VALUE."
  [stores title]
  (into []
        (keep (fn [{:keys [name conn binding]}]
                (let [ident (:title binding :S.Page/title)
                      db @conn
                      eid (d/q '[:find ?e . :in $ ?ident ?t
                                 :where [?e ?ident ?t]]
                               db ident title)]
                  (when eid
                    {:kb name :title title :eid eid
                     :uuid (:entity/uuid (d/pull db [:entity/uuid] eid))}))))
        stores))

(defn extract-wikilinks
  "[[Title]] targets in a text/html string (order-preserving, deduped).

   Uses `refs/page-reference-pattern`, the ONE grammar, rather than a second
   regex. The local one understood `[[Title]]` and `[[Title|Display]]` but
   could not match `[[Title][Display]]` at all — measured, it returns nil for
   `[[dh://abc/def][Bond]]`. That is the exact form `kb/link` hands an agent to
   paste into chat for a cross-database link, so agent-authored cross-db
   mentions were never indexed: no `:S.Message/mentions` row, no notification,
   no backlink.

   The trailing `(str/split #\"\\|\")` keeps the legacy pipe form working —
   the canonical pattern would otherwise capture `Title|Display` whole."
  [s]
  (->> (re-seq refs/page-reference-pattern (str s))
       (map (fn [[_ target _display]]
              (-> (str target) (clojure.string/split #"\|") first clojure.string/trim)))
       (remove clojure.string/blank?)
       distinct
       vec))

(defn normalize-title
  "Case/whitespace-insensitive dedup key for a page title. Mirrors the fuzzy
   match in room-agents' `knowledge_add`, so a `[[Report]]` wikilink resolves to
   a page an office/knowledge publish created as `report` or `Report ` instead
   of minting a near-duplicate stub."
  [t]
  (-> (str t) clojure.string/trim clojure.string/lower-case
      (clojure.string/replace #"\s+" " ")))

(defn resolve-room-links
  "Rewrite bare `[[Title]]` / `[[Title][Display]]` links in `content` to explicit
   `[[dh://…][Display]]` references by resolving each title against the KBs
   attached to `room-uuid`.

   A chat message lives in a room's content store, not in any KB, so a bare
   `[[Title]]` cannot say WHICH database it means. Here we resolve it: a title
   found in exactly ONE attached KB is rewritten to a dh:// reference (the URI
   carries that KB's store id + the page uuid, so the client opens it directly
   with no guessing); a title found in zero or several KBs is left bare and
   reported so the caller can nudge the author toward an explicit `kb/link`.
   Already-explicit `[[dh://…]]` links pass through untouched.

   Title match is case/whitespace-normalized (see `normalize-title`), mirroring
   `link-block-references!`. Returns
   `{:content <rewritten> :unresolved [{:title :reason :candidates}…]}`, where
   `:reason` is `:not-found` or `:ambiguous` (with `:candidates` = the KB names
   matched). Pure read — never writes."
  [room-uuid content]
  (if-not (and (string? content) (str/includes? content "[["))
    {:content content :unresolved []}
    (let [kbs (get-room-kbs room-uuid)
          ;; One title scan per attached KB per call (cached), keyed by both the
          ;; exact and normalized title. Cheaper than a per-link AVET probe when a
          ;; message carries several links, and avoids re-querying across links.
          index (memoize
                  (fn [db-scope]
                    (if-let [conn (connect-kb-database db-scope)]
                      (reduce (fn [m [t u]]
                                (-> (assoc m t u)
                                    (update ::norm assoc (normalize-title t) u)))
                              {::norm {}}
                              (d/q '[:find ?t ?u :where
                                     [?e :S.Page/title ?t] [?e :entity/uuid ?u]] @conn))
                      {::norm {}})))
          lookup (fn [db-scope title]
                   (let [idx (index db-scope)]
                     (or (get idx title)
                         (get (::norm idx) (normalize-title title)))))
          unresolved (volatile! [])
          resolved
          (str/replace
            content refs/page-reference-pattern
            (fn [[whole target display]]
              (if (str/starts-with? target "dh://")
                whole                                   ; already explicit
                (let [hits (keep (fn [kb]
                                   (when-let [u (lookup (:kb/db-scope kb) target)]
                                     {:scope (:kb/db-scope kb) :uuid u :name (:kb/name kb)}))
                                 kbs)]
                  (case (count hits)
                    1 (let [{:keys [scope uuid]} (first hits)]
                        (str "[[" (dh-ref/render (dh-ref/reference scope [:entity/uuid uuid]))
                             "][" (or display target) "]]"))
                    0 (do (vswap! unresolved conj {:title target :reason :not-found})
                          whole)
                    (do (vswap! unresolved conj
                                {:title target :reason :ambiguous
                                 :candidates (mapv :name hits)})
                        whole))))))]
      {:content resolved :unresolved @unresolved})))

(defn link-block-references!
  "Extract [[wikilinks]] from `content`, ensure each target page exists
   (Roam semantics: linking creates), and add :block/references datoms on
   `block-uuid`. The stored refs make backlinks + neighborhood plain
   queries. Returns the vector of target page uuids.

   Existing-page match is title-NORMALIZED (see `normalize-title`) rather than
   exact, so a link whose case/spacing differs from the stored title reuses that
   page instead of forking a duplicate."
  [kb-conn block-uuid content]
  (let [titles (extract-wikilinks content)]
    (when (seq titles)
      (let [pages (d/q '[:find ?t ?u :where
                         [?e :S.Page/title ?t] [?e :entity/uuid ?u]]
                       @kb-conn)
            targets
            (mapv (fn [t]
                    (let [want (normalize-title t)]
                      (or (some (fn [[et eu]] (when (= (normalize-title et) want) eu)) pages)
                        (let [u (random-uuid) now (java.util.Date.)]
                          (d/transact kb-conn
                            [{:entity/uuid u :entity/name t
                              :entity/created-at now :entity/updated-at now
                              :instance/of-role [:entity/name "S/Page"]
                              :S.Page/title t :S.Page/archived false}
                             ;; UI invariant: every page has ≥1 block (the
                             ;; editor treats blockless as still-loading).
                             {:entity/uuid (random-uuid)
                              :entity/created-at now :entity/updated-at now
                              :instance/of-role [:entity/name "S/Block"]
                              :block/parent [:entity/uuid u]
                              :block/order "a0"
                              :block/content ""}])
                          u))))
                  titles)]
        (d/transact kb-conn
          [{:entity/uuid block-uuid
            :block/references (mapv (fn [u] [:entity/uuid u]) targets)}])
        targets))))

;; =============================================================================
;; Fulltext (scriptum secondary index — mirrors dvergr's declare-kb-fulltext!)
;; =============================================================================

(def wiki-fulltext-ident :kb/wiki-fulltext)

(defn declare-wiki-fulltext!
  "Declare the scriptum fulltext index over the simmis wiki attrs
   (:S.Page/title + :block/content) on a KB conn. Best-effort — failure
   leaves fulltext unavailable, never breaks provisioning.

   The index directory is a SIDECAR of the store, `<store-path>-ft`, so it
   has to be derived from the path and not from `db-scope`. Those used to be
   the same string; since the store id became a hash OF the path they are not,
   and building the sidecar path from the id put it beside a directory that
   holds no store — leaving orphan `-ft` dirs and no index next to the data.

   MUST run BEFORE the store is governed, and is therefore called ahead of
   `store/ensure!` on both the create and the connect path. kontor's governance
   transaction predicate validates by running a SPECULATIVE `datahike.core/db-with`,
   and datahike instantiates secondary indices on that path too
   (`finalize-secondary-indices`); scriptum's Lucene directory takes an exclusive
   write lock, so the subsequent real write fails with LockObtainFailedException
   (\"Lock held by this virtual machine\"). Room stores never hit this only because
   dvergr declares their message index during provisioning, before simmis governs
   them. The general fix belongs in datahike — a speculative `db-with` should
   reuse an instantiated secondary index rather than open a second writer.

   Declared only when absent: it is a schema transaction, so repeating it buys
   nothing and costs a second IndexWriter on the same directory. Note the
   failure surfaces asynchronously inside datahike's writer, where the `catch`
   below cannot see it — it logs nothing, so it has to be avoided, not handled."
  [conn path]
  (try
    (when-not (d/q '[:find ?e . :in $ ?i :where [?e :db/ident ?i]]
                   @conn wiki-fulltext-ident)
      ((requiring-resolve 'dvergr.search.secondary/declare-index!)
       conn wiki-fulltext-ident [:S.Page/title :block/content]
       (str path "-ft")))
    (catch Throwable t
      (log/log! {:level :debug :id ::wiki-fulltext-declare-failed
                 :data {:path path :error (ex-message t)}})
      nil)))

(defn kb-upsert-knowledge-page!
  "Publish/extend a knowledge entry as a wiki page in `kb-conn`
   (the shared product KB — see room-agents' knowledge_add wiring).
   Title-addressable so [[Title]] chat references resolve to it.

   Creates the page with a summary block when missing; otherwise appends
   the new context as a block (entities accumulate context over time,
   matching dvergr's knowledge-graph semantics). Returns the page-uuid."
  [kb-conn title & {:keys [summary context source url]}]
  (let [db @kb-conn
        existing (d/q '[:find ?u . :in $ ?t
                        :where [?e :S.Page/title ?t] [?e :entity/uuid ?u]]
                      db title)
        now (java.util.Date.)
        line (->> [(when (and summary (seq summary)) summary)
                   (when (and context (seq context)) context)
                   (when (or source url)
                     (str "<em>" (or source "") (when url (str " — " url)) "</em>"))]
                  (remove nil?)
                  (clojure.string/join "<br/>"))
        page-uuid (or existing (random-uuid))]
    (when-not existing
      (d/transact kb-conn
        [{:entity/uuid page-uuid
          :entity/name title
          :entity/created-at now
          :entity/updated-at now
          :instance/of-role [:entity/name "S/Page"]
          :S.Page/title title
          :S.Page/archived false}]))
    (when (seq line)
      (let [last-order (->> (d/q '[:find [?o ...] :in $ ?p
                                   :where [?b :block/parent ?p] [?b :block/order ?o]]
                                 @kb-conn [:entity/uuid page-uuid])
                            sort last)]
        (let [block-uuid (random-uuid)]
          (d/transact kb-conn
            [{:entity/uuid block-uuid
              :entity/created-at now
              :entity/updated-at now
              :instance/of-role [:entity/name "S/Block"]
              :block/parent [:entity/uuid page-uuid]
              :block/order (frac/generate-key-between last-order nil)
              :block/content (str "<p>" line "</p>")}])
          ;; [[wikilinks]] in the text become stored refs (backlinks a query)
          (link-block-references! kb-conn block-uuid line))))
    page-uuid))
