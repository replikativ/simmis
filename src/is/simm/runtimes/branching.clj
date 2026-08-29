(ns is.simm.runtimes.branching
  "Per-KB branching seam for simmis. Thin shim around
   `yggdrasil.adapters.datahike` + `org.replikativ.spindel.yggdrasil`.

   Earlier history
   ---------------
   This namespace used to carry a local `KBConnRef` record implementing
   `spindel.engine.protocols/PForkable` directly, on the theory that
   pulling in `yggdrasil.adapters.datahike` would drag a `datahike.remote`
   / `superv.async` class-loading conflict into simmis. Verified
   2026-05-26 in the running simmis REPL: the conflict is gone, the
   yggdrasil datahike adapter and the spindel.yggdrasil bridge load and
   cascade-fork cleanly on live simmis KB connections.

   What this ns is now
   -------------------
   - `register-kb-conn!` wraps a datahike conn in a
     `yggdrasil.adapters.datahike` system named `(kb-system-id db-scope)`
     and registers it with the server execution context via
     `spindel.yggdrasil/register!`. The bridge's
     `(extend-protocol PForkable Object …)` then participates in
     `fork-context` automatically — same cascade behaviour as the old
     KBConnRef, now uniform with how dvergr.discourse + dvergr.substrate.git
     register their own yggdrasil systems on a room's ctx.
   - `get-kb-system` returns the live yggdrasil datahike system (a
     `DatahikeSystem` record) from the current execution context.
     The full `yggdrasil.protocols` stack — Snapshotable, Branchable,
     Graphable, Mergeable, GarbageCollectable — is available on it.
   - `get-kb-conn` returns the underlying datahike connection from
     the current execution context. Fork-aware: a forked ctx sees the
     branch's conn because yggdrasil's `checkout` returns a new
     DatahikeSystem with a branch-bound conn.

   See doc/archive/branching-systematic-design.md for the layering rationale."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [yggdrasil.protocols :as yp]
            [datahike.versioning :as dv]
            [dvergr.substrate.datahike :as sdh]
            [dvergr.system.db :as sdb]
            [dvergr.system.rooms :as rooms]
            ;; geschichte's own workspace/repo verbs, for the overlay lifecycle
            ;; at the bottom of this file. yggdrasil's protocols cover branch,
            ;; diff, conflicts and merge; publish/advance/list have no protocol
            ;; equivalent, so the repo half reaches through to geschichte here.
            [geschichte.bytes :as gbytes]
            [geschichte.merge :as gmerge]
            [geschichte.repo :as grepo]
            [geschichte.workspace :as gws]
            [org.replikativ.spindel.engine.core :as rtc]
            [org.replikativ.spindel.yggdrasil :as ygg]
            [is.simm.runtimes.context :as ctx]
            [is.simm.model.branching-broadcast :as bcast]
            [taoensso.telemere :as log]))

(defn kb-system-id
  "Stable string ID under which a KB's yggdrasil datahike system is
   registered as a spindel external-ref."
  [db-scope]
  (str "kb:" db-scope))

(defn register-system!
  "Register a KB connection as a yggdrasil datahike system on the
   server execution context. Idempotent: re-registering for the same
   `db-scope` overwrites the previous entry.

   Returns the registered yggdrasil system."
  [conn db-scope]
  (ctx/with-server-context
    ;; create+register via the shared dvergr idiom (dvergr.substrate.datahike/provision!).
    ;; :schema? false — the KB's schema (dvergr + simmis categorical) is installed at
    ;; create time by knowledge-bases.clj; here we only (re)seat the ygg system.
    (sdh/provision! {:conn conn :schema? false :register? true
                     :system-name (kb-system-id db-scope)})
    (let [sys (ygg/system (kb-system-id db-scope))]  ; resolve the just-registered system
      (log/log! {:level :info
                 :id ::kb-registered
                 :msg "KB registered as yggdrasil datahike system"
                 :data {:db-scope (str db-scope)
                        :system-id (kb-system-id db-scope)}})
      sys)))

(def ^{:deprecated "use register-system!"} register-kb-conn!
  "DEPRECATED alias — the registration was never KB-specific (drives
   register through it too); renamed for the proposals/ForkSet work."
  register-system!)

(defn get-kb-system
  "Return the registered yggdrasil datahike system for `db-scope` from
   the *current* execution context. In a forked context, returns the
   system on the branch; in the parent context, the trunk system."
  [db-scope]
  (ygg/system (kb-system-id db-scope)))

(defn get-kb-conn
  "Return the underlying datahike connection for `db-scope` from the
   current execution context, or nil if not registered. Fork-aware."
  [db-scope]
  (when-let [sys (get-kb-system db-scope)]
    (:conn sys)))

;; ============================================================================
;; Branch operations — server-side, delegate to yggdrasil.protocols.
;; ============================================================================

(def get-system
  "Neutral alias of get-kb-system (KBs, drives, room stores)."
  get-kb-system)

(def get-system-conn
  "Neutral alias of get-kb-conn."
  get-kb-conn)

;; ============================================================================
;; Repos — a room's geschichte code repository as a yggdrasil system.
;;
;; Everything above resolves through `kb-system-id` on the SERVER context.
;; Repos answer to neither half of that, and the context is the part that bites:
;; `dvergr.system.rooms/register-room-systems!` registers a room's repo into the
;; ROOM's execution context, so the server context holds none at all (measured
;; 2026-07-28 against the live server: `registered-systems` under
;; `ctx/with-server-context` contains zero `:geschichte` systems). A lookup on
;; the wrong context does not throw — it answers nil, and a repo fork then falls
;; through to the datom-count renderer looking merely uninteresting rather than
;; unresolved.
;; ============================================================================

(defn repo-scope-path
  "A repo `scope` as its canonical PATH, accepting either form.

   Two forms exist because the two stores that hold a repo scope disagree about
   what a scope IS. dvergr's registry keys a repo by `:system/scope`, the path;
   simmis's `:proposal.fork/scope` is declared `:db.type/uuid`, so a filed fork
   can only carry the system's `:system/id`. Rather than widen that attribute
   (datahike cannot change a valueType in place) or rebuild the path by string
   concatenation — which is precisely the bare-uuid mistake that once leaked six
   stores to the repository root — a uuid is resolved back through the registry,
   which is the only thing that knows where a system actually lives."
  [scope]
  (cond
    (uuid? scope) (:system/scope (sdb/system-by-id scope))
    (and (string? scope) (not (str/includes? scope "/")))
    (some-> (parse-uuid scope) sdb/system-by-id :system/scope)
    :else (str scope)))

(defn- repo-system-id
  "Yggdrasil system id under which dvergr registers a room repo.

   MIRRORS the private `dvergr.system.rooms/repo-system-name`. Roadmap #6 tracks
   making dvergr's naming public so this copy can go; until then keep the two in
   step, because drift here resolves nothing rather than failing."
  [path]
  (str "room-repo-" (.getName (io/file (str path)))))

(defn- room-ctx-owning-repo
  "The execution context of the room that owns repo `scope`, or nil.

   A linear scan over rooms — small (a room count, not a datom count) and taken
   once per diff, against a registry that is the only thing that knows which
   room a repo path belongs to."
  [path]
  (let [target (str path)]
    (some (fn [{:room/keys [id]}]
            (when (some #(= target (str (:path %))) (rooms/room-repos id))
              (rooms/room-ctx-for id)))
          (sdb/all-rooms))))

(defn get-repo-system
  "The yggdrasil geschichte system for a repo `scope` (a path OR a `:system/id`
   uuid — see `repo-scope-path`), resolved on the context of the room that owns
   it. nil when no room claims the scope or the room failed to hydrate."
  [scope]
  (when-let [path (repo-scope-path scope)]
    (when-let [room-ctx (room-ctx-owning-repo path)]
      (binding [rtc/*execution-context* room-ctx]
        (ygg/system (repo-system-id path))))))

(defn system-for
  "The yggdrasil system for `scope`, dispatching on `system-type` and binding the
   context that system actually lives on — the caller does not have to know
   which, and cannot get it wrong by omission.

   `:repo` resolves a geschichte system on the owning room's context; every
   other type is a datahike system on the server context."
  [scope system-type]
  (if (= :repo system-type)
    (get-repo-system scope)
    (ctx/with-server-context (get-kb-system scope))))

(defn repo-trunk
  "The branch a repo fork lands on: `:main`, the branch dvergr's clone seed
   creates — NOT the `:db` that names a datahike store's trunk.

   Deliberately NOT `current-branch`. Geschichte's `checkout` MUTATES the shared
   connection (it moves the repository's one current ref and hands back a record
   over the same conn), so whatever branch HEAD happens to sit on is a property
   of who checked out last, not of where trunk is. Reading trunk off it would
   make a fork land on another fork. `current-branch` is used only as a fallback
   for a repo that has no `:main` at all."
  [scope]
  (if-let [sys (get-repo-system scope)]
    (let [bs (yp/branches sys)]
      (if (contains? bs :main) :main (yp/current-branch sys)))
    :main))

(defn- gen-branch-name
  "Deterministic-ish branch name from a slug. `:db-fork-<slug>` so the
   datahike branch-name convention matches yggdrasil's fork-ygg-system."
  [parent-branch slug]
  (let [clean (-> (or slug (str (random-uuid)))
                  (str/replace #"[^A-Za-z0-9-]+" "-"))]
    (keyword (str (name parent-branch) "-" clean))))

(defn get-kb-conn-on-branch
  "Resolve a datahike connection bound to a specific branch of a KB. Used by
   spin-remote write handlers when the client signals an explicit active
   branch. Returns nil when the KB isn't registered."
  [db-scope branch-kw]
  (when-let [sys (get-kb-system db-scope)]
    (let [current (yp/current-branch sys)]
      (if (= current branch-kw)
        (:conn sys)
        (:conn (yp/checkout sys branch-kw))))))

(def ^:private internal-branch-re
  ;; Ephemeral branches minted by spindel's ctx forking (an overlay/branch
  ;; fork per execution-context fork — one per room per boot since rooms run
  ;; on forked ctxs). Never user-facing, never merged.
  #"^(?:overlay|fork)-[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$")

(defn internal-branch?
  "True for ephemeral ctx-fork branches (overlay-<uuid> / fork-<uuid>)."
  [branch-kw]
  (boolean (re-matches internal-branch-re (name branch-kw))))

(defn list-kb-branches
  "Return the set of USER-FACING branch keywords for a KB (internal
   ctx-fork branches filtered). Reads via yggdrasil's Branchable protocol."
  [db-scope]
  (when-let [sys (get-kb-system db-scope)]
    (into #{} (remove internal-branch?) (yp/branches sys))))

(defn gc-internal-branches!
  "Delete leaked ctx-fork branches (overlay-*/fork-*) from a KB. They are
   per-process ephemera; after a JVM restart every survivor is an orphan.
   Never touches the current branch. Returns the number deleted."
  [db-scope]
  (if-let [sys (get-kb-system db-scope)]
    (let [current (yp/current-branch sys)
          victims (->> (yp/branches sys)
                       (filter internal-branch?)
                       (remove #{current}))]
      (doseq [b victims]
        (try
          (yp/delete-branch! sys b)
          (catch Exception e
            (log/log! {:level :warn :id ::branch-gc-failed
                       :data {:db-scope (str db-scope) :branch b
                              :error (.getMessage e)}}))))
      (when (seq victims)
        (log/log! {:level :info :id ::internal-branches-gced
                   :msg "Leaked ctx-fork branches deleted"
                   :data {:db-scope (str db-scope) :count (count victims)}}))
      (count victims))
    0))

(defn branch-head-id
  "Snapshot id of `branch`'s current head, as a string, or nil. Used to
   content-address anything DERIVED from a branch (e.g. a cached diff summary):
   key on this and a branch that moves misses the cache instead of serving a
   description of a state that no longer exists."
  [db-scope branch-kw]
  (when-let [sys (get-kb-system db-scope)]
    (let [on-branch (if (= (yp/current-branch sys) branch-kw)
                      sys
                      (yp/checkout sys branch-kw))]
      (some-> (yp/snapshot-id on-branch) str))))

(defn merge-base-id
  "Snapshot-id of the common ancestor of `branch` and trunk, or nil.

   FALLBACK ONLY. Prefer the `:proposal.fork/base-commit` recorded when the
   branch was minted: this walks the commit graph (measured 361 ms on a
   325-commit KB, linear in commit count) and cannot answer once retention has
   pruned the base, whereas the recorded head is exact and free."
  [db-scope branch]
  (when-let [sys (get-kb-system db-scope)]
    (try (some-> (yp/common-ancestor sys branch (yp/current-branch sys)) str)
         (catch Exception e
           (log/log! {:level :warn :id ::merge-base-unresolved
                      :data {:db-scope (str db-scope) :branch branch
                             :error (.getMessage e)}})
           nil))))

(defn kb-db-at-commit
  "The KB's db value AS OF `commit-id` (a snapshot-id string or uuid), or nil.

   Needed because a diff taken against a merge BASE reports its `:removed`
   datoms with BASE entity ids, and the `before` side has to be read at the
   base — not at wherever trunk has since got to. Callers must read `before`
   out of this db.

   The hazard is subtler than eid REUSE, which datahike does not do: a base eid
   still denotes the same entity in trunk. What differs is its VALUE. If trunk
   edited a block after the fork and the branch deleted it, reading the base eid
   in the trunk db yields trunk's current text, and the review then shows the
   user a deletion of words the branch never saw. Measured 2026-07-26: branch
   retracts a block, trunk rewrites it, `before` comes back as trunk's rewrite.

   NOTE the deref on the conn. A `datahike.connector.Connection` is a record
   holding no `:store` — the store lives on the db value inside it. `(:store
   conn)` was nil, so this function failed for EVERY commit and every KB, and
   failed quietly: the caller logs `::commit-db-unresolvable` and falls back to
   trunk, which is exactly the state this function exists to avoid reading."
  [db-scope commit-id]
  (when (and db-scope commit-id)
    (when-let [sys (get-kb-system db-scope)]
      (let [store (:store @(:conn sys))
            cid (if (uuid? commit-id) commit-id (parse-uuid (str commit-id)))]
        (when cid
          (try (dv/commit-as-db store cid)
               (catch Exception e
                 (log/log! {:level :warn :id ::commit-db-unresolvable
                            :msg "Merge base did not resolve — diff falls back to trunk"
                            :data {:db-scope (str db-scope) :commit (str commit-id)
                                   :error (.getMessage e)}})
                 nil)))))))

(defn kb-diff-from
  "Diff `from` (a commit-id) → `branch`: what the BRANCH changed relative to the
   point it forked from, rather than relative to wherever trunk has since got to.

   Returns nil when `from` does not resolve, so callers can fall back to the
   two-branch diff and SAY they did — an additions-only diff that silently
   pretends to be a full one is how a reviewer accepts a change they never saw."
  [db-scope from branch]
  (when-let [sys (get-kb-system db-scope)]
    (when from
      (try (yp/diff sys (str from) branch)
           (catch Exception e
             (log/log! {:level :warn :id ::base-diff-failed
                        :data {:db-scope (str db-scope) :from (str from)
                               :branch branch :error (.getMessage e)}})
             nil)))))

(defn kb-commit-graph
  "Return the full commit graph for a KB via yggdrasil's Graphable
   protocol. Map shape: {:nodes {id {:parent-ids #{...} :meta {...}}}
                         :branches {branch-kw commit-id} :roots #{...}}."
  [db-scope]
  (when-let [sys (get-kb-system db-scope)]
    (yp/commit-graph sys)))

(defn branch-kb!
  "Create a new branch on a KB. `parent-branch` defaults to the current
   branch of the registered system (trunk on first call). `slug` is a
   short human-readable suffix appended to the parent's name. Returns
   `{:branch new-branch :parent parent-branch}`. Idempotent on the
   target branch name."
  ([db-scope slug]
   (branch-kb! db-scope slug nil))
  ([db-scope slug parent-branch]
   (when-let [sys (get-kb-system db-scope)]
     (let [parent (or parent-branch (yp/current-branch sys))
           new-branch (gen-branch-name parent slug)
           existing (yp/branches sys)]
       (if (contains? existing new-branch)
         (do (log/log! {:level :info :id ::branch-already-exists
                        :msg "Branch already exists, returning as-is"
                        :data {:db-scope (str db-scope) :branch new-branch}})
             {:branch new-branch :parent parent :existed? true})
         (do (yp/branch! sys new-branch parent)
             (log/log! {:level :info :id ::branch-created
                        :msg "KB branch created"
                        :data {:db-scope (str db-scope)
                               :parent-branch parent
                               :new-branch new-branch
                               :slug slug}})
             (bcast/emit-event! {:type :branch/created
                                 :db-scope db-scope
                                 :branch new-branch
                                 :parent parent})
             {:branch new-branch :parent parent :existed? false}))))))

(defn discard-kb-branch!
  "Delete a branch from a KB. Underlying snapshots stay in storage until
   GC. Returns `:ok` on success, `:not-registered` when the KB isn't
   registered. Refuses to delete the current trunk."
  [db-scope branch-kw]
  (if-let [sys (get-kb-system db-scope)]
    (let [trunk (yp/current-branch sys)]
      (if (= trunk branch-kw)
        (do (log/log! {:level :warn :id ::refuse-discard-trunk
                       :msg "Refused to discard trunk branch"
                       :data {:db-scope (str db-scope) :branch branch-kw}})
            :refuse-trunk)
        (do (yp/delete-branch! sys branch-kw)
            (log/log! {:level :info :id ::branch-discarded
                       :msg "KB branch discarded"
                       :data {:db-scope (str db-scope) :branch branch-kw}})
            (bcast/emit-event! {:type :branch/discarded
                                :db-scope db-scope
                                :branch branch-kw})
            :ok)))
    :not-registered))

(defn kb-diff
  "Return the structural diff between two branches of a KB. Delegates to
   yggdrasil's Mergeable.diff. Shape is adapter-specific (DatahikeDiff
   record with :added-datoms, :removed-datoms, :entities-touched)."
  [db-scope from-branch to-branch]
  (when-let [sys (get-kb-system db-scope)]
    (yp/diff sys from-branch to-branch)))

(defn kb-conflicts
  "Return the conflict descriptors between two branches via yggdrasil's
   Mergeable.conflicts. Datahike adapter currently returns []."
  [db-scope a-branch b-branch]
  (when-let [sys (get-kb-system db-scope)]
    (yp/conflicts sys a-branch b-branch)))

(defn merge-kb!
  "Merge `source-branch` into `target-branch`. Computes the diff via
   yggdrasil and applies it as a merge commit on target. Datahike's
   `merge!` requires tx-data + parent commits, which the adapter
   derives from the branch pair. Returns `:ok` or `:not-registered`."
  [db-scope source-branch target-branch]
  (if-let [sys (get-kb-system db-scope)]
    (let [target-conn (get-kb-conn-on-branch db-scope target-branch)]
      (when-not target-conn
        (throw (ex-info "Target branch not resolvable" {:db-scope db-scope
                                                        :target target-branch})))
      ;; yggdrasil's Mergeable.merge! computes diff internally when source is
      ;; a branch keyword. We checkout the target-conn first so the merge
      ;; commits onto target.
      (let [target-sys (yp/checkout sys target-branch)]
        (yp/merge! target-sys source-branch)
        (log/log! {:level :info :id ::branches-merged
                   :msg "KB branches merged"
                   :data {:db-scope (str db-scope)
                          :source-branch source-branch
                          :target-branch target-branch}})
        (bcast/emit-event! {:type :branch/merged
                            :db-scope db-scope
                            :branch target-branch
                            :source source-branch})
        :ok))
    :not-registered))

;; ============================================================================
;; Repo branch operations — the same five verbs as above, over a geschichte
;; system instead of a datahike one. Separate functions rather than a
;; system-type arity on each KB verb: the KB path is load-bearing for every
;; proposal in the system, and the two backends differ in ways that would make
;; a shared body lie (trunk is `:main` not `:db`; `checkout` mutates; deleting a
;; merged branch needs `:force?`). Callers dispatch on the fork's
;; `:proposal.fork/system-type`.
;; ============================================================================

(defn repo-head-id
  "Snapshot id (commit uuid, as a string) of `branch`'s tip in repo `scope`."
  [scope branch-kw]
  (when-let [sys (get-repo-system scope)]
    (some-> (yp/snapshot-meta sys branch-kw) :geschichte.commit/id str)))

(defn branch-repo!
  "Create a branch on a repo, off `parent-branch` (trunk by default). Same
   `{:branch :parent :existed?}` shape as `branch-kb!`."
  ([scope slug] (branch-repo! scope slug nil))
  ([scope slug parent-branch]
   (when-let [sys (get-repo-system scope)]
     (let [parent (or parent-branch (repo-trunk scope))
           new-branch (gen-branch-name parent slug)
           existing (yp/branches sys)]
       (if (contains? existing new-branch)
         {:branch new-branch :parent parent :existed? true}
         (do (yp/branch! sys new-branch parent)
             (log/log! {:level :info :id ::repo-branch-created
                        :data {:scope (str scope) :parent-branch parent
                               :new-branch new-branch :slug slug}})
             (bcast/emit-event! {:type :branch/created :db-scope scope
                                 :branch new-branch :parent parent})
             {:branch new-branch :parent parent :existed? false}))))))

(defn repo-diff
  "Unified diff between two commits/branches of a repo, as a
   `yggdrasil.types/GeschichteDiff` (`:files :patch :stat :summary`).

   `:patch? false` skips blob reading and rendering — a caller that only needs
   the changed-path list should pass it."
  ([scope from to] (repo-diff scope from to nil))
  ([scope from to opts]
   (when-let [sys (get-repo-system scope)]
     (yp/diff sys from to (or opts {})))))

(defn repo-conflicts
  "Conflict descriptors between two branches of a repo, from the three-tree
   merge plan. Unlike the datahike adapter's `[]`, these are real: a path both
   sides changed differently comes back as a `yggdrasil.types/Conflict`."
  [scope a-branch b-branch]
  (when-let [sys (get-repo-system scope)]
    (yp/conflicts sys a-branch b-branch)))

(defn merge-repo!
  "Merge `source-branch` into `target-branch` of a repo. Returns `:ok`,
   `:not-registered`, or throws with geschichte's reason (unresolved conflicts,
   no common ancestor).

   Checks out the target FIRST because geschichte merges into whatever the
   repository's current ref is — and that ref is shared mutable state, so
   merging without positioning it would land the fork on whichever branch
   someone else checked out last."
  [scope source-branch target-branch]
  (if-let [sys (get-repo-system scope)]
    (let [on-target (if (= (yp/current-branch sys) target-branch)
                      sys
                      (yp/checkout sys target-branch))]
      (yp/merge! on-target source-branch)
      (log/log! {:level :info :id ::repo-merged
                 :data {:scope (str scope) :source-branch source-branch
                        :target-branch target-branch}})
      (bcast/emit-event! {:type :branch/merged :db-scope scope
                          :branch target-branch :source source-branch})
      :ok)
    :not-registered))

(defn discard-repo-branch!
  "Delete a branch from a repo. Refuses to delete trunk.

   `:force? true` is not a shortcut here, it is required: geschichte's own
   merged-check rejects the delete even for a branch that IS fully merged
   (measured 2026-07-28 — after `merge!` the trunk tip carried the fork tip as a
   parent, and `delete-branch!` still answered \"Branch is not fully merged\").
   Without it, landing a repo fork would leave its branch behind forever.
   Reported upstream; drop the flag once geschichte's check is right."
  [scope branch-kw]
  (if-let [sys (get-repo-system scope)]
    (if (= (repo-trunk scope) branch-kw)
      :refuse-trunk
      (do (yp/delete-branch! sys branch-kw {:force? true})
          (log/log! {:level :info :id ::repo-branch-discarded
                     :data {:scope (str scope) :branch branch-kw}})
          (bcast/emit-event! {:type :branch/discarded :db-scope scope
                              :branch branch-kw})
          :ok))
    :not-registered))

;; ============================================================================
;; Repo overlays — the write side of a code fork.
;;
;; A geschichte connection has ONE current ref, and `checkout!` moves it, so
;; every agent in a room shares one HEAD. An overlay is the geschichte
;; equivalent of `git worktree add`: `workspace/fork!` plus a private
;; connection on its own datahike branch, so N agents write N branches at once.
;; It is O(1) — indices and blobs stay structurally shared — and stronger than
;; a git worktree in that each workspace owns its whole ref namespace rather
;; than sharing `refs/heads` with its siblings.
;;
;; That last property is the one that surprises: an overlay's refs are a COPY,
;; so a branch created canonically is not visible inside it and a branch created
;; inside it is not visible canonically. Handing work back is therefore an
;; explicit publish of one named ref, never an implicit consequence of
;; committing.
;; ============================================================================

(defn open-repo-overlay!
  "A private workspace over repo `scope`, forked from its current state.

   The caller owns the returned overlay and MUST eventually
   `discard-repo-overlay!` it — the datahike branch behind it is reclaimed by
   nothing else (`discard-repo-branch!` deletes a ref, which is a different
   thing)."
  [scope]
  (when-let [sys (get-repo-system scope)]
    (yp/overlay sys {})))

(defn repo-overlay-system
  "The `GeschichteSystem` inside an overlay — same system-id as the canonical
   one, bound to the overlay's private connection. This is what gets registered
   on a context so dvergr's file tools resolve the overlay instead of trunk:
   `current-workspace` filters registered systems by `system-type`, and the
   overlay record itself is not a system, only the thing holding one."
  [overlay]
  (some-> overlay :local-writes deref))

(defn repo-overlay-conn
  "The overlay's private geschichte connection."
  [overlay]
  (some-> (repo-overlay-system overlay) :conn))

(defn publish-repo-overlay!
  "Hand an overlay's work back as the canonical branch `branch-kw`.

   `branch-kw` must ALREADY exist canonically — `branch-repo!` creates it — and
   this is a plain fast-forward onto it. Deliberately not `:create? true`: the
   overlay's own refs carry `refs/heads/main` and not the fork's name, so
   `publish!` resolving the tip from the workspace's refs finds nothing and
   throws `\"Workspace ref has no publishable commit\"`. Passing the tip
   explicitly is what makes it work (measured 2026-07-29).

   `publish!` copies only reachable immutable commit metadata plus that one ref,
   and CAS-checks the canonical ref inside the merge transaction, so two agents
   publishing concurrently cannot silently overwrite each other. Returns nil
   when the overlay has no commits."
  [scope overlay-or-conn branch-kw]
  (when-let [sys (get-repo-system scope)]
    ;; either a GeschichteOverlay or a bare workspace connection — the ctx-fork
    ;; path reaches its workspace through `ygg/system`, which hands back the
    ;; inner system, never the overlay record
    (let [ws-conn (or (repo-overlay-conn overlay-or-conn) overlay-or-conn)
          tip (some-> (grepo/head-commit ws-conn) :geschichte.commit/id)]
      (when tip
        (gws/publish! (:conn sys) ws-conn
                      {:ref (str "refs/heads/" (name branch-kw)) :commit tip})
        (log/log! {:level :info :id ::repo-overlay-published
                   :data {:scope (str scope) :branch branch-kw :commit (str tip)}})
        (str tip)))))

(defn refresh-repo-overlay!
  "Fast-forward an overlay to trunk — \"update your branch\" — so a fork is
   reviewed against what trunk is NOW rather than what it was when the fork
   opened.

   This is the verb that makes a conflict resolvable at all, and it is
   load-bearing rather than convenient here: several agents working one small
   repository collide far more often than a human team does, and geschichte's
   conflict granularity is a whole FILE. Running it inside the agent's own
   overlay is what puts a conflict in front of the party that can actually fix
   it — the agent, mid-turn, holding the file text and the task it was doing.

   `workspace/advance!` refuses a dirty workspace rather than clobbering
   uncommitted work, so a failure here means \"commit first\", not \"lost work\".
   Returns `{:ok? true}`, or `{:ok? false :error msg}` — a refusal is an
   ordinary outcome to report to the agent, not an exception to propagate."
  [scope overlay]
  (if-let [sys (get-repo-system scope)]
    (try (gws/advance! (:conn sys) (repo-overlay-conn overlay))
         {:ok? true}
         (catch Exception e
           (log/log! {:level :info :id ::repo-overlay-refresh-refused
                      :data {:scope (str scope) :error (.getMessage e)}})
           {:ok? false :error (.getMessage e)}))
    {:ok? false :error "repo not registered"}))

(defn discard-repo-overlay!
  "Release an overlay and reclaim its workspace branch.

   The ONLY thing that reclaims it. Skipping this leaks a datahike branch that
   no later GC finds, because the branch is named `:geschichte.workspace/<uuid>`
   and every branch sweeper in this namespace matches trunk-derived names."
  [overlay]
  (when overlay
    (try (yp/discard! overlay)
         :ok
         (catch Exception e
           (log/log! {:level :warn :id ::repo-overlay-discard-failed
                      :msg "Overlay workspace leaked — nothing else reclaims it"
                      :data {:branch (str (:workspace-branch overlay))
                             :error (.getMessage e)}})
           :failed))))

(defn repo-workspace-branches
  "Every physical workspace branch in repo `scope` — the overlay branches, not
   the user-visible geschichte refs. The inventory a reaper works from."
  [scope]
  (when-let [sys (get-repo-system scope)]
    (set (gws/list (:conn sys)))))

(defn reap-repo-workspaces!
  "Remove every workspace branch of repo `scope` except those in `keep`.

   `keep` is the set of `:workspace-branch` keys belonging to overlays that are
   still live. Passing it is what makes this safe to run against a working
   system: `geschichte.workspace/remove!` does not ask whether anyone is holding
   the branch, so an unfiltered sweep would delete a fork an agent is writing
   into right now. Returns the number removed."
  [scope keep]
  (if-let [sys (get-repo-system scope)]
    (let [conn (:conn sys)
          victims (remove (set keep) (gws/list conn))]
      (doseq [b victims]
        (try (gws/remove! conn b)
             (catch Exception e
               (log/log! {:level :warn :id ::workspace-reap-failed
                          :data {:scope (str scope) :branch (str b)
                                 :error (.getMessage e)}}))))
      (when (seq victims)
        (log/log! {:level :info :id ::workspaces-reaped
                   :data {:scope (str scope) :removed (count victims)}}))
      (count victims))
    0))

(defn reap-orphan-repo-workspaces!
  "BOOT-time sweep of every room repo's workspace branches.

   Every one of them is an orphan at this point, and that is not an assumption
   but a consequence: an overlay is an in-memory record, so nothing that
   survived the restart holds any of these branches. Work that mattered was
   PUBLISHED to a canonical branch before the process died and is untouched
   here; work that was not published died with the process that could reach it.

   Must run BEFORE any overlay is opened — it keeps nothing. `reap-repo-
   workspaces!` is the live-safe variant that takes an explicit keep set.

   This exists because nothing else reclaims these branches. They are named
   `:geschichte.workspace/<uuid>`, which `internal-branch?` does not match, and
   `gc-internal-branches!` resolves systems through `get-kb-system`, which never
   resolves a repo — so before this, a restart leaked every un-discarded
   workspace permanently. Returns the number removed."
  []
  (let [n (reduce
           (fn [n {:room/keys [id]}]
             (+ n (reduce (fn [m {:keys [path]}]
                            (+ m (try (reap-repo-workspaces! path #{})
                                      (catch Exception e
                                        (log/log! {:level :warn :id ::workspace-sweep-failed
                                                   :data {:room (str id) :path (str path)
                                                          :error (.getMessage e)}})
                                        0))))
                          0 (rooms/room-repos id))))
           0 (sdb/all-rooms))]
    (log/log! {:level :info :id ::orphan-workspaces-reaped
               :msg "Boot sweep of orphaned repo workspace branches"
               :data {:removed n}})
    n))

;; ============================================================================
;; Repo forks as CONTEXT forks — the write path.
;;
;; `ygg/fork!` overlay-forks every registered system on a context and hands back
;; a ForkHandle; `ygg/discard-fork!` discards each of them. That cascade is the
;; API, not a hazard: minting an overlay of one's own ON TOP of it and then
;; discarding only that one is what leaks (measured, and self-inflicted).
;;
;; What spindel does NOT provide is publication to a NAMED branch —
;; `merge-fork!` merges to the parent, i.e. lands on trunk, which is the one
;; thing a proposal must not do before a human has seen it. That gap is
;; `publish-repo-overlay!` above, and it is the whole reason this layer exists
;; rather than simmis just calling `merge-fork!`.
;; ============================================================================

(defn open-repo-fork!
  "Fork the execution context of the room owning repo `scope`.

   Returns `{:handle … :child-ctx … :scope …}`, or nil when the repo does not
   resolve. The caller MUST eventually `close-repo-fork!` — a ForkHandle is an
   in-memory object, so a process that dies holding one leaves its workspace
   branch for the boot sweep."
  ([scope] (open-repo-fork! scope nil))
  ([scope branch-kw]
   (when-let [path (repo-scope-path scope)]
     (when-let [room-ctx (room-ctx-owning-repo path)]
       (let [fh (binding [rtc/*execution-context* room-ctx] (ygg/fork!))
             fork {:handle fh :child-ctx (:child-ctx fh) :scope scope}]
         ;; Start the workspace ON the fork's branch.
         ;;
         ;; A workspace forks at the repository's CURRENT ref — trunk — so
         ;; without this a REOPENED fork would begin from trunk and its publish
         ;; would move the branch backwards, replacing the very work the
         ;; reviewer just commented on rather than extending it. On a
         ;; first-minted branch this is a no-op (the branch is trunk head), so
         ;; there is no special case: always land on the branch being worked.
         (when branch-kw
           (try
             (binding [rtc/*execution-context* (:child-ctx fh)]
               (yp/checkout (ygg/system (repo-system-id path)) branch-kw))
             (catch Exception e
               (log/log! {:level :warn :id ::repo-fork-checkout-failed
                          :msg "Fork workspace stayed on trunk — its publish would rewind the branch"
                          :data {:scope (str scope) :branch (str branch-kw)
                                 :error (.getMessage e)}}))))
         (log/log! {:level :info :id ::repo-fork-opened
                    :data {:scope (str scope) :branch (some-> branch-kw str)}})
         fork)))))

(defn repo-fork-conn
  "The forked workspace's geschichte connection — where this fork's writes go.

   Resolved through `ygg/system` on the child context, which returns the
   EFFECTIVE system (the overlay's inner one), so no overlay record is needed."
  [{:keys [child-ctx scope]}]
  (when (and child-ctx scope)
    (when-let [path (repo-scope-path scope)]
      (binding [rtc/*execution-context* child-ctx]
        (:conn (ygg/system (repo-system-id path)))))))

(defn repo-fork-workspace
  "`{:workspace … :filesystem …}` for a repo fork, in the shape dvergr's tool
   context takes.

   These are EXPLICIT options to `dvergr.tools/make-context`, which resolves a
   default only `when-not` they are supplied — and every file verb
   (`workspace-read`, `workspace-write!`, `workspace-glob`) reads `:filesystem`
   straight off that map. So handing these two keys to a tool call redirects its
   filesystem and NOTHING else: the room's messages, the KB and the book all
   resolve elsewhere and are untouched. That precision is the point — a broader
   context binding would silently move reads nobody asked to move."
  [fork]
  (when-let [conn (repo-fork-conn fork)]
    (let [gsub (requiring-resolve 'dvergr.substrate.geschichte/filesystem)
          workspace {:conn conn
                     :repository {:conn conn :config (:config @conn)}}]
      {:workspace workspace :filesystem (gsub workspace)})))

(defn publish-repo-fork!
  "Hand a fork's work over as the canonical branch `branch-kw`. Returns the
   published commit id as a string, or nil when the fork committed nothing."
  [{:keys [scope] :as fork} branch-kw]
  (when-let [conn (repo-fork-conn fork)]
    (publish-repo-overlay! scope conn branch-kw)))

(defn refresh-repo-fork!
  "Update-from-trunk for a code fork: import trunk's newer commits into the
   agent's workspace so its change is reviewed against the repository as it is
   NOW.

   Deliberately NOT `ygg/merge-fork-from-parent!`, which is a SILENT NO-OP here
   and therefore worse than nothing. Measured 2026-07-29: with trunk and the
   fork editing one file differently it reported success and changed nothing.
   The cause is the ref-namespace isolation that makes overlays useful in the
   first place — a workspace holds its OWN `refs/heads/main`, so merging the
   parent's `:main` resolves to the fork's own `:main`, i.e. a branch merged
   into itself. Every geschichte overlay would report a clean refresh forever.

   `workspace/advance!` is the honest primitive: it imports the canonical
   commits and fast-forwards, and REFUSES when the fork has diverged rather than
   pretending. Returns `{:ok? true}`, or `{:ok? false :error msg}` — a refusal
   is a result to hand back to the agent, not an exception to propagate.

   LIMIT, stated because the refusal is not a bug: a diverged fork cannot be
   refreshed by any primitive geschichte currently offers. A real merge would
   need trunk's commits present in the workspace first, and `advance!` — the
   thing that imports them — is exactly what declines. Until that gap is closed
   the diverged case resolves at review time instead."
  [{:keys [scope] :as fork}]
  (if-let [sys (and scope (get-repo-system scope))]
    (if-let [ws-conn (repo-fork-conn fork)]
      (try (gws/advance! (:conn sys) ws-conn)
           {:ok? true}
           (catch Exception e
             (log/log! {:level :info :id ::repo-fork-refresh-refused
                        :data {:scope (str scope) :error (.getMessage e)}})
             {:ok? false :error (.getMessage e)}))
      {:ok? false :error "fork has no workspace"})
    {:ok? false :error "repo not registered"}))

(defn close-repo-fork!
  "Discard a fork's overlays across every system it forked. The ONLY correct
   cleanup — `ygg/discard-fork!` reaps each one, so nothing is left behind."
  [{:keys [handle child-ctx]}]
  (when handle
    (try (binding [rtc/*execution-context* (or (:parent-ctx child-ctx) child-ctx)]
           (ygg/discard-fork! handle))
         :ok
         (catch Exception e
           (log/log! {:level :warn :id ::repo-fork-discard-failed
                      :msg "Fork overlays leaked — the boot sweep will reap them"
                      :data {:error (.getMessage e)}})
           :failed))))

;; ============================================================================
;; The neutral fork API — ONE dispatch on system-type, here.
;;
;; Everything above comes in pairs: `branch-head-id`/`repo-head-id`,
;; `kb-diff`/`repo-diff`, `merge-kb!`/`merge-repo!`. Callers used to pick a side
;; themselves, which meant every new comparison was another place to forget one
;; — and forgetting is silent. A repo scope asked for `branch-head-id` answers
;; nil, nil is also what "no such branch" looks like, so a cache key collapses
;; and a tier classifies an empty delta as trivial.
;;
;; So the choice is made once, here, and callers name a system-type instead of a
;; backend. The pairs stay: the two really do differ (trunk is `:main` vs `:db`;
;; a repo's conflicts are real while the datahike adapter reports none; a merged
;; repo branch needs `:force?` to delete), and collapsing them into shared
;; bodies would hide exactly those differences.
;; ============================================================================

;; Defined further down, with the conflict-text machinery it needs. Declared
;; here because the neutral API below is what callers use, and a conflict is
;; only actionable once its three sides are resolved to text.
(declare repo-conflict-details)

(defn- repo? [system-type] (= :repo system-type))
(defn- world? [system-type] (= :world system-type))

(defn- world-call [sym & args]
  (apply (requiring-resolve
          (symbol "is.simm.ops.run-world-proposals" (name sym)))
         args))

(defn trunk-of
  "The branch a fork of this system-type lands on."
  [scope system-type]
  (cond
    (world? system-type) :world/trunk
    (repo? system-type) (repo-trunk scope)
    :else :db))

(defn head-id
  "Head commit of `branch`, as a string, or nil when it does not resolve."
  [scope system-type branch]
  (cond
    (world? system-type) (world-call 'head-id scope branch)
    (repo? system-type) (repo-head-id scope branch)
    :else (branch-head-id scope branch)))

(defn fork-branch!
  "Create a fork branch off trunk. `{:branch :parent :existed?}`."
  [scope system-type slug]
  (cond
    (world? system-type)
    (throw (ex-info "Adopted worlds are created by Run promotion, not branch minting"
                    {:scope scope :system-type system-type}))

    (repo? system-type) (branch-repo! scope slug)
    :else (branch-kb! scope slug)))

(defn delta
  "A branch's change set against its trunk, in whatever shape its adapter
   reports — a `DatahikeDiff` of datoms or a `GeschichteDiff` of files.

   `:patch? false` is honoured only by the repo side, where rendering a patch
   means reading every changed blob on both sides; a caller that only needs
   counts should pass it."
  ([scope system-type branch] (delta scope system-type branch nil))
  ([scope system-type branch opts]
   (let [trunk (trunk-of scope system-type)]
     (cond
       (world? system-type) (world-call 'delta scope)
       (repo? system-type) (repo-diff scope trunk branch opts)
       :else (kb-diff scope trunk branch)))))

(defn conflicts-with-trunk
  "Conflict descriptors between `branch` and its trunk, asked in the SAME
   argument order the merge uses — the adapter names the first side `ours`, and
   landing merges the branch INTO trunk, so trunk is ours. Asking branch-first
   still detects every conflict (the three-tree compare is symmetric) but labels
   the sides backwards, which stays invisible until something renders them.

   For a repo these are real. The datahike adapter still answers `[]`, so a KB
   fork's silence means \"not computed\", not \"clean\"."
  [scope system-type branch]
  (let [trunk (trunk-of scope system-type)]
    (or (cond
          (world? system-type)
          (world-call 'conflicts scope)

          (repo? system-type)
          ;; Real conflicts only, and that is now geschichte's own answer: a file
          ;; both sides touched in different places merges while planning, so it
          ;; never reaches here. Reporting one would have a reviewer arbitrate a
          ;; merge that is about to happen anyway.
          (repo-conflict-details scope branch)

          :else (kb-conflicts scope trunk branch))
        [])))

(defn land!
  "Merge `branch` into its trunk. Throws with the backend's reason when the
   merge cannot be made — a repo refuses an unresolved conflict."
  [scope system-type branch]
  (let [trunk (trunk-of scope system-type)]
    (cond
      (world? system-type) (world-call 'settle-scope! scope :merge)
      (repo? system-type) (merge-repo! scope branch trunk)
      :else (merge-kb! scope branch trunk))))

(defn drop-branch!
  "Delete a fork branch."
  [scope system-type branch]
  (cond
    (world? system-type) (world-call 'settle-scope! scope :discard)
    (repo? system-type) (discard-repo-branch! scope branch)
    :else (discard-kb-branch! scope branch)))

(def ^:private max-conflict-side
  "Per-side character ceiling on a conflicting file's text. A reviewer looking
   at a conflict needs to see WHICH change collides, not the whole file."
  4000)

(defn repo-conflict-details
  "Conflicts between `branch` and trunk, each carrying the actual TEXT of the
   three sides.

   Every one of these is REAL — a file the two sides changed in the same place.
   geschichte merges the rest line by line while planning (0.1.14), so what
   arrives here is already the set a person has to decide. It did not use to be:
   simmis filtered the file-level set itself, and dropping that filter is the
   point of upstreaming the content merge.

   `yggdrasil.types/Conflict` reports content IDs, sizes and modes — enough for
   a merge planner and useless to a person. A card that says \"1 conflicting
   change\" tells a reviewer that something is wrong and nothing about what, so
   the only move left is to refuse it. Resolving the ids to text is what turns
   the conflict into something a human can act on.

   Reads at the three COMMITS, not the worktree: `ours` must be trunk as it is
   now and `theirs` the fork as it is now, and a worktree read would answer
   whatever the repository's shared current ref happens to be checked out at.

   Binary and over-long sides come back as nil rather than as mojibake —
   `:ours-skipped?` and friends say which, so the view can be honest about it."
  [scope branch]
  (when-let [sys (get-repo-system scope)]
    (let [conn (:conn sys)
          trunk (repo-trunk scope)
          text-at (fn [commit path]
                    (when commit
                      (try
                        (let [bs (grepo/read-at conn commit path)
                              s (when bs (gbytes/decode-utf8 bs))]
                          (cond
                            (nil? s) nil
                            ;; git's own heuristic — a NUL means not text
                            (str/includes? s "\u0000") nil
                            (> (count s) max-conflict-side)
                            (str (subs s 0 max-conflict-side) "\n… (truncated)")
                            :else s))
                        (catch Exception _ nil))))
          ours-c (some-> (yp/snapshot-meta sys trunk) :geschichte.commit/id)
          theirs-c (some-> (yp/snapshot-meta sys branch) :geschichte.commit/id)
          base-c (some-> (yp/common-ancestor sys trunk branch) parse-uuid)]
      (vec (for [c (or (repo-conflicts scope trunk branch) [])
                 :let [p (:path c)
                       ours (text-at ours-c p)
                       theirs (text-at theirs-c p)
                       base (text-at base-c p)]]
             {:path p
              :base base :ours ours :theirs theirs
              ;; a side present in the plan but unreadable as text is NOT the
              ;; same as a side that does not exist, and a reviewer deciding a
              ;; merge must be able to tell them apart
              :base-skipped? (and (some? (:base c)) (nil? base))
              :ours-skipped? (and (some? (:ours c)) (nil? ours))
              :theirs-skipped? (and (some? (:theirs c)) (nil? theirs))})))))

(defn merge-trunk-into-fork!
  "Bring trunk's current state INTO a fork branch, as a real merge commit.

   The update-from-trunk verb for a DIVERGED fork — the case `advance!` refuses,
   because a fast-forward is not available once both sides have moved. Here the
   two are merged: `plan` reconciles the trees and, for a file both sides
   changed, the lines within it; `prepare-merge!` stages that plan with trunk as
   the second parent; the commit that follows carries both histories.

   Recording it as a MERGE is the part that is easy to get wrong. The three-tree
   compare is against the merge BASE, so a fork whose content was fixed up by
   hand still conflicts until its HISTORY contains trunk. Afterwards the base IS
   trunk's tip and the merge is clean by construction.

   All or nothing. A partial application would leave the fork half-reconciled
   against a trunk it still does not merge with, which is worse than not trying:
   the next reviewer reads a branch nobody wrote. So a plan that does not come
   back clean changes nothing, and the paths a person must decide are returned.

   Returns `{:resolved n :unresolved [{:path :base :ours :theirs} …]}`, where
   `n` counts the files geschichte reconciled line by line."
  [scope branch]
  (let [fork (open-repo-fork! scope branch)]
    (try
      (let [w (repo-fork-conn fork)
            ours-id (:geschichte.commit/id (grepo/head-commit w))
            ;; trunk as the WORKSPACE sees it. The workspace copied refs at fork
            ;; time and this fork was opened a moment ago, so this is trunk's
            ;; current tip — the commit the conflicts were computed against.
            theirs-id (get (grepo/refs w) (str "refs/heads/" (name (repo-trunk scope))))
            plan (when theirs-id (gmerge/plan w ours-id theirs-id))]
        (if-not (:clean? plan)
          {:resolved 0 :unresolved (repo-conflict-details scope branch)}
          (let [n (count (:merged plan))]
            (grepo/prepare-merge! w plan)
            (grepo/commit! w {:message (str "merge trunk into " (name branch)
                                            (when (pos? n)
                                              (str " (" n " file(s) reconciled line by line)")))
                              :author "simmis <merge@simm.is>"})
            (publish-repo-fork! fork branch)
            (log/log! {:level :info :id ::trunk-merged-into-fork
                       :data {:scope (str scope) :branch (str branch) :files n}})
            {:resolved n :unresolved []})))
      (catch Exception e
        (log/log! {:level :warn :id ::trunk-merge-failed
                   :msg "Merging trunk into the fork failed — the fork is untouched"
                   :data {:scope (str scope) :branch (str branch)
                          :error (.getMessage e)}})
        {:resolved 0 :unresolved (repo-conflict-details scope branch)
         :error (.getMessage e)})
      (finally (close-repo-fork! fork)))))
