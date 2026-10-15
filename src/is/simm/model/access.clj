(ns is.simm.model.access
  "The authorization seam: identity, and the one `can?` predicate.

   IDENTITY — `authenticated-party-id` derives the acting party from the
   connection's validated JWT principal, never from client-supplied ids.
   Every RPC that acts on behalf of someone starts here.

   AUTHORIZATION — `can?` answers (subject, action, resource) as plain
   datalog over the existing relations (party –:room/parties→ room, room
   –:grant/*→ system). It is enforced at two planes: the DATA plane
   (konserve-sync scope authorization in `runtimes/web.clj`) and the
   CONTROL plane (the RPCs in `ops/`). Both call this one predicate, so a
   later swap to eacl's traversal engine happens behind this signature.

   The action vocabulary and the fork model it encodes are written up in
   `doc/authority.md`; `permission-satisfies?` below is its
   normative statement.

   Covered by `access_test`, `access_control_plane_test`,
   `runtimes/data_plane_test`, and the authority suite under
   `test/is/simm/authority/`, which runs the predicate and both enforcement
   planes together against real grants (doc/authority.md)."
  (:require [is.simm.distributed-scope :as ds]
            [is.simm.model.system-db :as system-db]
            [datahike.api :as d]
            [taoensso.telemere :as log]))

(defn authenticated-party-id
  "The party uuid of the CONNECTION's authenticated principal (JWT :sub).

   kabel-auth's websocket middleware attaches :kabel/principal to every
   authenticated message; distributed-scope's invoke-on-peer binds it to
   ds/*principal* around handler invocation. This is the authoritative
   sender identity for spin-remote handlers — never trust a client-passed
   party id for writes.

   IMPORTANT: the dynamic binding does NOT survive into async spin
   continuations — capture the result at the top of the handler body.

   Throws when the connection is unauthenticated or the principal carries
   no party uuid."
  []
  (let [p (ds/require-principal)
        pid (some-> (:sub p) parse-uuid)]
    (or pid
        (throw (ex-info "Authenticated principal has no party uuid :sub"
                        {:principal (dissoc p :exp :iat)})))))

;; =============================================================================
;; can? — the authorization decision
;; =============================================================================
;;
;; Datalog over the shared system DB now, eacl's IAuthorization traversal behind
;; the same signature later (the two overlapping relation models — KB
;; owner/shared-with and room membership + room→grant→system — are consulted
;; together during the transition, per the chosen shim). Deny-by-default: an
;; unknown subject, scope, or relation yields false.

(defn subject->party-uuid
  "Normalize a subject to a party uuid, or nil for anonymous.
   Accepts a principal map ({:sub uuid-str}), a uuid, or a uuid string."
  [subject]
  (cond
    (uuid? subject) subject
    (map? subject) (some-> (:sub subject) (as-> s (if (uuid? s) s (parse-uuid (str s)))))
    (string? subject) (parse-uuid subject)
    :else nil))

(defn- scope-uuid [scope]
  (cond (uuid? scope) scope
        (string? scope) (parse-uuid scope)
        :else nil))

(defn- as-uuid [x] (cond (uuid? x) x (string? x) (parse-uuid x) :else nil))

;; Every lookup here answers nil for a nil key. datahike reads a nil `:in`
;; binding as UNBOUND, so the bare query would match an arbitrary entity — in an
;; authorization module that is an allow, not a deny (see system-db/require-id!).

(defn- kb-eid-by-scope [db scope]
  (when (some? scope)
    (d/q '[:find ?e . :in $ ?s :where [?e :kb/db-scope ?s]] db scope)))

(defn- room-eid-by-scope [db scope]
  ;; a room owns two stores: its category-S content DB and dvergr's message DB
  (when (some? scope)
    (or (d/q '[:find ?e . :in $ ?s :where [?e :room/content-db-scope ?s]] db scope)
        (d/q '[:find ?e . :in $ ?s :where [?e :room/db-scope ?s]] db scope))))

(defn- kb-eid-by-id [db kb-id]
  (when-let [id (as-uuid kb-id)]
    (d/q '[:find ?e . :in $ ?id :where [?e :kb/id ?id]] db id)))

(defn- room-eid-by-id [db room-id]
  (when-let [id (as-uuid room-id)]
    (d/q '[:find ?e . :in $ ?id :where [?e :room/id ?id]] db id)))

(defn- mail-eid-by-id [db account-id]
  (when-let [id (as-uuid account-id)]
    (d/q '[:find ?e . :in $ ?id :where [?e :mail-account/id ?id]] db id)))

(defn- admin? [db party-uuid]
  (and (some? party-uuid)
       (= :admin (d/q '[:find ?role . :in $ ?p
                        :where [?e :party/id ?p] [?e :party/role ?role]] db party-uuid))))

(defn- proposal-room-eid
  "The room ENTITY a proposal is scoped to (via :proposal/room), or nil.

   `:proposal/room` is a `:db.type/uuid` VALUE, not a ref (system_db.clj), so
   the datom binds a room uuid — it still has to be resolved to an entity like
   every other id here. Returning it directly made `party-can-access-room?`
   pass a uuid to `d/pull`, which THROWS `:entity-id/syntax` rather than
   denying; that took down every policed proposal RPC (accept!/dismiss!/diff!)
   while `list-proposals!` — policed as `:authenticated` — kept working."
  [db proposal-id]
  (when-let [id (as-uuid proposal-id)]
    (some->> (d/q '[:find ?ruid . :in $ ?pid
                    :where [?p :proposal/id ?pid] [?p :proposal/room ?ruid]] db id)
             (room-eid-by-id db))))

(def permission-order
  "Grant permission values, weakest first. Each implies every value before it.

   `:merge` is the ONE value added for the fork/review model, deliberately
   BETWEEN `:read-write` and `:owner`. It is not a reinterpretation: every
   existing value keeps exactly the meaning it had, and `:read-write` — roughly
   all production grants — keeps write while losing nothing it previously
   conferred, because merge was never a distinct action before.

   `:owner` is what dvergr's `attach!` records for a room's OWN systems (its
   repo, KB and messages store), which is why a room can still land forks into
   the stores it owns without anyone granting anything."
  [:read :read-write :merge :owner])

(defn- permission-satisfies?
  "Does a grant `permission` permit `action`?

   The action vocabulary, fixed here (see `doc/authority.md`):

     :read   — see it
     :write  — change it, which in the fork model means WRITE A FORK
     :merge  — land a fork onto trunk. NOT implied by :write. This is the
               layer Forgejo calls a branch-protection merge whitelist and
               GitLab calls \"allowed to merge\": separate from, and not a
               consequence of, write access to the repository.
     :grant  — manage authority over the resource. Owner only, and not
               delegable — see the antisymmetry note in the design doc.

   DENY-BY-DEFAULT ON THE ACTION ITSELF. An action this function does not
   recognise is refused, not waved through. The previous `:read`-or-anything
   fallthrough was safe only while the vocabulary had two entries; with :merge
   and :grant in it, a typo'd or newly-invented action must not silently inherit
   read's permissiveness. A grant with no explicit permission defaults to :read
   at the call site."
  [permission action]
  (case action
    :read  (boolean permission)
    :write (contains? #{:read-write :merge :owner} permission)
    :merge (contains? #{:merge :owner} permission)
    (:grant :admin) (= :owner permission)
    false))

(def shared-with-permission
  "The grant permission a direct share (`:kb/shared-with`) confers: write, not
   merge, not grant.

   A share is a bare uuid on the KB — it records no permission of its own — and
   until P1 `can?` read it as OWNER, so a sharee could merge, re-share and
   delete. That was never what sharing was documented to give (\"full (read +
   write)\" in this namespace, \"Shared with you\" in the UI), and it made
   `share-kb!` an escalation: a `:read-write` room grantee shared the KB with
   themselves and came out owner-equivalent. A share now means exactly
   `:read-write`, the permission a room attach confers by default, so the two
   ways of reaching a KB answer the same question the same way. Landing to
   trunk and handing out authority stay with the owner."
  :read-write)

(defn- authority-override?
  "The global `:admin` role may MANAGE authority over a KB or drive — share,
   attach, revoke, delete — without owning it (`:grant`/`:admin` actions only).
   It does not read through this: an admin who wants a KB's content gets it by
   granting it, which is recorded. Admin can already rewrite any agent's
   prompt and join any experiment room, so this widens nothing in kind."
  [db party-uuid action]
  (and (contains? #{:grant :admin} action) (admin? db party-uuid)))

(defn- party-can-access-kb?
  "True if `party-uuid` may `action` the KB entity `kb-eid`:
   - the KB owner: everything;
   - the global `:admin` role: `:grant`/`:admin` only (`authority-override?`);
   - a party in `:kb/shared-with`: what `shared-with-permission` (`:read-write`)
     permits — read and write, not merge, not grant;
   - transitive: a member of a room whose grant on the KB's system PERMITS the
     action. The grant's :grant/permission refines this: :read grants read only,
     :read-write grants both; a grant with no explicit permission defaults to
     :read (so it never silently confers write)."
  [db party-uuid kb-eid action]
  (let [{:kb/keys [owner shared-with system-id]}
        (d/pull db [:kb/owner :kb/shared-with :kb/system-id] kb-eid)]
    (boolean
     (or (= owner party-uuid)
         (authority-override? db party-uuid action)
         (and (contains? (set shared-with) party-uuid)
              (permission-satisfies? shared-with-permission action))
         (and system-id
              ;; party → room membership → grant(s) → this KB's system;
              ;; any grant whose permission satisfies `action` authorizes.
              (let [grant-eids (d/q '[:find [?g ...] :in $ ?sid ?p
                                      :where
                                      [?sys :system/id ?sid]
                                      [?room :room/parties ?pe] [?pe :party/id ?p]
                                      [?g :grant/room ?room] [?g :grant/system ?sys]]
                                    db system-id party-uuid)]
                (some (fn [g]
                        (permission-satisfies?
                         (:grant/permission (d/pull db [:grant/permission] g) :read)
                         action))
                      grant-eids)))))))

(defn- party-can-access-room?
  "May `party-uuid` perform `action` on room entity `room-eid`?

   Membership is the gate for `:read` and `:write` — a room IS its conversation,
   so a member who could not write could not speak, and the fork/review model
   depends on writes being generous. `:merge`, `:grant` and `:admin` are the
   irreversible half and stop at the OWNER: landing to trunk and handing out
   authority are not consequences of having been added to a chat.
   Changing the membership itself is `:grant` (rpc-policy), because a member
   reaches every KB, drive and repo the room is granted.

   This function took no action argument until now, which is why 21 `:action W`
   RPC policies resolved to \"is a member\" — and why kabel's new publish gate
   (kabel#14) would otherwise have authorized writes exactly as permissively as
   reads."
  [db party-uuid room-eid action]
  (let [{:room/keys [owner parties]}
        (d/pull db [{:room/owner [:party/id]} {:room/parties [:party/id]}] room-eid)
        owner? (= party-uuid (:party/id owner))]
    (boolean
     (case action
       (:merge :grant :admin) owner?
       (or owner? (some #(= party-uuid (:party/id %)) parties))))))

(defn- system-eid-by-id [db system-id]
  (when (some? system-id)
    (d/q '[:find ?e . :in $ ?sid :where [?e :system/id ?sid]] db system-id)))

(defn- party-can-access-system?
  "True if `party-uuid` may `action` the dvergr registry system `system-id`.

   The registry's own resources — a room's repo, messages store and agent data
   DBs — are reached ONLY through room grants, so membership of a room holding a
   sufficient grant is the whole rule for those.

   A KB IS THE EXCEPTION, and it is delegated. A KB has both a `:kb/db-scope`
   and a `:kb/system-id`, and `can?` resolves the two in different branches — so
   before this delegation the same KB gave DIFFERENT answers depending on which
   uuid named it: by db-scope the owner fell back to `:kb/owner` and could
   merge, by system-id this grant-only rule refused them on their own KB.
   Measured 2026-07-30. It failed CLOSED, so it was never a hole, and B1 is what
   made it visible — until `:merge` existed the `:read-write` grant satisfied
   `:write` and both paths agreed by accident. An authorization answer must not
   depend on which name the caller used for the resource, least of all before
   grants become signed facts that have to mean one thing.

   This branch exists because a proposal fork's scope can name one of them. A
   code fork's `:proposal.fork/scope` is a repo's `:system/id`, and with no case
   for it `can?` fell through to `false` — so Accept on a code fork was refused
   as `:not-authorized` even for the owner of the room that owns the repo.
   Failing closed was right; failing closed forever was not."
  [db party-uuid system-id action]
  (if-let [kb-eid (d/q '[:find ?e . :in $ ?sid :where [?e :kb/system-id ?sid]] db system-id)]
    (party-can-access-kb? db party-uuid kb-eid action)
    (let [grants (d/q '[:find ?room ?perm :in $ ?sid
                        :where
                        [?sys :system/id ?sid]
                        [?g :grant/system ?sys]
                        [?g :grant/room ?room]
                        [?g :grant/permission ?perm]]
                      db system-id)]
      (boolean
       (some (fn [[room-eid perm]]
             (and (permission-satisfies? (or perm :read) action)
                  ;; REACHABILITY, not permission — hence `:read` and not
                  ;; `action`. The question here is only "is this party in the
                  ;; room that holds the grant", so a grant held by a room the
                  ;; party is not in confers nothing. The STRENGTH of the access
                  ;; comes from the grant, checked just above; asking the room
                  ;; for `action` too would require room OWNERSHIP to merge a
                  ;; fork into a repo the room was explicitly granted.
                  (party-can-access-room? db party-uuid room-eid :read)))
             grants)))))

(defn- drive-eid-by-id [db drive-id]
  (when-let [id (as-uuid drive-id)]
    (d/q '[:find ?e . :in $ ?id :where [?e :drive/id ?id]] db id)))

(defn- party-can-access-drive?
  "A drive is a KB without the sharing list: its owner (`:drive/owner`) may do
   anything, the `:admin` role may manage authority over it
   (`authority-override?`), and everyone else reaches it only through a room
   grant on its registry system (`party-can-access-system?` — the same rule as a
   room's repo)."
  [db party-uuid drive-eid action]
  (let [{:drive/keys [owner system-id]} (d/pull db [:drive/owner :drive/system-id] drive-eid)]
    (boolean
     (or (= owner party-uuid)
         (authority-override? db party-uuid action)
         (and system-id (party-can-access-system? db party-uuid system-id action))))))

(defn- open-fork-rows
  "[fork proposal] pairs (pulled) for the OPEN forks matching `where-args`.
   A fork is open while its own status is unset and its proposal is `:open`;
   once decided its branch is merged or deleted and it is an ordinary branch
   name again."
  [db rows]
  ;; The fork's own status is filtered HERE, not with `missing?` in the query:
  ;; on datahike 0.8.1903 `missing?` reported `:proposal.fork/status` missing on
  ;; a fork that had it (position-dependent, not a cache), so a decided fork
  ;; would have stayed locked — replikativ/datahike#1116.
  (keep (fn [[f p]]
          (let [fork (d/pull db [:proposal.fork/author :proposal.fork/authority-scope
                                 :proposal.fork/scope :proposal.fork/status] f)]
            (when (nil? (:proposal.fork/status fork))
              [fork (d/pull db [:proposal/author] p)])))
        rows))

(defn- open-fork
  "The open proposal fork that branch `branch` of store `scope` holds, or nil."
  [db scope branch]
  (when (and (some? scope) (some? branch))
    (first (open-fork-rows
            db (d/q '[:find ?f ?p
                      :in $ ?s ?b
                      :where
                      [?f :proposal.fork/scope ?s] [?f :proposal.fork/branch ?b]
                      [?p :proposal/forks ?f] [?p :proposal/status :open]]
                    db scope (name branch))))))

(defn- trunk-branch? [branch]
  (or (nil? branch) (= "db" (name branch))))

(declare can?)

(defn- fork-controller?
  "Does an open proposal fork belong to `party-uuid`? Its author
   (`:proposal.fork/author`, else the filer `:proposal/author` for rows filed
   before per-fork authors existed), or whoever may decide it: `:merge` on the
   fork's authority scope — the scope `ops.proposals` checks before it lands
   the fork."
  [db party-uuid [fork proposal]]
  (boolean
   (or (= party-uuid (or (:proposal.fork/author fork) (:proposal/author proposal)))
       (can? db party-uuid :merge
             (or (:proposal.fork/authority-scope fork) (:proposal.fork/scope fork))))))

(defn- party-can-access-branch?
  "May `party-uuid` `action` branch `branch` of the store `scope`?

   Trunk (nil / `:db`) and any branch that is not an open proposal fork answer
   exactly as the store does. An OPEN PROPOSAL FORK belongs to its author and to
   whoever may decide it (doc/proposals-and-time-travel.md §5,
   `fork-controller?`): writing to it, or discarding it, needs `action` on the
   store AND control of the fork. Without this, any writer of the KB could edit
   or delete another party's proposal while it was under review, and the
   reviewer would accept something its author never wrote. Reading a fork is
   reading the store."
  [db party-uuid scope branch action]
  (let [scope (scope-uuid scope)]
    (boolean
     (and (can? db party-uuid action scope)
          (or (= :read action)
              (trunk-branch? branch)
              (let [row (open-fork db scope branch)]
                (or (nil? row) (fork-controller? db party-uuid row))))))))

(defn- proposal-open-forks [db proposal-id]
  (when-let [id (as-uuid proposal-id)]
    (open-fork-rows db (d/q '[:find ?f ?p :in $ ?pid
                              :where [?p :proposal/id ?pid] [?p :proposal/status :open]
                              [?p :proposal/forks ?f]]
                            db id))))

(defn- party-can-decide-fork?
  "`{:proposal-fork {:proposal p :scope s :branch b}}`: the proposal's room
   decides who may see and act on it (`action` on the room, as `{:proposal p}`),
   and for anything but `:read` the named fork must be an open fork of THAT
   proposal controlled by `party-uuid` (`fork-controller?`). Dismissing a fork
   deletes its branch — a contribution under review is its author's to withdraw
   and its deciders' to refuse, not any room member's."
  [db party-uuid {:keys [proposal scope branch]} action]
  (boolean
   (when-let [room-eid (proposal-room-eid db proposal)]
     (and (party-can-access-room? db party-uuid room-eid action)
          (or (= :read action)
              (let [scope (scope-uuid scope)]
                (when-let [row (and scope branch
                                    (some (fn [[fork :as row]]
                                            (when (= scope (:proposal.fork/scope fork)) row))
                                          (open-fork-rows
                                           db (d/q '[:find ?f ?p :in $ ?pid ?s ?b
                                                     :where [?p :proposal/id ?pid]
                                                     [?p :proposal/status :open]
                                                     [?p :proposal/forks ?f]
                                                     [?f :proposal.fork/scope ?s]
                                                     [?f :proposal.fork/branch ?b]]
                                                   db (as-uuid proposal) scope (name branch)))))]
                  (fork-controller? db party-uuid row))))))))

(defn- party-can-decide-proposal?
  "`{:proposal-forks p}`, every open fork of a proposal at once (dismissing the
   whole ForkSet): the room as for `{:proposal p}`, and beyond `:read` either
   the proposal's author (the filer withdraws their own proposal) or control of
   EVERY open fork — a reviewer who may refuse each contribution may refuse
   them together, and nobody else may."
  [db party-uuid proposal-id action]
  (boolean
   (when-let [room-eid (proposal-room-eid db proposal-id)]
     (and (party-can-access-room? db party-uuid room-eid action)
          (or (= :read action)
              (= party-uuid (d/q '[:find ?a . :in $ ?pid
                                   :where [?p :proposal/id ?pid] [?p :proposal/author ?a]]
                                 db (as-uuid proposal-id)))
              (every? #(fork-controller? db party-uuid %)
                      (proposal-open-forks db proposal-id)))))))

(defn- party-can-revoke-attachment?
  "`{:attachment {:room r :kb k}}` / `{:attachment {:room r :drive d}}` — may
   `party-uuid` take KB/drive k/d out of room r? The room's owner may (`:grant`
   on the room: deciding what the room holds), and so may whoever holds `:grant`
   on the resource itself — its owner, or `:admin` — who may always withdraw
   their own resource from a room, member of it or not. Any other action is
   refused: attaching is checked as room `:write` + resource `:grant` instead."
  [db party-uuid {:keys [room kb drive]} action]
  (boolean
   (and (= :grant action)
        (or (when-let [e (room-eid-by-id db room)] (party-can-access-room? db party-uuid e :grant))
            (cond kb    (when-let [e (kb-eid-by-id db kb)] (party-can-access-kb? db party-uuid e :grant))
                  drive (when-let [e (drive-eid-by-id db drive)] (party-can-access-drive? db party-uuid e :grant))
                  :else false)))))

(defn- party-can-access-mail?
  "Mail is private to its owner unless explicitly attached to one of the
   party's rooms. Room grants are read-only by default."
  [db party-uuid mail-eid action]
  (let [{:mail-account/keys [owner system-id]}
        (d/pull db [:mail-account/owner :mail-account/system-id] mail-eid)]
    (boolean
     (or (= owner party-uuid)
         (and system-id
              (some (fn [grant-eid]
                      (permission-satisfies?
                       (:grant/permission (d/pull db [:grant/permission] grant-eid) :read)
                       action))
                    (d/q '[:find [?g ...] :in $ ?sid ?party
                           :where
                           [?system :system/id ?sid]
                           [?room :room/parties ?member]
                           [?member :party/id ?party]
                           [?g :grant/room ?room]
                           [?g :grant/system ?system]]
                         db system-id party-uuid)))))))

(defn can?
  "May `subject` perform `action` on `resource`?

   subject  — principal map {:sub uuid-str} | party uuid | uuid string | nil (anon)
   action   — :read | :write | :merge | :grant | :admin (see
              `permission-satisfies?`). Isolation is enforced (owner/shared/
              member/grant); within a tenant, :write on a KB reached only via a
              room grant additionally requires that grant to be :read-write.
   resource — one of:
     • a store scope (uuid|string)  — data plane + block-remote :db-scope
     • {:kb <kb-id>}                — a KB by :kb/id (control plane)
     • {:drive <drive-id>}          — a drive by :drive/id
     • {:branch {:scope s :branch b}} — one branch of a store: the store's
                                      answer, narrowed on an open proposal
                                      fork to its author and its deciders
     • {:proposal-fork {:proposal p :scope s :branch b}} — one open fork of a
                                      proposal: the room, then fork control
     • {:proposal-forks <proposal-id>} — every open fork of a proposal
     • {:attachment {:room r :kb|:drive id}} — revoking an attachment (:grant)
     • {:room <room-id>}            — a room by :room/id (control plane)
     • {:settings <party>} | {:self <party>} — that party's own settings
     • :admin                       — requires the :admin role
     • :authenticated               — any authenticated party (e.g. creates)

   Deny-by-default: an unauthenticated subject, an unresolvable resource, or a
   missing relation is a denial.

   TWO ARITIES, and the db one is the real function. The decision is a pure
   function of a system-DB VALUE; the 3-arity only supplies that value from the
   ambient connection. Keeping the `system-db/get-conn` read out of the decision
   is what makes the seam testable at all — a test can build a db and ask it
   directly, instead of installing a global — and it is the one change that lets
   the same decision later be taken against a db that is NOT the local mutable
   system DB: an `as-of` db (what was authorized at the time a write was made), a
   replicated projection of grant entries, or a peer's copy. Prefer the 4-arity
   at new call sites; see `doc/authority.md`."
  ([subject action resource]
   (can? (some-> (system-db/get-conn) deref) subject action resource))
  ([db subject action resource]
   (let [party (subject->party-uuid subject)]
     (boolean
      (cond
        (nil? party) false                              ; anonymous: deny (public later)
        (nil? db) false                                 ; no system DB: deny

        (= resource :authenticated) true
        (= resource :admin) (admin? db party)

        (map? resource)
        (cond
          (contains? resource :kb)
          (when-let [e (kb-eid-by-id db (:kb resource))] (party-can-access-kb? db party e action))
          (contains? resource :room)
          (when-let [e (room-eid-by-id db (:room resource))] (party-can-access-room? db party e action))
          (contains? resource :mail)
          (when-let [e (mail-eid-by-id db (:mail resource))] (party-can-access-mail? db party e action))
          (contains? resource :proposal-fork)
          (party-can-decide-fork? db party (:proposal-fork resource) action)
          (contains? resource :proposal-forks)
          (party-can-decide-proposal? db party (:proposal-forks resource) action)
          (contains? resource :attachment)
          (party-can-revoke-attachment? db party (:attachment resource) action)
          (contains? resource :proposal)
          (when-let [e (proposal-room-eid db (:proposal resource))] (party-can-access-room? db party e action))
          (contains? resource :drive)
          (when-let [e (drive-eid-by-id db (:drive resource))] (party-can-access-drive? db party e action))
          (contains? resource :branch)
          (let [{:keys [scope branch]} (:branch resource)]
            (party-can-access-branch? db party scope branch action))
          (contains? resource :settings) (= party (as-uuid (:settings resource)))
          (contains? resource :self) (= party (as-uuid (:self resource)))
          :else false)

        ;; a bare scope (store topic)
        :else
        (when-let [scope (scope-uuid resource)]
          (cond
            (kb-eid-by-scope db scope) (party-can-access-kb? db party (kb-eid-by-scope db scope) action)
            (room-eid-by-scope db scope) (party-can-access-room? db party (room-eid-by-scope db scope) action)
            ;; a dvergr registry `:system/id` — a room's repo / messages store /
            ;; agent data DB. Last, so it can only ever answer for a scope the two
            ;; store-shaped resources above did not claim.
            (system-eid-by-id db scope) (party-can-access-system? db party scope action)
            :else false)))))))

;; =============================================================================
;; Control-plane policy — the single gate for every remote fn
;; =============================================================================
;;
;; distributed-scope's invoke-on-peer calls `authorize-remote` before running
;; ANY handler in its registry: block-remote's and the spindel
;; defn-spin-remote endpoints. Deny-by-default: a fn with no policy entry is
;; refused.
;;
;; datahike.kabel's handlers (`dispatch`, `create-database`,
;; `delete-database`) are NOT in that registry. They register in
;; `kabel.remote/functions`, which the server never serves (it composes
;; distributed-scope's middleware, not `kabel.remote/serve`), so they are
;; unreachable and have no row: if they ever became reachable through this
;; gate they would be refused. A raw transact into a room's message store
;; must stay refused (it would forge messages); see raw-paths-test.

(defn normalize-remote-name
  "The semantic name a policy is keyed by. spindel's defn-spin-remote registers
   under a mangled `<ns>/spin-remote-<name>-<idx>`; block-remote registers
   under `<prefix>/<name>`. Reduce both to `<name>`
   (keeping the trailing ! that spin names carry)."
  [fn-name]
  (let [n (name (symbol fn-name))]
    (or (second (re-matches #"spin-remote-(.+)-\d+" n)) n)))

(def rpc-policy
  "normalized-name -> {:action kw :resource (fn [arg-map] resource)}.
   `resource` returns a value `can?` understands: a scope, {:kb id}, {:room id},
   {:branch {:scope s :branch b}}, {:proposal id}, {:self party},
   {:settings party}, :admin, or :authenticated.

   A row binds ONE argument. A handler that acts on a SECOND client-supplied id
   (the party to add, the agent to remove, the KB to attach) checks it with the
   helpers at the end of this namespace — see doc/authority.md, \"Who may change
   authority\"."
  (let [scope    (fn [k] (fn [a] (get a k)))
        room     (fn [k] (fn [a] {:room (get a k)}))
        kb       (fn [k] (fn [a] {:kb (get a k)}))
        self     (fn [k] (fn [a] {:self (get a k)}))
        settings (fn [k] (fn [a] {:settings (get a k)}))
        proposal (fn [k] (fn [a] {:proposal (get a k)}))
        ;; revoking a room's attachment of the KB/drive named by `k`
        attachment (fn [k] (fn [a] {:attachment
                                    (cond-> {:room (:room-id-str a)}
                                      (= k :kb-id-str) (assoc :kb (get a k))
                                      (= k :drive-id-str) (assoc :drive (get a k)))}))
        mail     (fn [k] (fn [a] {:mail (get a k)}))
        always   (fn [r] (fn [_] r))
        ;; a write that names a branch of a store: on an open proposal fork it
        ;; is the fork author's or a decider's (see `party-can-access-branch?`)
        branch   (fn [s b] (fn [a] {:branch {:scope (get a s) :branch (get a b)}}))
        R :read W :write M :merge G :grant A :admin]
    {;; --- block-remote (page/block edits, all carry :db-scope) ---
     "create-block"            {:action W :resource (branch :db-scope :branch-kw)}
     "create-sibling-after"    {:action W :resource (branch :db-scope :branch-kw)}
     "delete-block"            {:action W :resource (branch :db-scope :branch-kw)}
     "ensure-page"             {:action W :resource (branch :db-scope :branch-kw)}
     "indent-block"            {:action W :resource (branch :db-scope :branch-kw)}
     "outdent-block"           {:action W :resource (branch :db-scope :branch-kw)}
     "move-block-up"           {:action W :resource (branch :db-scope :branch-kw)}
     "move-block-down"         {:action W :resource (branch :db-scope :branch-kw)}
     "rename-page"             {:action W :resource (branch :db-scope :branch-kw)}
     "toggle-block-collapsed"  {:action W :resource (branch :db-scope :branch-kw)}
     "update-block-collapsed"  {:action W :resource (branch :db-scope :branch-kw)}
     "update-block-content"    {:action W :resource (branch :db-scope :branch-kw)}
     "update-block-order"      {:action W :resource (branch :db-scope :branch-kw)}
     "find-page-by-title"      {:action R :resource (scope :db-scope)}
     ;; Metadata rosters stay lean; the selected Datahike store is registered
     ;; just before its client subscribes. The scope grant is the same gate as
     ;; the subsequent data-plane subscription.
     "prepare-store!"          {:action R :resource (scope :db-scope-str)}
     ;; page types + properties. `add-property` DECLARES a datahike attribute,
     ;; which is append-only — it was reachable from any browser before these
     ;; moved server-side, with no check at all.
     "add-type"                {:action W :resource (branch :db-scope :branch-kw)}
     "remove-type"             {:action W :resource (branch :db-scope :branch-kw)}
     "add-property"            {:action W :resource (branch :db-scope :branch-kw)}
     "remove-property"         {:action W :resource (branch :db-scope :branch-kw)}
     "save-property"           {:action W :resource (branch :db-scope :branch-kw)}
     ;; --- branching (KB by :db-scope-str) ---
     "branch-kb!"              {:action W :resource (scope :db-scope-str)}
     ;; deleting a branch: someone else's open proposal fork is not yours to
     ;; delete while it is under review
     "discard-kb-branch!"      {:action W :resource (branch :db-scope-str :branch-kw)}
     ;; landing a branch onto trunk — the one branching op that is not a write
     ;; to a fork but a decision about what trunk becomes
     "merge-kb!"               {:action M :resource (scope :db-scope-str)}
     "kb-commit-graph!"        {:action R :resource (scope :db-scope-str)}
     "kb-diff!"                {:action R :resource (scope :db-scope-str)}
     "list-kb-branches!"       {:action R :resource (scope :db-scope-str)}
     ;; --- KB by id ---
     "create-kb!"              {:action W :resource (self :party-id-str)}
     ;; Deleting a KB and changing who reaches it directly are :grant — the
     ;; KB's owner (or the :admin role, `authority-override?`). Policed :write
     ;; they let any room grantee delete the KB, revoke other sharees, or share
     ;; it with themselves.
     "delete-kb!"              {:action G :resource (kb :kb-id-str)}
     "load-kb-details!"        {:action R :resource (kb :kb-id-str)}
     "share-kb!"               {:action G :resource (kb :kb-id-str)}
     "unshare-kb!"             {:action G :resource (kb :kb-id-str)}
     ;; --- room ops (by :room-id-str) ---
     ;; A member may bring their OWN agent in, reconfigure and take it out
     ;; again; which agent a call may name is the handler's
     ;; (`access/require-agent-manageable!`).
     "add-agent-to-room!"      {:action W :resource (room :room-id-str)}
     "update-agent-assignment!" {:action W :resource (room :room-id-str)}
     "remove-agent-from-room!" {:action W :resource (room :room-id-str)}
     ;; Membership IS authority: a new member reaches every KB, drive and repo
     ;; the room is granted. Changing it is :grant, which stops at the owner.
     ;; The handler also checks WHO is added (`access/require-enlistable!`).
     "add-room-party!"         {:action G :resource (room :room-id-str)}
     "remove-room-party!"      {:action G :resource (room :room-id-str)}
     ;; Attaching grants the room's members the KB/drive: :write on the room
     ;; here, and :grant on the KB/drive itself in the handler.
     "attach-drive-to-room!"   {:action W :resource (room :room-id-str)}
     ;; Detaching is the room owner's (what the room holds) or the resource
     ;; owner's (withdrawing their own), member of the room or not.
     "detach-drive-from-room!" {:action G :resource (attachment :drive-id-str)}
     "attach-kb-to-room!"      {:action W :resource (room :room-id-str)}
     "detach-kb-from-room!"    {:action G :resource (attachment :kb-id-str)}
     "delete-drive-node!"      {:action W :resource (room :room-id-str)}
     "mkdir-drive-path!"       {:action W :resource (room :room-id-str)}
     "put-drive-file!"         {:action W :resource (room :room-id-str)}
     "dispatch-message!"       {:action W :resource (room :room-id-str)}
     "ensure-room!"            {:action W :resource (room :room-id-str)}
     "run-world-live!"         {:action R :resource (room :room-id-str)}
     "cancel-room-run!"        {:action W :resource (room :room-id-str)}
     "promote-room-run-world!" {:action W :resource (room :room-id-str)}
     ;; spend is egress, authorized in advance by the owner, not by anyone who
     ;; can speak in the room
     "update-room-budget!"     {:action G :resource (room :room-id-str)}
     "load-room-details!"      {:action R :resource (room :room-id-str)}
     ;; meeting token: joining the room's call = reading the room
     "mint-video-token!"       {:action R :resource (room :room-id-str)}
     "room-app-status!"        {:action R :resource (room :room-id-str)}
     ;; screens gallery: the caller's OWN screen stream (owner-scoped now, no
     ;; room arg) — any authed party may read their own; the handler keys on the
     ;; principal, so there is no cross-user access to police here.
     "search-screens!"         {:action R :resource (always :authenticated)}
     ;; owner controls over one's OWN captures (handler keys on the principal)
     "delete-screenshot!"      {:action W :resource (always :authenticated)}
     "list-recordings!"        {:action R :resource (always :authenticated)}
     "delete-recording!"       {:action W :resource (always :authenticated)}
     ;; web-page archive (owner's own captures; handler keys on the principal)
     "search-pages!"           {:action R :resource (always :authenticated)}
     "delete-page!"            {:action W :resource (always :authenticated)}
     ;; screen-share grants: toggling a room's window onto your own stream is a
     ;; write against that room — gated to members (my RPC re-checks membership).
     "open-screen-grant!"      {:action W :resource (room :room-id-str)}
     "close-screen-grant!"     {:action W :resource (room :room-id-str)}
     "screen-grant-heartbeat!" {:action W :resource (room :room-id-str)}
     "load-room-drive!"        {:action R :resource (room :room-id-str)}
     ;; --- room create + rooms list (self) ---
     ;; the initial members pass the same check as `add-room-party!`'s party
     "create-room!"            {:action W :resource (self :creator-id-str)}
     "load-rooms!"             {:action R :resource (self :party-id-str)}
     ;; --- drives (create) ---
     "create-drive!"           {:action W :resource (always :authenticated)}
     ;; --- contacts (own list) ---
     "add-contact!"            {:action W :resource (self :party-id-str)}
     "add-contact-by-handle!"  {:action W :resource (self :party-id-str)}
     "remove-contact!"         {:action W :resource (self :party-id-str)}
     ;; --- parties list (UI) ---
     "list-parties!"           {:action R :resource (always :authenticated)}
     ;; --- agents (powerful: rewrites prompts/models) ---
     "update-agent-config!"    {:action A :resource (always :admin)}
     ;; --- sandbox (room-scoped) ---
     ;; Evaluating code in a room's sandbox is the room's FULL capability —
     ;; every KB, repo and tool its agents hold — so it is the owner's
     ;; (:merge), not every member's. The handler records the principal as the
     ;; author, never a client-supplied id.
     "eval-in-room!"           {:action M :resource (room :room-uuid-str)}
     ;; whether to SHOW eval controls; the handler answers `can?` for the
     ;; caller, so a non-member gets false rather than a refusal
     "can-eval-in-room!"       {:action R :resource (always :authenticated)}
     "get-room-state!"         {:action R :resource (room :room-uuid-str)}
     ;; --- proposals (room-scoped via :proposal/room) ---
     "accept-proposal!"        {:action W :resource (proposal :proposal-id-str)}
     ;; Dismissing discards fork branches: the room gate, and control of the
     ;; forks (their authors, or :merge on their scope) — or, for the whole
     ;; proposal, being its filer. See `party-can-decide-fork?`.
     "dismiss-proposal!"       {:action W :resource (fn [a] {:proposal-forks (:proposal-id-str a)})}
     ;; accepting carries the room gate; ops.proposals additionally requires
     ;; :merge on each fork's own scope before it lands one
     "accept-fork!"            {:action W :resource (proposal :proposal-id-str)}
     "dismiss-fork!"           {:action W :resource (fn [a] {:proposal-fork
                                                              {:proposal (:proposal-id-str a)
                                                               :scope (:scope-str a)
                                                               :branch (:branch a)}})}
     ;; Commenting is a WRITE on the proposal's room, not a read: it puts words
     ;; on a shared record under the author's name, and `request-changes!` also
     ;; puts a contributor back to work. Room membership is the gate, the same
     ;; one that decides who may accept — a reviewer who may land a change may
     ;; certainly ask for a smaller one instead.
     "comment-on-proposal!"    {:action W :resource (proposal :proposal-id-str)}
     "request-changes!"        {:action W :resource (proposal :proposal-id-str)}
     ;; Policed as :authenticated, NOT {:proposal id}: the whole point is to
     ;; answer for an id the caller may not be able to see, and a
     ;; {:proposal id} policy would refuse before the handler could give the
     ;; deliberately-ambiguous nil. `visible-status` does the authorization
     ;; itself and conflates "absent" with "not yours".
     "proposal-status!"        {:action R :resource (always :authenticated)}
     "proposal-diff!"          {:action R :resource (proposal :proposal-id-str)}
     "proposal-checks!"        {:action R :resource (proposal :proposal-id-str)}
     "list-proposals!"         {:action R :resource (always :authenticated)}
     ;; --- tasks (aggregate; the handler derives the party from the principal
     ;; and ops.tasks filters each KB through can?, so there is no id to police
     ;; here — but a STATUS WRITE targets one KB and is gated on that scope) ---
     "list-tasks!"             {:action R :resource (always :authenticated)}
     ;; the handler derives the party from the principal and filters each book
     ;; through can?, so there is no client-supplied id to police here
     "load-position!"          {:action R :resource (always :authenticated)}
     ;; one team's book: a member of the room reads it (ops.book-report
     ;; re-checks the same grant against the party it is reporting for)
     "load-book!"              {:action R :resource (room :room-id-str)}
     "load-account-ledger!"    {:action R :resource (room :room-id-str)}
     "load-feed!"              {:action R :resource (always :authenticated)}
     "set-task-status!"        {:action W :resource (scope :scope-str)}
     ;; --- settings (own) ---
     "load-settings!"          {:action R :resource (settings :party-id-str)}
     "save-env-var!"           {:action W :resource (settings :party-id-str)}
     "delete-env-var!"         {:action W :resource (settings :party-id-str)}
     "save-preferred-model!"   {:action W :resource (settings :party-id-str)}
     "save-syntax-pref!"       {:action W :resource (settings :party-id-str)}
     ;; --- mail knowledge sources ---
     "load-mail-accounts!"     {:action R :resource (always :authenticated)}
     "test-mail-connection!"   {:action W :resource (always :authenticated)}
     "save-mail-account!"      {:action W :resource (always :authenticated)}
     "sync-mail-account!"      {:action W :resource (mail :account-id-str)}
     "load-mail-folders!"      {:action R :resource (mail :account-id-str)}
     "load-mail-page!"         {:action R :resource (mail :account-id-str)}
     "load-mail-message!"      {:action R :resource (mail :account-id-str)}
     ;; --- admin (role check; ignores the client-supplied party id) ---
     "load-admin-data!"        {:action R :resource (always :admin)}
     "load-schedules!"         {:action R :resource (always :authenticated)}
     ;; --- unread badges + per-room notify level (handler derives party from the principal) ---
     "load-unread-counts!"     {:action R :resource (always :authenticated)}
     "mark-read!"              {:action W :resource (always :authenticated)}
     "load-notify-prefs!"      {:action R :resource (always :authenticated)}
     "set-notify-pref!"        {:action W :resource (always :authenticated)}
     ;; --- experiments-remote ---
     ;; A leaderboard is the room's: a member reads it. An experiment room
     ;; dvergr made has no members, so joining one is for admins.
     "load-leaderboard!"       {:action R :resource (room :room-id-str)}
     "join-experiment-room!"   {:action A :resource (always :admin)}}))

(defn authorize-remote
  "The `:authorize-fn` for distributed-scope's invoke-on-peer. Consulted for
   every network-inbound remote call before its handler runs. Deny-by-default:
   an unpoliced fn, an unauthenticated principal, or a failed `can?` is refused."
  [principal fn-name arg-map]
  (if-let [{:keys [action resource]} (get rpc-policy (normalize-remote-name fn-name))]
    (let [fname (normalize-remote-name fn-name)
          ;; RESOLVE ONCE. The denial log below used to call `(resource
          ;; arg-map)` a second time, outside the guard — measured, not
          ;; supposed. It did not break the decision (telemere realizes `:data`
          ;; on its handler thread, so the second throw never reached this
          ;; caller and the fn still returned false), but it destroyed the
          ;; diagnostic: for a resolver that throws, `::rpc-denied` lost its
          ;; `:resource` field and telemere emitted a handler error in its
          ;; place — exactly the entry you need to tell a policy typo from a
          ;; genuine refusal. It also assumed every resolver is pure and cheap.
          resolved (try {:value (resource arg-map)}
                        (catch Exception e {:error e}))
          ;; Deny-by-default has to survive a BROKEN check, not just a false
          ;; one. A resolver that throws (e.g. a uuid reaching `d/pull` as an
          ;; entity id) otherwise escapes as "Remote invocation error" — the
          ;; caller cannot tell a bug from a refusal, and clients that only
          ;; branch on a nil result hang forever. Fail closed and log loudly.
          allowed (if-let [e (:error resolved)]
                    (do (log/log! {:level :error :id ::rpc-check-failed
                                   :msg "resource resolver threw — denying"
                                   :data {:fn fname
                                          :action action
                                          :sub (:sub principal)
                                          :error (.getMessage ^Exception e)
                                          :ex-data (ex-data e)}})
                        false)
                    (try (can? principal action (:value resolved))
                         (catch Exception e
                           (log/log! {:level :error :id ::rpc-check-failed
                                      :msg "authorization check threw — denying"
                                      :data {:fn fname
                                             :action action
                                             :sub (:sub principal)
                                             :error (.getMessage e)
                                             :ex-data (ex-data e)}})
                           false)))]
      (when-not allowed
        (log/log! {:level :warn :id ::rpc-denied
                   :data {:fn fname
                          :action action
                          :has-principal (some? (:sub principal))
                          :sub (:sub principal)
                          ;; nil when the resolver threw — the ::rpc-check-failed
                          ;; entry above carries the reason
                          :resource (:value resolved)}}))
      allowed)
    (do
      (log/log! {:level :warn :id ::rpc-no-policy
                 :msg "remote fn has no authorization policy — denied"
                 :data {:fn (normalize-remote-name fn-name) :raw (str fn-name)}})
      false)))

(defn warn-on-sender-mismatch!
  "Transitional guard: log when a client-supplied sender id disagrees with
   the authenticated principal. Lets us find and fix remaining handlers
   that still thread client identity before enforcement hardens."
  [client-sender-id auth-pid handler-id]
  (when (and client-sender-id (not= (str client-sender-id) (str auth-pid)))
    (log/log! {:level :warn :id ::sender-mismatch
               :msg "Client-supplied sender differs from authenticated principal"
               :data {:handler handler-id
                      :client-sender (str client-sender-id)
                      :principal-party (str auth-pid)}})))

;; =============================================================================
;; Argument checks — the second id a handler acts on
;; =============================================================================
;;
;; A policy row binds ONE argument (the room, the KB). Several handlers act on a
;; SECOND id the client supplies — the party to add, the agent to remove, the KB
;; to attach — and the authority suite found every one of them unchecked. These
;; are the checks, written once; a handler calls them before it changes
;; anything. Each throws `{:type :not-authorized}` (the shape the gate's refusal
;; has), so a client cannot tell a handler refusal from a gate refusal and need
;; not.

(defn- refuse! [msg data]
  (log/log! {:level :warn :id ::handler-refused :msg msg :data data})
  (throw (ex-info (str "not authorized: " msg) (assoc data :type :not-authorized))))

(defn- current-db [] (some-> (system-db/get-conn) deref))

(defn require!
  "Throw `:not-authorized` unless the connection's principal may `action`
   `resource` (`can?`). For the second id a handler acts on, e.g. the KB in
   `attach-kb-to-room!`."
  [action resource]
  (let [p (ds/require-principal)]
    (when-not (can? (current-db) p action resource)
      (refuse! "argument check" {:action action :resource resource :sub (:sub p)}))))

(defn- party-facts [db party-uuid]
  (when-let [e (and (some? party-uuid)
                    (d/q '[:find ?e . :in $ ?p :where [?e :party/id ?p]] db party-uuid))]
    (let [{:keys [party/type actor/kind party/owner]}
          (d/pull db [:party/type :actor/kind {:party/owner [:party/id]}] e)]
      {:kind (or kind type) :owner (:party/id owner)})))

(defn enlistable?
  "May `subject` make `party-uuid` a member of a room `subject` controls?

   A human, yes: rooms invite people (the new-room picker and room settings
   offer every human). An AGENT only when `subject` owns it, or is `:admin`: an
   agent answers on its owner's model and budget, so putting someone else's
   agent in your room spends their money and points their agent at your
   conversation. An unknown party, no."
  [db subject party-uuid]
  (let [me (subject->party-uuid subject)
        {:keys [kind owner] :as facts} (party-facts db party-uuid)]
    (boolean
     (and me facts
          (case kind
            :agent (or (= me owner) (admin? db me))
            true)))))

(defn require-enlistable!
  "Throw `:not-authorized` unless the principal may add every party in
   `party-uuids` to a room (`enlistable?`). Used by `add-room-party!` and by
   `create-room!` for its initial members."
  [party-uuids]
  (let [p (ds/require-principal)
        db (current-db)]
    (doseq [pid party-uuids]
      (when-not (enlistable? db p pid)
        (refuse! "party may not be added to a room by this principal"
                 {:party (str pid) :sub (:sub p)})))))

(defn agent-manageable?
  "May `subject` manage `agent-uuid` in room `room-id` — remove it
   (`remove-agent-from-room!`) or change its role and response policy
   (`update-agent-assignment!`)? Only an AGENT that IS in that room, and only by
   the room's owner (`:grant` on the room), the agent's owner, or `:admin`. A
   human is never removed this way — that is `remove-room-party!`, and the
   removal handler also deletes the party."
  [db subject room-id agent-uuid]
  (let [me (subject->party-uuid subject)
        {:keys [kind owner]} (party-facts db agent-uuid)
        in-room? (some? (d/q '[:find ?r . :in $ ?rid ?a
                               :where [?r :room/id ?rid] [?r :room/parties ?pe]
                               [?pe :party/id ?a]]
                             db (as-uuid room-id) agent-uuid))]
    (boolean
     (and me (= :agent kind) in-room?
          (or (= me owner)
              (admin? db me)
              (can? db me :grant {:room room-id}))))))

(defn require-agent-manageable!
  "Throw `:not-authorized` unless `agent-manageable?`. Returns whether the
   principal may also DELETE the agent party once it is in no room: only its
   owner or `:admin` — a room owner removes another party's agent from their
   room, not from the world."
  [room-id agent-uuid]
  (let [p (ds/require-principal)
        db (current-db)
        me (subject->party-uuid p)]
    (when-not (agent-manageable? db p room-id agent-uuid)
      (refuse! "not an agent of this room the principal may manage"
               {:room (str room-id) :agent (str agent-uuid) :sub (:sub p)}))
    (or (= me (:owner (party-facts db agent-uuid))) (admin? db me))))

;; =============================================================================
;; The sandbox's KBs — what an agent's code may reach in the room it runs in
;; =============================================================================

(defn sandbox-kb-allows?
  "May `subject`, acting in room `room-id`'s sandbox, `action` (`:read` or
   `:write`) the KB `kb-id`? THE check for every KB handle or KB write the
   sandbox vocabulary hands out (`kb/*`, `knowledge_add`). All three must hold:

   - `subject` may `action` the ROOM (a member, for :read and :write);
   - THIS room holds a grant on the KB that permits `action` — the sandbox is
     the room's, so a KB attached elsewhere is not addressable from it, and a
     `:read` grant gives no write handle;
   - `subject` may `action` the KB (`can?`).

   For an agent the subject is the agent party, which reaches the KB through the
   room's grants — what the agent-tool authorization (`tool-authorization`)
   checks for the room. There is no fallback: a room with no grant reaches no
   KB. (The room CREATOR's first KB used to be handed out to a room without
   one, so any member's code wrote the owner's private KB on trunk.)"
  [db subject room-id kb-id action]
  (let [party (subject->party-uuid subject)
        room-eid (room-eid-by-id db room-id)
        kb-eid (kb-eid-by-id db kb-id)]
    (boolean
     (and party room-eid kb-eid (contains? #{:read :write} action)
          (party-can-access-room? db party room-eid action)
          (some (fn [perm] (permission-satisfies? (or perm :read) action))
                (d/q '[:find [?perm ...] :in $ ?r ?k
                       :where [?k :kb/system-id ?sid] [?s :system/id ?sid]
                       [?g :grant/system ?s] [?g :grant/room ?r]
                       [(get-else $ ?g :grant/permission :read) ?perm]]
                     db room-eid kb-eid))
          (party-can-access-kb? db party kb-eid action)))))

(defn require-sandbox-kb!
  "Throw `:not-authorized` unless `sandbox-kb-allows?`. Called on every use, not
   once at join: detaching a KB or removing the agent from the room revokes its
   handles without rebuilding the sandbox."
  [subject room-id kb-id action]
  (when-not (sandbox-kb-allows? (current-db) subject room-id kb-id action)
    (refuse! "sandbox may not reach this KB"
             {:action action :room (str room-id) :kb (str kb-id)
              :subject (str (subject->party-uuid subject))})))

(defn may-eval-in-room?
  "May `subject` evaluate code in room `room-id`'s sandbox? The `eval-in-room!`
   policy row's question (`:merge` on the room — the owner), asked so the UI can
   hide the control from everyone else."
  [db subject room-id]
  (can? db subject :merge {:room room-id}))
