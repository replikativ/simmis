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
   `doc/permissions-and-forks.md`; `permission-satisfies?` below is its
   normative statement.

   Covered by `access_test`, `access_control_plane_test` and
   `runtimes/data_plane_test`. Known gap: no INTEGRATION test runs the
   predicate and the two enforcement planes together against real grants."
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

(defn- kb-eid-by-scope [db scope]
  (d/q '[:find ?e . :in $ ?s :where [?e :kb/db-scope ?s]] db scope))

(defn- room-eid-by-scope [db scope]
  ;; a room owns two stores: its category-S content DB and dvergr's message DB
  (or (d/q '[:find ?e . :in $ ?s :where [?e :room/content-db-scope ?s]] db scope)
      (d/q '[:find ?e . :in $ ?s :where [?e :room/db-scope ?s]] db scope)))

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
  (= :admin (d/q '[:find ?role . :in $ ?p
                   :where [?e :party/id ?p] [?e :party/role ?role]] db party-uuid)))

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

   The action vocabulary, fixed here (see `doc/permissions-and-forks.md`):

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

(defn- party-can-access-kb?
  "True if `party-uuid` may `action` (:read | :write) the KB entity `kb-eid`:
   - direct: KB owner or in :kb/shared-with — full (read + write), OR
   - transitive: a member of a room whose grant on the KB's system PERMITS the
     action. The grant's :grant/permission refines this: :read grants read only,
     :read-write grants both; a grant with no explicit permission defaults to
     :read (so it never silently confers write)."
  [db party-uuid kb-eid action]
  (let [{:kb/keys [owner shared-with system-id]}
        (d/pull db [:kb/owner :kb/shared-with :kb/system-id] kb-eid)]
    (boolean
     (or (= owner party-uuid)
         (contains? (set shared-with) party-uuid)
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
  (d/q '[:find ?e . :in $ ?sid :where [?e :system/id ?sid]] db system-id))

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
   action   — :read | :write | :admin. Isolation is enforced (owner/shared/
              member/grant); within a tenant, :write on a KB reached only via a
              room grant additionally requires that grant to be :read-write.
   resource — one of:
     • a store scope (uuid|string)  — data plane + block-remote :db-scope
     • {:kb <kb-id>}                — a KB by :kb/id (control plane)
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
   at new call sites; see `doc/permissions-and-forks.md`."
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
          (contains? resource :proposal)
          (when-let [e (proposal-room-eid db (:proposal resource))] (party-can-access-room? db party e action))
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
;; ANY handler (block-remote, the spindel defn-spin-remote endpoints, and the
;; datahike.kabel dispatch/create/delete handlers all register in the one
;; registry). Deny-by-default: a fn with no policy entry is refused.

(defn normalize-remote-name
  "The semantic name a policy is keyed by. spindel's defn-spin-remote registers
   under a mangled `<ns>/spin-remote-<name>-<idx>`; block-remote and
   datahike.kabel register under `<prefix>/<name>`. Reduce both to `<name>`
   (keeping the trailing ! that spin names carry)."
  [fn-name]
  (let [n (name (symbol fn-name))]
    (or (second (re-matches #"spin-remote-(.+)-\d+" n)) n)))

(def rpc-policy
  "normalized-name -> {:action kw :resource (fn [arg-map] resource)}.
   `resource` returns a value `can?` understands: a scope, {:kb id}, {:room id},
   {:proposal id}, {:self party}, {:settings party}, :admin, or :authenticated."
  (let [scope    (fn [k] (fn [a] (get a k)))
        room     (fn [k] (fn [a] {:room (get a k)}))
        kb       (fn [k] (fn [a] {:kb (get a k)}))
        self     (fn [k] (fn [a] {:self (get a k)}))
        settings (fn [k] (fn [a] {:settings (get a k)}))
        proposal (fn [k] (fn [a] {:proposal (get a k)}))
        mail     (fn [k] (fn [a] {:mail (get a k)}))
        always   (fn [r] (fn [_] r))
        R :read W :write M :merge A :admin]
    {;; --- datahike.kabel writer ---
     "dispatch"                {:action W :resource (scope :store-id)}
     "create-database"         {:action W :resource (always :authenticated)}
     "delete-database"         {:action A :resource (always :admin)}
     ;; --- block-remote (page/block edits, all carry :db-scope) ---
     "create-block"            {:action W :resource (scope :db-scope)}
     "create-sibling-after"    {:action W :resource (scope :db-scope)}
     "delete-block"            {:action W :resource (scope :db-scope)}
     "ensure-page"             {:action W :resource (scope :db-scope)}
     "indent-block"            {:action W :resource (scope :db-scope)}
     "outdent-block"           {:action W :resource (scope :db-scope)}
     "move-block-up"           {:action W :resource (scope :db-scope)}
     "move-block-down"         {:action W :resource (scope :db-scope)}
     "rename-page"             {:action W :resource (scope :db-scope)}
     "toggle-block-collapsed"  {:action W :resource (scope :db-scope)}
     "update-block-collapsed"  {:action W :resource (scope :db-scope)}
     "update-block-content"    {:action W :resource (scope :db-scope)}
     "update-block-order"      {:action W :resource (scope :db-scope)}
     "find-page-by-title"      {:action R :resource (scope :db-scope)}
     ;; Metadata rosters stay lean; the selected Datahike store is registered
     ;; just before its client subscribes. The scope grant is the same gate as
     ;; the subsequent data-plane subscription.
     "prepare-store!"          {:action R :resource (scope :db-scope-str)}
     ;; page types + properties. `add-property` DECLARES a datahike attribute,
     ;; which is append-only — it was reachable from any browser before these
     ;; moved server-side, with no check at all.
     "add-type"                {:action W :resource (scope :db-scope)}
     "remove-type"             {:action W :resource (scope :db-scope)}
     "add-property"            {:action W :resource (scope :db-scope)}
     "remove-property"         {:action W :resource (scope :db-scope)}
     "save-property"           {:action W :resource (scope :db-scope)}
     ;; --- branching (KB by :db-scope-str) ---
     "branch-kb!"              {:action W :resource (scope :db-scope-str)}
     "discard-kb-branch!"      {:action W :resource (scope :db-scope-str)}
     ;; landing a branch onto trunk — the one branching op that is not a write
     ;; to a fork but a decision about what trunk becomes
     "merge-kb!"               {:action M :resource (scope :db-scope-str)}
     "kb-commit-graph!"        {:action R :resource (scope :db-scope-str)}
     "kb-diff!"                {:action R :resource (scope :db-scope-str)}
     "list-kb-branches!"       {:action R :resource (scope :db-scope-str)}
     ;; --- KB by id ---
     "create-kb!"              {:action W :resource (self :party-id-str)}
     "delete-kb!"              {:action W :resource (kb :kb-id-str)}
     "load-kb-details!"        {:action R :resource (kb :kb-id-str)}
     "share-kb!"               {:action W :resource (kb :kb-id-str)}
     "unshare-kb!"             {:action W :resource (kb :kb-id-str)}
     ;; --- room ops (by :room-id-str) ---
     "add-agent-to-room!"      {:action W :resource (room :room-id-str)}
     "remove-agent-from-room!" {:action W :resource (room :room-id-str)}
     "add-room-party!"         {:action W :resource (room :room-id-str)}
     "remove-room-party!"      {:action W :resource (room :room-id-str)}
     "attach-drive-to-room!"   {:action W :resource (room :room-id-str)}
     "detach-drive-from-room!" {:action W :resource (room :room-id-str)}
     "attach-kb-to-room!"      {:action W :resource (room :room-id-str)}
     "detach-kb-from-room!"    {:action W :resource (room :room-id-str)}
     "delete-drive-node!"      {:action W :resource (room :room-id-str)}
     "mkdir-drive-path!"       {:action W :resource (room :room-id-str)}
     "put-drive-file!"         {:action W :resource (room :room-id-str)}
     "dispatch-message!"       {:action W :resource (room :room-id-str)}
     "ensure-room!"            {:action W :resource (room :room-id-str)}
     "update-room-budget!"     {:action W :resource (room :room-id-str)}
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
     "eval-in-room!"           {:action W :resource (room :room-uuid-str)}
     "get-room-state!"         {:action R :resource (room :room-uuid-str)}
     ;; --- proposals (room-scoped via :proposal/room) ---
     "accept-proposal!"        {:action W :resource (proposal :proposal-id-str)}
     "dismiss-proposal!"       {:action W :resource (proposal :proposal-id-str)}
     ;; per-fork decisions carry the same room gate; ops.proposals additionally
     ;; requires :write on the fork's own scope before it lands one
     "accept-fork!"            {:action W :resource (proposal :proposal-id-str)}
     "dismiss-fork!"           {:action W :resource (proposal :proposal-id-str)}
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
     "save-schedule!"          {:action W :resource (always :authenticated)}
     ;; --- admin (role check; ignores the client-supplied party id) ---
     "load-admin-data!"        {:action R :resource (always :admin)}
     "load-schedules!"         {:action R :resource (always :authenticated)}
     ;; --- context stats / misc reads that any authed party may run ---
     "get-room-context-stats!" {:action R :resource (always :authenticated)}
     ;; --- unread badges + per-room notify level (handler derives party from the principal) ---
     "load-unread-counts!"     {:action R :resource (always :authenticated)}
     "mark-read!"              {:action W :resource (always :authenticated)}
     "load-notify-prefs!"      {:action R :resource (always :authenticated)}
     "set-notify-pref!"        {:action W :resource (always :authenticated)}}))

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
