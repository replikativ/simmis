# Authority

Who may do what, and where it is enforced. The short version: there is one
predicate, three planes call it, and all three are deny-by-default.

## "Permissions" is four problems, and only one is an ACL

1. **Reversible state** — wiki pages, KBs, the book, code. → **fork and
   review.** Be generous about what may be written; the review gate is the
   control. See [proposals-and-time-travel.md](proposals-and-time-travel.md).
2. **The narrative record** — the chat log, decisions, provenance. →
   **append-only.** A shape constraint, not an ACL. A party may amend its own
   message; editing another party's is forgery and should be structurally
   impossible rather than merely forbidden.
3. **Authority** — who may land a fork, who may grant. → a **default-deny
   predicate** over the relations that already exist.
4. **Irreversible egress** — email, telegram, HTTP, money, tokens. →
   **capability and budget, authorized in advance.** Review cannot cover it,
   because by the time you are reviewing, the mail has gone.

The axis is not read versus write. It is **where the write lands** — trunk or a
fork.

## One predicate

```clojure
(access/can? principal action resource)
```

`principal` is always derived from a validated JWT, never from an id the client
passed. `resource` is one of a small set of shapes — a store scope, `{:kb id}`,
`{:drive id}`, `{:branch {:scope s :branch b}}`, `{:room id}`, `{:proposal id}`,
`{:self party}`, `{:settings party}`, `:admin`, or `:authenticated`.

`can?` answers as plain Datalog over relations that already exist — party
–`:room/parties`→ room –`:grant/*`→ system. Authorization is a query, not a
second bookkeeping system that can drift from the first.

Humans and agents are the same kind of principal: both are party rows in the
shared system DB, so nothing in the authority model needs to know which it is
talking about.

## Three planes

### Control plane — RPCs

`access/rpc-policy` maps a normalized function name to `{:action :resource}`,
and `authorize-remote` is consulted before every inbound call. It fails closed
on a *broken* check as well as a false one: a resource resolver that throws is
logged and denied, rather than escaping as an error the caller cannot
distinguish from a bug.

Handlers do take client-supplied ids. That is safe because the **policy** binds
them:

```clojure
"load-rooms!" {:action :read :resource (self :party-id-str)}
```

and `can?` requires the resolved `:self` to equal the authenticated party, so
passing someone else's id is refused. When reviewing a new RPC, the question is
not "does the handler check?" but "does its policy row name the right
argument?"

A row binds ONE argument. When a handler acts on a second id the client supplies
— the party to add, the agent to remove, the KB to attach — it checks that id
with the shared helpers in `access.clj` (`require!`, `require-enlistable!`,
`require-agent-manageable!`) before changing anything. They refuse with
`{:type :not-authorized}`, the same shape the gate uses. So the review question
has a second half: does the handler act on any id its row does not name?

A room dvergr makes for an experiment (`room-run`, `runner/run!` in a home,
`catalog_benchmark`) has no members, so membership admits nobody to it. An admin
sees such rooms in the room list and joins one (`join-experiment-room!`,
policed `:admin`; `is.simm.ops.experiments/join!` refuses a room that keeps no
experiment): from then on it is an ordinary room, and its leaderboard
(`load-leaderboard!`, `{:room id}`) is read by its members. The dvergr ops
behind it run for the reading party as a dvergr principal
(`experiments/party-principal`), not for the process; simmis configures no
dvergr authorizer, so dvergr's default admits it and the policy row above is
the check that decides.

An RPC with no row is **refused**, so forgetting one produces a dead feature
rather than an open one. `rpc-coverage-test` joins the live registry against
the table in both directions: a registered fn without a row fails it, and so
does a row without a registered fn.

datahike.kabel's raw store handlers (`dispatch`, `create-database`,
`delete-database`) are not in that registry. They register in
`kabel.remote/functions`, which the server does not serve, and they have no
row, so the gate would refuse them if they became reachable. `dispatch` checks
nothing itself: served behind any row admitting room members, it would let a
member transact raw datoms, forged messages included, into the room's store.

### Data plane — replication

Subscription to a store scope goes through `can?`. Publication *from* a client
is refused unconditionally: sync is one-directional here, so an inbound publish
is either a bug or an attack, whoever sends it.

### HTTP plane — routes

Every route declares `:auth` in its route data:

| Value | Meaning |
|---|---|
| `:public` | no check — the SPA shell, its assets, the login endpoints |
| `:authenticated` | a valid token, nothing more; correct when the handler scopes to the party itself |
| `:blob-read` | `:authenticated`, or the blob-read cookie on GET/HEAD — blob reads only (see below) |
| `{:action … :resource fn}` | a token, then `can?` — the same vocabulary as `rpc-policy` |

`http-auth/validate-auth-declared!` runs as reitit's `:validate` hook, which
fires when the router is **constructed**. A route with no declaration throws
there and the server does not start, naming the route. There is no state in
which an undeclared route serves traffic.

If the server refuses to boot with `:http/undeclared-routes`, that is this.
Decide what the route needs; do not reach for `:public` to make the message go
away.

## Grants

A grant is a `:grant/*` row in the system DB — subject, relation, resource —
attaching a system (a KB, a drive, a repo) to a room with a permission. Because
membership and grants are both ordinary datoms, "may this party read this
store?" is a traversal rather than a lookup in a separate table.

Two deliberate choices:

- **Do not reinterpret existing permission values.** Changing what an existing
  `:grant/permission` value means is a wrong-allow risk on every row already
  written. Add a new value instead.
- **Self-approval dissolves by granting less, not by adding negation.** Agents
  default to propose-only; granting merge on one resource is a deliberate,
  revocable unlock.

## Who may change authority

`access.clj`'s principle: **membership grants read and write of the
conversation; authority-granting and irreversible actions stop at the owner.**
Concretely, with the action each RPC is policed as:

| Act | Who | Where it is checked |
|---|---|---|
| add or remove a room member (`add-room-party!`, `remove-room-party!`) | the room's owner (`:grant`) | policy row |
| …and WHICH party may be added (also `create-room!`'s initial members) | any human; an agent only by its owner, or `:admin` | `access/require-enlistable!` |
| set the room's budget (`update-room-budget!`) | the room's owner (`:grant`) | policy row |
| add your own agent (`add-agent-to-room!`) | a member; the agent is owned by the caller, never by an id the client names | handler (principal) |
| remove an agent (`remove-agent-from-room!`), or change its role and response policy (`update-agent-assignment!`) | an agent that is in this room, by the room's owner, the agent's owner or `:admin`; on removal the party is deleted only when its owner (or `:admin`) removed it from its last room | `access/require-agent-manageable!` |
| attach a KB / drive to a room | `:write` on the room AND `:grant` on the KB / drive | policy row + `access/require!` |
| detach a KB / drive from a room | the room's owner, or `:grant` on the KB / drive (its owner withdraws it, member of the room or not; `:admin`) | policy row (`{:attachment …}`) |
| share, unshare or delete a KB | `:grant` on the KB | policy row |
| write to or discard an open proposal fork | `:write` on the store AND (the fork's author, or `:merge` on its authority scope) | policy row (`{:branch …}`) |
| dismiss one fork of a proposal (`dismiss-fork!`) | `:write` on the proposal's room AND control of that fork, as above | policy row (`{:proposal-fork …}`) |
| dismiss a whole proposal (`dismiss-proposal!`) | `:write` on the room AND (its filer, or control of every open fork) | policy row (`{:proposal-forks …}`) |

Membership is owner-level because it hands out authority: a new member reaches
every KB, drive and repo the room is granted. A member who is not the owner can
still bring in and take out their OWN agents, and attach KBs and drives they own.

**`:grant` on a KB or drive** is its owner, or the global `:admin` role
(`access/authority-override?`). The override covers `:grant`/`:admin` only — it
lets an admin manage who reaches a KB, not read it without leaving a record.

**A direct share (`:kb/shared-with`) means `:read-write`**
(`access/shared-with-permission`): read and write, not merge, not grant. Shares
are bare party uuids on the KB with no permission of their own; `can?` used to
read them as owner, which made `share-kb!` an escalation (a room grantee shared
the KB with themselves and could then merge, re-share and delete). Existing
shares keep the read and write they were documented to give; what they lose is
landing to trunk and managing the KB, which stay with its owner.

**A room's drive answers to the room's owner.** `drives/ensure-room-drive!`
provisions it on first need, owned by the room's owner; it used to be owned by
whoever first opened the Files tab, which made that member owner of the room's
files. `drives/migrate-room-provisioned-drive-owners!` runs at every server
boot, before the server accepts connections, and rewrites `:drive/owner` and
the registry's `:system/owner` of exactly those drives (named "<room> Drive" or
"Room Drive", first granted to that room, owned by someone other than the
room's owner) to the room's owner, logging `::drive-owner-migrated` for each.
It is idempotent: once the rows agree it transacts nothing. Authorization reads
`:drive/owner` alone, so the member who triggered the provisioning holds only
what the room grants.

**An open proposal fork** — a branch named by an open fork of an `:open`
proposal (doc/proposals-and-time-travel.md §5) — belongs to its author
(`:proposal.fork/author`, else the filer) and to whoever may decide it (`:merge`
on the fork's authority scope, the check `ops.proposals` makes before landing
it). Other writers of the store may read it but not write to it or discard it.
Trunk and branches that are no open fork answer exactly as the store does. Once
a fork is accepted or dismissed it is an ordinary branch name again. (The fork's
status is filtered in Clojure rather than with `missing?` in the query —
[replikativ/datahike#1116](https://github.com/replikativ/datahike/issues/1116).)

`load-room-details!` offers in its attach pickers only the KBs and drives the
VIEWER may attach, and leaves out who else a room KB is shared with. It returns
`:can-grant?`, so the settings page shows membership and budget controls only to
the owner; a member sees the budget and the members but not the controls.

## The sandbox and eval

Code that runs in a room's sandbox holds whatever the sandbox was handed, so
what it is handed is the authorization.

**KB handles.** `kb/*`, `kb/conn`, `kb/db`, `knowledge_add` and
`wiki/summarize!` reach a KB only through `access/sandbox-kb-allows?`: the
acting party (for an agent, the agent) may `:read`/`:write` the room, the room
holds a grant on the KB permitting the action, and `can?` lets the party do it
to the KB. `room-agents/get-room-kb-conns` lists only KBs that pass for
`:read`, and every use checks again (`access/require-sandbox-kb!`), so a `:read`
grant never yields a write handle and detaching a KB or removing the agent
revokes its handles in a running sandbox. A room with no KB grant reaches no
KB. It used to fall back to the room creator's first KB, so a member's code in
such a room wrote the owner's private KB on trunk. In the rooms they own,
owners' agents keep the raw `kb/conn` on the KBs the room may write (plan
§2.9.3); wrapping it (`:tx/author` on every transaction) comes later.

dvergr's own sandbox surface (`dvergr.room/kb`, `dvergr.room/db`, the guarded
`datahike.api/connect`) resolves databases from the room's grants in the
dvergr registry and never reaches a KB the room is not granted. It does not
consult `can?`: `dvergr.room/db` and `connect` return a conn by name for any
database attached to the room, whatever the grant's permission.

**Evaluating code in a room** (`eval-in-room!`, the sandbox panel's quick eval
and the wiki's ▶ Run) is the room's full capability, so its policy row is
`:merge` on the room, the owner. The author recorded with the evaluation is the
connection's principal, not an id from the client. `can-eval-in-room!` and
`get-room-state!`'s `:can-eval?` answer the same question for the UI, which
shows the eval controls only to the owner.

## Identity

Accounts come from `:alpha-users` in `config.local.edn`. There is no
self-registration unless `SIMMIS_ALLOW_SELF_REGISTRATION=true`, which warns on
every boot — an account is what every `:authenticated` route and RPC checks
for, so creating one is not a neutral act.

`POST /auth/dev`, which mints a token for any address, is opt-in via
`SIMMIS_DEV_AUTH=true` and also warns on every boot. Never expose a server to a
network while it is set.

## Blob reads

`GET /blobs/<sha256>` accepts the Authorization header or an HttpOnly,
`SameSite=Lax` cookie scoped to `/blobs/`. The cookie exists because the UI
reaches blobs through `<img src>` and `<a href>` — requests the *browser*
issues, which cannot carry an Authorization header. It authenticates reads
only (GET/HEAD); a write must present the header.

The cookie does **not** hold the access token. Cookies are not scoped by port
and every port of a host is the same site, so any server on another port of
the same host — an agent's dev server, say — receives it. It holds a blob-read
token instead: signed with a key derived from the JWT secret
(HMAC-SHA256(secret, `"simmis/blob-read/v1"`)), issuer `simmis/blob-read`,
audience `blobs`, one hour. Only routes declaring `:auth :blob-read` accept
it (`http-auth/blob-reader-party-id`); the access-token validator — every
other route, every RPC, the websocket handshake — refuses it, by key and by
issuer, and the blob route in turn refuses an access token in the cookie.
`/auth/login` and `/auth/refresh` set it, and the client renews it through
`POST /auth/blob-cookie` (header-authenticated) before it expires, and again on
waking up (visibility, focus, back online) since timers stall while a machine
sleeps; `/auth/logout` expires it. The blob-read cookie itself still reaches
other ports of the same host, so what such a server can obtain is a one-hour
read capability for any blob by hash — not more, but not nothing, because blob
reads are not room-scoped (below).

It authenticates but does not **authorize**: any signed-in user can fetch any
blob by hash. Blobs are content-addressed and deliberately shared, so the same
bytes in two rooms are one blob, and scoping would need a record of which rooms
referenced it. This is listed in the README's known gaps.

## The authority matrix

`test/is/simm/authority/` exercises `can?`, the control plane and the data
plane together, against a real system DB and real grants, with principals
{owner, member, non-member, admin, anonymous}:

- every `rpc-policy` row is decided for every principal, and a row without a
  reviewed matrix entry fails the suite;
- RPCs are delivered through distributed-scope's own inbound dispatch, so the
  gate, the principal binding and the handler run as they do for a websocket
  message;
- each attack is paired with the legitimate use of the same path.

Attacks that still succeed today are written as `known-hole`s carrying an id:
they pass while the hole is open and fail once it closes, so the fix has to
flip its test into a plain refusal. They are the open authority defects, and
the suite is where they are listed — not here.
