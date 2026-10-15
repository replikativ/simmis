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
`{:room id}`, `{:proposal id}`, `{:self party}`, `{:settings party}`, `:admin`,
or `:authenticated`.

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

A room dvergr makes for an experiment (`room-run`, `runner/run!` in a home,
`catalog_benchmark`) has no members, so membership admits nobody to it. An admin
sees such rooms in the room list and joins one (`join-experiment-room!`,
policed `:admin`; `is.simm.ops.experiments/join!` refuses a room that keeps no
experiment): from then on it is an ordinary room, and its leaderboard
(`load-leaderboard!`, `{:room id}`) is read by its members.

An RPC with no row is **refused**, so forgetting one produces a dead feature
rather than an open one. `rpc-coverage-test` joins the live registry against
the table so the omission is visible.

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

## What is not covered

`can?`, the control plane and the data plane each have unit coverage, but
nothing exercises them together against a real system DB and real grants. Every
plane is currently tested against a stub of the others.
