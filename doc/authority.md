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
`{:kb-store scope}` (a KB's store and nothing else), `{:drive id}`,
`{:branch {:scope s :branch b}}`, `{:room id}`, `{:proposal id}`, `{:self party}`,
`{:settings party}`, `:admin`, or `:authenticated`.

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

**What a refused or failed call tells the client.** distributed-scope replies
to a failed invocation with the `pr-str` of the exception, which on the JVM is
every stack frame, the class and file names and all of `ex-data`.
`is.simm.runtimes.rpc-errors` decides what that string is: the gate
(`rpc-errors/gate` around `authorize-remote`) throws, and every registered
handler is wrapped (`rpc-errors/install!`) to replace its failure with, an
error that prints as `{:error "<message>" :type <kw> :correlation-id "<uuid>"}`
and nothing else. `:error` is the first line of an `ex-info`'s message, or
"internal error" for any other throwable; `:type` only when it is in
`rpc-errors/client-types` (`:not-authorized`, `:authentication-required`, …).
The full exception is logged (`::remote-call-failed`; a gate refusal is
`::remote-call-refused`, next to `access`'s `::rpc-denied` with the policy
row) under the same correlation id, which the client shows as `(ref xxxxxxxx)`
(`remote/error-text`), so a report can be matched to the log.

A room dvergr makes for an experiment (`room-run`, `runner/run!` in a home,
`catalog_benchmark`) has no members, so membership admits nobody to it. An admin
sees such rooms in the room list and joins one (`join-experiment-room!`,
policed `:admin`; `is.simm.ops.experiments/join!` refuses a room that keeps no
experiment): from then on it is an ordinary room, and its leaderboard
(`load-leaderboard!`, `{:room id}`) is read by its members. The dvergr ops
behind it run for the reading party as a dvergr principal
(`is.simm.ops.dvergr/party-principal`), not for the process, and dvergr asks
simmis's authorizer about each of them (below, "dvergr ops").

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
So the browser never writes through its replica's writer: wiki edits are
`block_remote` RPCs, gated by the `{:branch …}` rows above, with the optimistic
display a prediction the client drops or rejects on the RPC's answer (see
architecture.md, "Writes go through the server"). `browser-write-guard-test`
fails on any browser code that calls a replica's writer, `d/transact!` or a
konserve write.

A wiki widget's writes are the one raw-transaction path the browser has, and
they are policed as such. `widget-transact` takes a widget's data transaction
under `:write` on `{:kb-store scope}` — a resource `can?` answers only for a
KB's store, so no principal can address a room's message store, repo or agent
DB through it. The handler (`block_remote.clj`) refuses schema in the data:
tx functions and any operation but add/retract/retractEntity are refused by
shape, and the transaction is then computed with `d/with` and refused if any
datom names a `:db/*`, `:db.*/*` or `:tx/*` attribute, touches a schema entity,
or writes the transaction entity beyond the `:db/txInstant` and `:tx/author`
the handler stamps. `widget-install-attr` declares an attribute under `:merge`
(the KB's owner — a `:read-write` grantee may not, and `:admin`'s authority
override covers `:grant` only): a `:db/ident` is append-only and global to the
KB. The widget names the codomain object; the handler derives the value type
with `schema/codomain->db-type` and records the morphism, and refuses a
`:db.type/*` keyword, an unknown object and the store's own namespaces
(`data-model.md`, "Schema types"). `widget-writes-test` runs each attack with
its legitimate twin. Where the browser names the
uuid of a block it is creating (`create-sibling-after`'s `:new-uuid`,
`ensure-page`'s `:first-block-uuid`), the handler refuses one that already
names an entity: `:entity/uuid` is a unique identity, so it would upsert onto
that entity rather than create a block.

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

## Private MCP

The optional MCP listener (`runtimes/mcp.clj`, `SIMMIS_MCP_PORT` or
`:mcp {:port ...}`) binds only `127.0.0.1`; it is not mounted on the public
HTTP router. Access it through an SSH local forward. It runs against the
existing server execution context through `is.simm.ops.dvergr/host`, not a
second daemon or a second set of store connections.

The surface permits these selected reads:

- conversations: `room_list`, `room_detail`, `room_messages`, `run_list`, `run_detail`;
- workspace and world inspection: `room_files`, `room_read`, `room_diff`, `room_review`;
- resources and executions: `room_wallet`, `job_list`, `job_status`, `attempt_list`, `attempt_detail`, `review_list`;
- experiments: `scorecard_list`, `scorecard_detail`, `experiment_progress`, `experiment_report`, `catalog_list`.
- captured artifacts: `simmis_attempt-files`, `simmis_attempt-file`.

`room_read` reads the CURRENT workspace. `attempt_detail` returns the certified
Attempt's immutable evidence (including captured output files), not files from
today's workspace or a discarded execution world. These reuse Dvergr's existing
records; there is no second result store. Oversized upstream MCP results may
be truncated; workspace file reads support byte offsets and lengths. Captured
artifacts do not require reading the full evidence response:
`simmis_attempt-files {room, id, offset?}` lists up to 100 paths with sizes and
content identities, and `simmis_attempt-file {room, id, path, offset?, length?}`
returns at most 16384 bytes as base64. Follow `next-offset`, concatenate the
decoded bytes, then decode UTF-8; individual chunks can split a character.
These are the text-file outputs the upstream evaluator captures, not a binary
artifact ingestion API. Both reads include the Attempt's content identity;
neither can fall back to the current workspace when a captured path is absent.
Their schemas reject extra fields and validate direct in-process callers too.

Each authenticated principal carries a signed exact tool ceiling, independent
of the profile a client requests. Tokens with no capability claims retain only
the original five conversation/run tools, including through MCP resources.
Version-one claims must name a nonempty subset of the server's selected reads;
malformed/unknown versions are refused. Adding a tool on the server does not
widen an already-issued credential. Every operation
and resource read also passes the existing `access/dvergr-authorize` gate;
room listings are filtered and a valid credential grants no access to another
party's room.
No raw database writer or unrestricted code-evaluation tool is exposed.
The host validates MCP argument shapes at runtime against upstream schemas
(Dvergr's invoke does not itself validate them), and caps list limits at 1000.
Visibility filters and already-open resource sessions recheck room authority.
Checker execution/calibration and workflow export (which calibrates on export),
paid starts and host-path trajectory exports are not on this read surface.
The Simmis artifact and work ops register through Dvergr's authorized host-op seam,
not dynamic unguarded tools, and are removed when the listener stops.

An operator can call `is.simm.runtimes.mcp/mint-token!` for an existing human
party UUID. Its result contains a credential and must be saved privately, not
printed or logged. Tokens expire after one hour, authenticate as that party,
and create no membership or grant. The party is checked on each request;
deleting it revokes access, and grant changes affect subsequent operations.
There is no individual-token revocation registry in this first surface.

MCP tokens use issuer/audience `simmis-mcp` and an HMAC-derived signing key
distinct from the web and blob keys. Web tokens, blob capabilities, the
Dvergr local-admin token and Dvergr agent-run tokens are not accepted here;
MCP tokens do not authenticate to the web. This human-account surface is not yet
an OAuth flow or an agent-delegation API.

`POST /auth/mcp-token` exchanges a validated web Authorization bearer for the
same one-hour read capability by default. A closed JSON body may choose only
`{"mode":"read"}` or explicitly `{"mode":"work"}`; it cannot choose an identity
or arbitrary scopes. Empty bodies retain read access. Version-two work credentials
carry signed `mode=work` and an exact ceiling of the selected reads plus
`simmis_post`, `simmis_task`, `simmis_work-allowance`, `simmis_work-status` and
`job_cancel`, `simmis_workflow-write`, `simmis_workflow-check` and
`simmis_benchmark`. Old credentials
and saved client sessions do not gain writes or newly added tool names.
only the authenticated existing human may obtain their own credential. It
refuses agent/deleted accounts, cookie-only requests and disabled MCP, and its
response is `Cache-Control: no-store`. The exchange is on the web server;
the MCP protocol listener remains private. This is an explicit credential
exchange, not acceptance of web tokens by MCP.

### Conversation and bounded work

Workflow authoring uses ordinary versioned Geschichte files under
`workflows/<name>/`. `simmis_workflow-write` requires current room `:write`
permission and accepts one atomic edit batch, limited to 100 files, 512 KiB per
file and 2 MiB per batch. It cannot edit the room-root `AGENTS.md` or paths
outside that bundle. A workflow may include its own fixture instructions.
`simmis_workflow-check` requires room `:read` permission but is deliberately
available only through an explicitly selected work credential, not the existing
read-only tool ceiling. Both return structural inspection without evaluating
checker source or calibration. Inspection reads one immutable worktree manifest
and pinned content hashes, capped at 100 regular UTF-8 files and 2 MiB.
A successful write remains acknowledged even if the resulting bundle is
incomplete or inspection fails. The returned bundle content ID identifies the
inspected revision; it is not a correctness certificate or execution admission.

`simmis_benchmark` requires BOTH current room read/merge permission and
installation-operator status (`:party/role :admin`). Neither room ownership
alone nor installation-admin status without room access admits authored
checker execution. `expected-bundle` must match the inspected content UUID;
the winning durable request freezes that revision and its complete experiment
definition before execution. An identical retry observes its original Job,
including after authoring files change; changed intent conflicts.

This endpoint admits at most eight offline developer-profile cells, sequentially,
with native-accounted providers, explicit aggregate allowance, finite integer
per-cell shares of both microUSD and model dispatches, `/out` capture, and
discard settlement. It refuses judges, live/frozen web configuration, arbitrary
capture roots, custom profiles and automatic promotion. Attempts and Scorecards
remain the existing room results; an operator admitting execution does not
make an authored checker trusted—the evaluator tier stays `:ad-hoc`.
Money and dispatch allowance are aggregate; `timeout-ms` is per attempt, not a
whole-experiment deadline.

The installation admits one checker benchmark at a time. The physical capacity
permit is acquired only by the durable winner, after its Job pointer exists,
and returned only after grouped physical cleanup and wallet return succeed.
A checker runs synchronously on Dvergr's owned scoring worker, never a detached
future. Its SCI context and cumulative-allocation accounting are created there;
the 20-second deadline/interruption state is sticky through catches, verdicts
must contain at most 100 boolean checks and finite reward, and capture is
bounded to 100 files / two million characters. These are **cooperative** checks:
a large or blocking allowed host operation can exceed them before the next SCI
callback. Such a worker keeps cleanup unresolved and capacity held rather than
being reported as physically stopped. This is explicitly a trusted-operator
path, not safe arbitrary-user checker hosting or a hard memory/process fence.
Checker policy and effective grading parameters are part of verifier identity.

Generic bounded tasks and benchmark candidates select the versioned
`:simmis/file-worker` sandbox extension. This is deliberately a file/code
surface: after standard working-context installation and on every rebind, it
denies injected `dvergr.*`/legacy room and shared-registry APIs, host environment
access and native `intake.mail` reads (including already-open accounts).
Spindel 0.1.126's opaque `CurrentWorld` boundary and reactive programming
primitives remain available; candidate code receives no raw execution-context
record. Real-context tests cover that boundary and independent interpreters.
Otherwise a clean workspace would still leave shared task results and actor
configuration reachable through unfiltered host helpers. The exact extension
set and policy are pinned in candidate/submission/experiment identities.
Ordinary room participants retain their richer programming environment; this
endpoint restriction is not a claim that all upstream shared reads are scoped.

`simmis_post` uses the same `ops/chat.clj` dispatch as the browser, authenticated
as the human account, with the room's existing `:write` permission, recipient
selection and threading. It can wake assigned agents on their ongoing ROOM
budgets. It is not a bounded isolated task. The durable message's typed kind
`:mcp-user-message` records its trusted transport source; clientInfo is not a
verified Codex/Claude agent identity, and tool arguments cannot override the author.

`simmis_task` requires current room read AND `:merge` authority, the same
privileged boundary as generic code in the UI. It starts one generic code/file
candidate, not an existing Simmis party, with an explicit model, microdollar cap,
native dispatch count, model-step limit, timeout, and `review` or `discard`
settlement. These limits are finite and validated at runtime. No automatic merge
is available. Its speculative effect policy forbids room/lifecycle mutations,
native processes, undeclared external tools and sandbox HTTP; file writes stay
inside the fork and under a quota. The native model gateway has separate conserved
dispatch/spend admission. Subscription providers are not accepted as having these
native cost guarantees.

Owners explicitly provision a finite execution allowance through
`simmis_work-allowance`. It is NOT payment, a billing balance or an agent-accessible
mint capability. Both microUSD and model-dispatch units are conserved: room → job
→ candidate Run. Unused resources return only after the job's evaluation cleanup
group confirms physical completion, not merely cancellation of its Spin. Unresolved
provider reservation wallets are never refunded as unused Run balances.

`model/work_requests.clj` stores a typed host-system admission journal, keyed by
authenticated party, room and caller request UUID. Its immutable content-addressed
intent pins the candidate/environment and limits. Only the atomic insert winner
may start a job; same-intent retries observe it, changed intent conflicts. The job
pointer commits before evaluation starts. An interrupted pointerless admission is
reported, never silently re-executed. This is at-most-once admission, not a promise
of exactly-once completion. Boot recovery runs after orphan-Run reconciliation;
terminal descendant wallets return bottom-up, while unresolved/live work remains
pending. `simmis_work-status` projects safe admission metadata (not its ownership
token); execution state remains in Dvergr Jobs/Runs.

The completion evaluator captures bounded text deliverables under `/out` before
world discard into the certified Attempt, readable through the existing artifact
tools. Its check means the Run completed, NOT that the task was correct. The capture
bound is 100 files and two million characters; the upstream tree reader counts
characters rather than UTF-8 bytes. Retained worlds remain reviewable through
the existing run/proposal flow.

### Connecting a second operator

Each person logs in as themselves; no account, room membership or grant is
created by MCP login. From the person's own machine, forward **only** the MCP
port with their existing SSH access (substitute their SSH account if different):

```sh
ssh -N -T -o ExitOnForwardFailure=yes \
  -L 127.0.0.1:47990:127.0.0.1:47889 simmis@simm.is
python3 /absolute/path/to/simmis/scripts/mcp_client.py login --origin https://dev.simm.is
```

The helper prompts for the person's email and password and saves a web session
in `~/.config/simmis/mcp-session.json` (mode 600), not the password. Treat this
file as a full web-login credential: it is broader than the MCP read token.
`headers` exchanges it for the chosen MCP bearer, renewing the web access
token through `/auth/refresh` when needed. Once the refresh session expires or
is revoked, run `login` again. Never paste the output of `headers` into logs.
Authentication requests require HTTPS except on loopback, refuse redirects and
ignore environment proxies; the session's origin is retained with the login.
To opt into work, rerun `login --access work`. The saved choice is used by
credential renewal; an existing session without that choice stays read-only.
`headers` cannot change it. Work login requires the server to confirm work
mode, and it still creates no room permission or execution allowance.

For Codex, add the following to the person's user `config.toml`
([HTTP MCP configuration](https://learn.chatgpt.com/docs/extend/mcp)):

```toml
[mcp_servers.simmis_dev]
url = "http://127.0.0.1:47990/mcp"
http_headers_helper = "python3 /absolute/path/to/simmis/scripts/mcp_client.py headers"
startup_timeout_sec = 30
tool_timeout_sec = 60
enabled_tools = ["room_list", "room_detail", "room_messages", "run_list", "run_detail"]
```

The optional `enabled_tools` line above intentionally narrows this client to
the original five tools; omit it to discover the credential's full selected
surface (reads by default, bounded work only after explicit work login).
New tools also require a freshly exchanged credential and client reconnect.

Claude Code's HTTP MCP entry uses the same URL and command under
`headersHelper`. No subscription credential, signing key, or operator nREPL
access is needed. These clients still see only the rooms their human account
may read; SSH access alone does not grant Simmis membership.

The listener starts with the server when configured and stops before its host
is cleared. Its handle is process-level transport state, not room or UI state
that a world fork should copy.

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
| accept or reject an escalated review's proposal | `:merge` on the room (its owner): the policy row above, then `review/decide!`, then dvergr's authorizer for `room_merge` / `room_discard` | policy row + `workflows.review/decide!` + `access/dvergr-authorize` |
| a wiki widget's data transaction (`widget-transact`) | `:write` on the KB's store (`{:kb-store …}` — never a room store); no schema in it | policy row + `block_remote` (shape, then `d/with`) |
| declare an attribute from a widget (`widget-install-attr`) | `:merge` on the KB's store (its owner); the value type derived from the codomain object | policy row + `block_remote` |
| declare an attribute from an agent's sandbox (`kb/install-attr!`) | `:merge` through the room: a member of the room, the room holding a `:merge`/`:owner` grant on the KB, and `can?` `:merge` on it; the value type derived from the codomain object | `access/require-sandbox-kb!` + `attributes/declaration` |
| read a room's labels (`list-labels!`) | a member (`:read` on the room); no RPC writes a label | policy row |

| stop a Run (`cancel-room-run!`) | its starter, the room's owner, or `:admin` — each from inside the room | policy row (`:write`) + `access/require-run-cancellable!` |

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

## Runs

Seeing a Run is reading its room (`run-world-live!`, and the Runs the room's
replica holds). Stopping one is narrower (plan decision §2.9.2): its
**starter**, the room's **owner**, or `:admin` — each only from inside the room,
since the `cancel-room-run!` gate is `:write` on the room
(`access/may-cancel-run?`).

A dvergr Run records the agent that runs (`:run/actor`) and its trigger
message (`:run/trigger`), not who caused it, so the starter is derived
(`is.simm.agents.runs/run-starter`):

| Run | Starter |
|---|---|
| triggered by a message | the message's logical initiator (`:metadata :from`, delegated work), else its author — a human's message names the human, an agent's names that agent party |
| scheduled | none: the trigger is the scheduler's message, and dvergr's schedule row records no owner |
| a job (`:run/kind` in `dvergr.jobs/kinds`) | the job's actor, which dvergr records as its starter (none for `:dvergr`, the process) |
| anything else | none |

A Run with no starter is the owner's (or an admin's) to stop. The handler
finds the Run among the room's LIVE Runs before it decides, and cancels through
dvergr's `cancel-room-run!`, which binds the Run to the room again: a Run id
names nothing in another room.

The client reads Runs from the room replica, which says nothing about the
viewer, so whether the viewer may stop a Run is asked of the server per live
Run (`can-cancel-room-run!`, the same rule). The run strip and the Run inspector
show the stop control only when it answers true.

## dvergr ops

dvergr asks its `:authorize` seam (`dvergr.principal`) before every op it runs
for a principal other than the process itself. simmis invokes dvergr ops in one
way, `is.simm.ops.dvergr/invoke`, which passes the principal of the acting
party (`party-principal`, `:principal/id "party/<uuid>"`) and installs
`access/dvergr-authorize` on that call's daemon map. The authorizer maps
dvergr's resource descriptor, as dvergr resolved it, onto a `can?` question:

| dvergr resource | simmis question |
|---|---|
| `[:room x]` | the same action on `{:room id}` (dvergr's room id is the slug, `/` as `_fork_`, or a uuid); a fork (`<slug>_fork_fork-<hex>`, a Run's or a review's world) answers as its room; on a room `:merge` and `:admin` are the owner's |
| `[:job x]` | `:read` on the job's room; anything else (cancel) is `access/may-cancel-run?` with the job's owner as starter |
| `[:global]` | a read: `:authenticated`; a write: `:admin` |
| `[:agent x]` | `:admin` |
| anything else, or a handle that resolved to nothing | denied |

A principal that names no party is denied, and so is a throw anywhere (dvergr
denies on a throw too). Denials raise dvergr's 403 op error and log
`::dvergr-op-denied`. The leaderboard reads (`scorecard_list`,
`scorecard_detail`, `experiment_progress`, `experiment_report`) therefore
refuse a party that may not read the room themselves, not only behind the
`load-leaderboard!` row.

A review (`workflows.review/review-run!`) merges or discards AS the
reviewer's party through this same seam, so an approval lands only when that
party holds `:merge` on the room; otherwise dvergr escalates it with the
refusal as the reason. Delegating merge to a reviewer for a workflow's scopes
does not exist yet.

The seam is installed per call, not process-wide (`principal/configure!`). The
process-wide seam also answers every registry-tool call of every agent
(`dvergr.tools/execute` asks it with the agent's principal), and dvergr classes
`clojure_eval` as `:admin` on the room, which in simmis is the owner's act,
while simmis gives a room's agents its sandbox (below). Agents' tool calls are
governed by their per-agent tool maps and the sandbox checks until they move
behind `can?`.

## The sandbox and eval

Code that runs in a room's sandbox holds whatever the sandbox was handed, so
what it is handed is the authorization.

**Screens.** A member's screen capture is theirs; a room sees a slice of it
only through its own grant windows (`screen-grants/windows-for-room`).
`screen/*` returns a frame to a room's agent only when its time lies inside one
of THAT room's windows for its sharer — open, or ended (so an agent asked after
the meeting reads what was shown during it). A frame from before, between or
after the windows, or inside a window granted to another room, is never
returned; `since`/`until` narrow a window and never widen it
(`test/is/simm/runtimes/screen_history_test.clj`). A window whose client died
ends at its last heartbeat, not when the next share opens.

**Transcripts.** "Transcribe" (`transcribe-drive-file!`) is `:write` on the
room, like `put-drive-file!`: it reads a file already on the room's drive and
posts into the room as the caller. `transcript/*` reads only the agent's own
room's drive.

**KB handles.** `kb/*`, `kb/conn`, `kb/db`, `knowledge_add` and
`wiki/summarize!` reach a KB only through `access/sandbox-kb-allows?`: the
acting party (for an agent, the agent) may `:read`/`:write` the room, the room
holds a grant on the KB permitting the action, and `can?` lets the party do it
to the KB. Declaring an attribute (`kb/install-attr!`) is that check at
`:merge` — still membership of the room, but a `:merge` or `:owner` grant on
the KB, so a `:read-write` grant (what attaching records by default) writes
data and not schema, as for a widget; the agent names the codomain object and the
value type is derived. `room-agents/get-room-kb-conns` lists only KBs that pass for
`:read`, and every use checks again (`access/require-sandbox-kb!`), so a `:read`
grant never yields a write handle and detaching a KB or removing the agent
revokes its handles in a running sandbox. A room with no KB grant reaches no
KB. It used to fall back to the room creator's first KB, so a member's code in
such a room wrote the owner's private KB on trunk. In the rooms they own,
owners' agents keep the raw `kb/conn` on the KBs the room may write (plan
§2.9.3); wrapping it (`:tx/author` on every transaction) comes later.

dvergr's own sandbox surface (`dvergr.room/kb`, `dvergr.room/db`, the guarded
`datahike.api/connect`, `dvergr.room/*room*`) resolves databases from the
room's grants in the dvergr registry and never reaches a KB the room is not
granted. It writes only where the grant permits `:write`: every
`transact`/`transact!` on a sandbox handle asks first, so a `:read` grant
yields a read-only handle and a grant downgraded after the handle was handed
out refuses at once. The room's messages store is read-only from the sandbox
whatever the grant (messages go through `dvergr.room/post!`). dvergr asks the
host what a permission permits, and `web/install-grant-permits!` answers with
`access/permission-satisfies?`, the ladder `can?` uses, before any room
hydrates; without it a `:merge` grant, which dvergr does not know, would be
read-only there. The same answer decides the writable-KB list
`knowledge_add` uses, which systems `dvergr.room/gc!` collects, and whether a
fork may merge into a system (dvergr#326). `authority/sandbox_grants_test`
runs the attacks through the room's sandbox.

**Evaluating code in a room** (`eval-in-room!`, the sandbox panel's quick eval
and the wiki's ▶ Run) is the room's full capability, so its policy row is
`:merge` on the room, the owner. The author recorded with the evaluation is the
connection's principal, not an id from the client. The code runs in the room's
execution context, as an agent's `clojure_eval` does, so the databases attached
to the room resolve and their grants apply. `can-eval-in-room!` and
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

## Token audiences

An access token names the surface it is for, and each surface accepts only its
own `aud`. The web app (websocket handshake and every HTTP route) is
`auth-config/web-audience`, `"simmis-api"`: what `/auth/login`, `/auth/refresh`
and `/auth/dev` have always minted, so no session was logged out when the check
arrived. It is kabel's `:required-claims {:aud …}` on the one `:jwt` config the
websocket plane and `http-auth` both build their validator from. A token for
another audience (MCP's `<origin>/mcp`), with an audience list, or with none,
is refused.

kabel's issuer registry takes one `:required-claims` for every issuer, so an
external IdP (`:oidc-issuers`) must be configured to mint the same audience; a
per-issuer audience would need kabel to accept `:required-claims` per issuer
entry.

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
other route, every RPC, the websocket handshake — refuses it, by key, by issuer
and by audience, and the blob route in turn refuses an access token in the
cookie.
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
