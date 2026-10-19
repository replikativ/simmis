# Architecture

How simmis is shaped, and where to look for each thing. Read this before the
code; the rest of `doc/` assumes it.

## The one-sentence version

A Clojure server owns a set of Datahike databases and is the only writer; a
ClojureScript client holds a real Datahike replica of the ones it may see and
queries it locally; agents run in a sandbox on the server and write to
branches rather than to trunk.

## Processes and ports

One JVM runs everything in development (`clj -M:dev`):

| | |
|---|---|
| Web UI | http://localhost:8080 |
| WebSocket (kabel) | ws://localhost:47295 |
| nREPL — server, Clojure | 47888 |
| nREPL — browser, ClojureScript | 9631 |

Those are the default instance's ports. `SIMMIS_PORT_OFFSET` adds one number
to all of them (and to shadow's own 9630), so a second instance can run beside
the first; `src/is/simm/runtimes/instance.clj` resolves the ports, the state
home (`DVERGR_HOME`, default `.dvergr`) and the data root (`SIMMIS_DATA_DIR`,
default `data`), and `user/shadow-config` applies the ports to shadow.

`dev/user.clj` is the lifecycle; `src/is/simm/runtimes/web.clj` is the server
boot — system DB, mail sync, room hydration, kabel + auth + sync, telegram,
then the HTTP routes.

Two boot steps are switches. **Storage GC** (`runtimes/storage_gc.clj`, which
drives dvergr's `gc-stores!` at boot and every 6h) is OFF unless
`:storage-gc {:enabled? true}` or `SIMMIS_STORAGE_GC=true`: on datahike without
#1117 a mark that dies on an Error comes back empty and the sweep deletes the
whole store, which is how the dev.simm.is system DB was lost. simmis pins
datahike 0.8.1906, the first release with #1117 (a mark that dies raises
`:gc-incomplete-mark` and nothing is swept); the switch stays off by default
regardless. While enabled on an older datahike (or a source checkout) it warns on every sweep, and it logs a sweep that timed out,
failed or skipped rooms as an error rather than as an empty one. dvergr refuses
a deleting sweep unless it was enabled for the process's home
(`enable-storage-gc!`, dvergr 0.1.291): the loop's `start!` enables it for
simmis's home and `stop!` (also on server stop) revokes it, so with the switch
off nothing is ever enabled and a sweep run anyway is logged as refused. **Mail sync**
for stored accounts is ON unless `:mail {:sync? false}` or
`SIMMIS_MAIL_SYNC=false`, which isolated instances use. The **transcript
watcher** (`runtimes/transcripts.clj`) runs only when `SIMMIS_TRANSCRIPTS_DIR`
names the directory jigasi saves call transcripts into; unset, boot logs one
info line and watches nothing. A system-DB directory
with no `:db` head stops the boot with that diagnosis; it is never created
over or deleted.

An optional private MCP listener (`runtimes/mcp.clj`) starts in the same JVM
when `SIMMIS_MCP_PORT` or `:mcp {:port ...}` is configured. Its bind is always
loopback, its base port receives the instance offset, and it serves the
existing rooms through Dvergr's embedding host hook. It owns no databases.
The default surface is selected room/message/run, workspace, Attempt, job and experiment
reads for an explicitly authenticated human. Explicit work credentials add canonical
chat dispatch and one bounded isolated candidate, not a raw database writer or
unrestricted eval endpoint. The typed system-DB admission journal makes request
retries observe existing Dvergr Jobs/Runs; it is not a second workflow engine.
The opt-in `simmis_coordinator` profile composes scoped child Runs and experiments
within an existing allowance. Ordinary file workers retain their narrower
surface; see [coordinator-work.md](coordinator-work.md). The current alpha
runtime pins classified resource wallets and independent Run sessions. Existing
unclassified financial stores are not migrated automatically; use a separate
fresh alpha home when testing this dependency pin, not a copied or reset home.
Workflow bundles are versioned Geschichte files. An explicitly admitted,
exact-revision `simmis_workflow-run` uses the ordinary bounded candidate and
completion evaluator: declared literal parameters and frozen source/inputs
are materialized in its inherited room fork, not trunk. Its source pin and
input replacements appear in the proposed diff; it never implicitly executes
the bundle's checker or treats completion as correctness. Workflow preparation
separates `workflow-runs/prepare-source` (revision validation,
literal rendering and input selection, without a provider) from native
candidate construction. Portable worker input construction uses the same
`worker-files/freeze-files` descriptor validation and content identity as
explicit room-file selection; it does not read additional room files.
Room-header workflow
tabs carry the room replica scope into their Run inspectors and open results
in the column that owns the action. Restored workflow and Run tabs resolve
missing scopes from the authenticated room roster, like chat/thread tabs.
Pinned recurring deployments use a host-owned scheduling adapter, not another
execution engine. A listener on Dvergr's root clock only coalesces notifications
into a one-element queue; one owned off-engine worker recovers and drains the
system-DB occurrence outbox through the existing workflow/benchmark admissions.
Copied clock listeners reject non-root contexts. Shutdown unsubscribes and joins
the physical worker before replacement. Current authority, retained-source
identity and host/model policy are checked at each admission. Money and dispatch
allowance come from the same finite shared room wallet as manual execution;
scheduling never creates funds. Overlaps are skipped and missed ticks coalesce
to the latest intended slot. The Schedules view keeps these deployments distinct
from legacy agent-authored prompt/code tasks.
Separately, an
exact-revision offline benchmark reuses Dvergr's experiment runner and room
Attempts/Scorecards; it requires both installation-operator and room merge
authority. Its clean fixture room is provisioned outside the authoring room,
so candidate workspaces do not inherit checker/gold files. The Job runs in
the fixture's execution context while durable results stay in the control
room. Synchronous SCI grading stays inside Dvergr's physically tracked scoring
worker; it is cooperative trusted-operator execution, not hard process isolation.
Optional reference/damaged-answer calibration runs on the owned world-setup
worker before each candidate; answers remain outside its workspace and do not
promote checker trust.
Local-worker transport lives separately in `ops/worker-service.clj` and
`ops/worker-api.clj`, backed by typed enrollment/assignment records. Its six
narrow operations use separate worker credentials, immutable selected-file
inputs and bounded output receipts. Application startup installs these bindings
with disjoint worker/lease authentication on the private MCP listener. Separate
human control operations require an explicit worker-control credential and
opt-in new admission. Completion alone never
imports files or certifies a Run. See [data-model.md](data-model.md).
The separate `ops/worker-frames.clj` provides bounded LF-delimited JSON for
private process pipes: string keys preserve filenames; duplicate fields,
trailing values, invalid Unicode and oversized/deep frames are refused.
A malformed frame or I/O failure poisons the channel rather than retrying.
This is framing, not authentication or executor attestation: every guest
message (including ready/result) is candidate-controlled. Shutdown evidence
comes only from the external controller, never a guest report. The framing
module is not a process launcher or installed transport. The separate
`ops/worker-process.clj` stdio entrypoint can run a single credentialless guest
or a serial inference broker. It captures the protocol streams and redirects
Java and Clojure standard I/O before loading Dvergr. The broker's opening
configuration must come from the trusted parent before forwarding guest
traffic. These endpoints are not installed by server startup and supply no
physical containment by themselves. `build/worker` supplies the minimal pinned
runtime; the local SCI controller retains guest and broker in one finite
systemd unit, with physical capture and durable admission before GO.
The host-only `ops/worker-work.clj` adapter connects these to Dvergr's existing
Protocol/WorldSetup/Evaluator capabilities. It owns no alternative execution
engine or result store. Separate conserved admission allowance funds local
work; remote subscription usage remains explicitly unpriced. Cancellation
retains the native supervisor until remote stop, and output import stays in
the canonical isolated Run world with current permission checks.
The separate `local-worker-workflow` control uses the same provider-independent
saved-workflow preparation as native server execution. It freezes the inspected
bundle, literal parameters, fixture/input overlays and regular-file modes before
admission. Only those files reach the guest. Ordinary workflow source may include
checker/gold files, but authored checkers are not executed and this is not a
benchmark secrecy boundary. Exact retries observe the original Job; recovery
reconstructs its input only from the stored snapshot, not newer room content.
`ops/worker-benchmarks.clj` is a host-only local-worker adapter for the same
experiment runner: one enrolled candidate, up to eight case/repetition cells,
serial execution and discard settlement. It uses the existing clean fixture
parent and trusted server-side calibration/checker. Guests receive only pinned
fixture inputs, never authoring-room files, checker, gold or calibration data.
Each cell consumes one explicitly funded admission. Before calibration, a
nonclaimable prepared assignment binds its Run to an immutable case/input;
publication is separate and serialized against cancellation. Recovery fences
independently identified siblings even if another child is unbound, but holds
all refunds and retirement until the complete graph and world cleanup are
proven. A pre-binding crash remains an explicit held incident, not a guessed
case or permission to rerun work. Bounded MCP controls and the Workflows backend
selector expose this adapter to authorized operators; enrollment and explicit
admission funding remain separate. Subscription billing is unpriced, and
unreported usage or cost dimensions remain unknown rather than measured zero.
Certified reports preserve independently known billed and notional costs.
The separate host-only `ops/worker-recovery.clj` reconciles orphan exposure
without execution or late import. Missing world disposal authority keeps
cleanup unresolved; a terminal Run or discarded projection is not physical
cleanup proof. Startup invokes it after orphan Run reconciliation and before
MCP claims are accepted. `runtimes/worker-maintenance.clj` owns one periodic
reconciliation resource independent of fast receipt delivery. An initial hold
or unhealthy resource prevents new claims/admission but preserves existing
lease shutdown. `scripts/worker_driver.py` supplies private operator enrollment,
explicit allowance/task/workflow/benchmark submission and one-shot local SCI execution/recovery;
its login binds the exact MCP destination, including explicit tunnel aliases.
Benchmark retirement failures retain their exact process-local cleanup callback
and physical checker permit; reconciliation retries disposal, not execution.
Recorded Attempt evidence is distinct from mutable workspace files. See
[authority.md](authority.md#private-mcp).
Read-only current-workspace Git bundles use Geschichte's Git object/pack
implementation and a synthetic root commit, with no ancestry traversal or
server checkout/export writes. Bounded optimistic downloads reject intervening
workspace changes; they introduce neither a result database nor a retained
export store.

## Stores: one owns each concept

Everything references across a store boundary **by value** — a uuid — never by
Datahike entity id. Branches share an eid counter after a fork, so a cross-store
ref by eid is a bug that shows up as data loss.

| Store | Owns | Referenced elsewhere by |
|---|---|---|
| **system DB** (`.dvergr/system-db`, shared with dvergr) | identity (`:actor/*`, `:party/*`), teams (`:room/*`), grants, the system registry, the KB registry (`:kb/*`), proposals (`:proposal/*`), mail accounts | its uuids |
| **room store** (one per team) | chat messages, the team's book (`:kontor.*`), schedules, and the category-S projection of those | `:room/content-db-scope` |
| **KB store** (one per wiki) | pages, blocks, typed-page properties, and the property declarations for them | `:kb/db-scope` |
| **geschichte repo** (one per team) | the code agents work in | the room's repo system |
| **drive store** | files | `:drive/*` in the system DB |
| **app store** (`data/simmis-v2`) | the application's own furniture | `simmis-scope` |

A room store is provisioned by dvergr and resolved on **that room's execution
context**, not the server's — yggdrasil's registry is context-backed, so looking
a room store up from the server context finds nothing and returns `nil` with no
error. Use `model.room-databases/room-store-conn`, or `(room-conn "slug")` at
the REPL.

## Server and client

The client is not a view onto an API. It runs a **real Datahike replica in the
browser**, backed by IndexedDB and kept current by konserve-sync over a kabel
websocket. Queries are local; the server is the authorization boundary, not a
query API.

That gives two distinct channels, and they are authorized separately:

- **Control plane** — RPC. `defn-spin-remote` in `.cljc` declares a function
  that exists on both sides; the client call travels to the server handler.
  Every inbound call passes `access/authorize-remote` first. A refused or
  failed call reaches the client as a short message, a type and a correlation
  id (`runtimes/rpc_errors.clj`); the exception stays in the server log.
- **Data plane** — replication. konserve-sync subscribes the client to store
  scopes it may read. Subscription is gated; publication from a client is
  refused outright, because sync here is one-directional.

See [authority.md](authority.md) for how each is gated.

## Three rules that shape everything else

**Writes go through the server.** The client proposes; the server resolves the
store, checks `can?`, and transacts. A client never declares schema —
`:db/ident` is append-only, so a browser that could declare an attribute could
permanently fix its type.

The replica's own writer is never used for this. A write the user should see
at once is a datahike.optimistic PREDICTION over the RPC that makes it durable
(`uis/web/desktop/optimistic_write.cljc`): the RPC answers with the store's
head after the write, the prediction is dropped once the replica reaches that
head, and a refused or failed RPC rejects it and tells the user. `opt/transact!`
would hand the transaction to the replica's KabelWriter — a raw
`datahike.kabel/dispatch`, past `can?`, which this server does not serve — and
such a write never arrives and never fails. The same holds for `d/transact!` on
any replica conn and for konserve writes into a synced store;
`browser-write-guard-test` reads every ClojureScript file (and the `:cljs`
branch of every `.cljc`) and fails on any of them.

A wiki widget (`widget_sandbox.cljs`) is browser code too. Its `kb/*` writes
are `block-remote/widget-transact` (data, predicted on the KB's overlay) and
`block-remote/widget-install-attr` (an attribute declaration, sent in order
with the data writes but not predicted, since the replica cannot apply data of
a type it does not know). Widgets write only on a KB tab's trunk. A widget
block carries the db's `:max-tx` in its row and is keyed by it, so it
re-evaluates when what it shows changes.

A write that is not shown at once is still never silent. Every call site routes
its outcome through `uis/web/desktop/remote.cljc`: `rem/invoke!` and `rem/spin!`
report a failure by default, `rem/on-ok`/`rem/on-error` are the two
continuations of a spin-remote invoked from a DOM callback, and
`rem/result-failure` treats a refusal a handler ANSWERS — `{:status :error …}`,
`{:success false …}` — as a failure rather than a result. A setting shown
before the server took it (notification level, code view) is put back when
the server refuses it.

**Aggregates are views over sources.** No persisted aggregate, no denormalised
index. If an aggregate is slow, cache it on a key derived from the sources' own
heads, so a stale entry is impossible rather than unlikely.

**A failure is a value.** `nil` means never attempted, `{:error …}` means it
broke, a list means it worked. An empty list must never be able to mean
"denied" or "the schema is missing".

## Three seams

Adding a feature touches these and not much else.

**Write** — `block_remote`'s `resolve-conn`. Server-side, scope- and
branch-aware. Authorization happens here because it is the only place that can
see both the actor and the target store.

**Read** — `db_signal/ensure-view-db-signal! [scope ref]`. The single place a
`(scope, reference)` pair becomes a db value; it handles `nil`/`:now` and
`{:as-of T}`. No read path may pick a store by falling back to a default.

**Navigate** — `refs/open!`. The single place a reference becomes a tab.
Aggregate rows carry a `:ref`, and wiki and chat `[[dh://…]]` links produce the
same shape, so a new target kind is added once.

## The UI is not React

The client renders with [spindel](https://github.com/replikativ/spindel) — FRP
signals and spins producing incremental DOM. It has a handful of non-obvious
rules whose violations do not fail loudly; they are listed under "spindel sharp
edges" in [`CLAUDE.md`](../CLAUDE.md), and most UI bugs shipped here came from
breaking one.

Remote calls from the client go through
`uis/web/desktop/remote.cljc` — `invoke!`, `spin!`, `report-error!` — which
surfaces failures to the user by default.

An open wiki page follows its replica. A block's text lives in a TipTap
editor, a foreign node spindel renders the container of and never the
children, so the block's content also rides on the container as
`data-content`, an attribute spindel reconciles on every commit; a
MutationObserver on it hands the committed content to the editor
(`block_editor.cljs`, `follow-replica!`). The content comes from the
effective db — replica plus this session's predictions — so a pending write
of one's own is never overwritten by an older replica value. Every block takes
the change except the one being edited (focused, or holding unsaved typing):
that one keeps its text and shows a notice offering the other version
(`block_sync.cljc`). Blocks added, deleted or moved elsewhere arrive through
the page's `ifor-each`, whose items carry everything a block renders from —
depth, children, the conflict flag.

When a block's editor leaves the page, typed text that was not saved is never
dropped (`block-sync/unmounted`): if the block still exists (the page closed,
the block moved) the text is saved, as leaving the block would; if another
session deleted it, the text is kept in a notice on the page ("This block was
deleted in another session") that restores it as a new block where the old
one was, or discards it (`sig/orphaned-blocks`). Existence is read from the
effective db, and a block this session deleted itself is not kept.

The editor itself — TipTap, which listens on the document, plus the replica
observer — is then destroyed. spindel calls a foreign node's `:on-unmount`
with nil and fires the unmount of the LAST committed vnode, a closure from a
later render than the mount, so a handle closed over in the render body is a
different, empty one by then. `uis/web/desktop/foreign_lifecycle.cljc`
(`owned`) keys what a mount acquired by the node's place in the tree (parent
address, slot, `:key`) and releases it on unmount; the block editor, the
chat input and the Vega chart use it.

The navigation lists the wikis a user owns, those shared with them, and those
reached through a grant to a room they are in (`kbs/sidebar-kbs`), each marked
read-only when `can?` refuses the user `:write` — a room's `:read` grant — and
a read-only wiki's pages open non-editable.

## Where things live

```
dev/user.clj                 lifecycle; dev/repl.clj is the read surface
src/is/simm/
├── model/                   system DB, rooms, parties, KBs + grants, room
│                            content DBs, access (can?), billing, schema
├── runtimes/
│   ├── web.clj              server boot and the HTTP routes
│   ├── web.cljs             client boot
│   ├── http_auth.clj        route auth: the gate and its boot-time validator
│   ├── rpc_errors.clj       what a failed RPC tells the client (no traces)
│   ├── auth_config.clj      kabel-auth store, JWT, accounts from config
│   ├── branching.clj        yggdrasil branching and branch GC
│   ├── storage_gc.clj       periodic orphan GC of every store — OFF unless
│   │                        enabled (see below)
│   ├── screen_intake.clj    screen-share frames → descriptions, per-user
│   │                        store; `screen/*` for agents, through grants
│   ├── transcripts.clj      jigasi watcher + "Transcribe" → drive file and
│   │                        room message; `transcript/*` for agents
│   └── telegram.clj         adapter over dvergr's telegram channel
├── agents/                  personas, prompts, tools, summaries, merges
└── uis/web/desktop/         the spindel UI
    ├── signals.cljc         all UI signals and mutators
    ├── db_signal.cljc       client replicas, tiered stores, konserve-sync
    ├── optimistic_write.cljc a write shown at once, made durable by an RPC
    ├── tab_heal.cljc        open tabs reconciled against the room roster
    ├── backlink_target.cljc what a backlink row opens, or why it opens nothing
    ├── *_remote.cljc        RPC endpoints
    └── views/               columns, chat, nav, proposals, files, …
```

## The stack underneath

| Layer | Project |
|---|---|
| UI | [spindel](https://github.com/replikativ/spindel) |
| Agents | [dvergr](https://github.com/replikativ/dvergr) |
| Database | [datahike](https://github.com/replikativ/datahike) |
| Storage | [konserve](https://github.com/replikativ/konserve) |
| Branching | [yggdrasil](https://github.com/replikativ/yggdrasil) |
| Replication | [konserve-sync](https://github.com/replikativ/konserve-sync) over [kabel](https://github.com/replikativ/kabel), [kabel-auth](https://github.com/replikativ/kabel-auth) JWT |
| Ledger | [kontor](https://github.com/replikativ/kontor) |
| Filesystem | [muschel](https://github.com/replikativ/muschel) |
| Versioned repos | [geschichte](https://github.com/replikativ/geschichte) |
