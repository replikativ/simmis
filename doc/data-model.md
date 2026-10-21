# The data model

## File-backed room Memory

Room Memory is the room's existing Geschichte repository, not another drive
or a writable browser store. Folder listings carry worktree path/content/size/
mode metadata; file contents are fetched by the captured content identity.
Markdown is recognized by extension at any path. `wiki/` is an organizational
convention, not a separate database boundary. Room-local source and workflows
use these same files; the loaded SCI interpreter state is not a file entity.

The Markdown codec retains exact source and derives source-positioned syntax
nodes separately from semantic Simmis blocks. Explicit paired block markers
carry stable block identities and child grouping; ordinary unmarked Markdown
has provisional blocks. A list inside a block does not implicitly become its
Simmis children. File YAML properties, typed property projections, and the
source-selected disposable query view do not install untrusted schema or
execute imported code/widgets. Existing legacy KB entities are retained; this
surface is not a completed migration of their identity or widget semantics.

## Existing KB vocabulary

Everything stored in a KB or a room store is described by one small vocabulary.
You cannot read the wiki, type or property code without it.

Host-side work admission is control-plane data in the system DB, separate from
the speculative KB/room worlds. `model/work_requests.clj` declares typed
`:work.request/*` identities, ownership, limits, lifecycle and job-pointer
attributes. The complete immutable intent uses a Datahike `store-ref`, not an EDN
string or an exploded runtime object. Atomic insert/CAS transitions decide who
may schedule; Dvergr's room-store Jobs, Runs, Attempts and accounting remain the
execution and result records. Fork rollback does not duplicate the host admission
or its conserved resource authority.
The same journal admits `:task`, `:benchmark` and `:allowance` requests.
Benchmark intents pin the client submission, canonical ExperimentDef, resolved
candidate roster, grading policy and aggregate limits. Fixtures use the durable
request UUID as their host-side locator; this is not another result store.
The canonical ExperimentDef also freezes bounded workflow provenance (name,
title and admitted bundle ID). Completed leaderboards use that frozen label,
not current authoring files, and retain the experiment identity for inspection.
Calibrated WorldSetup references include the original bundle-bearing fixture
identity and execution policies. Successful setup/calibration evidence stays in
the existing Attempt evidence, not a separate calibration database.
Restart recovery retires settled fixtures even if the wallet-return write
completed immediately before a process crash.

Pinned workflow activations and their occurrence outbox also live in the host
system DB (`model/workflow_schedules.clj`), not copied room worlds. Typed
`:schedule.activation/*` rows retain the delegator, room, target revision,
GC-tracked immutable target, enablement, cursor and unresolved occurrence.
The target retains source bytes, literal execution settings, normalized cadence,
host policy and concrete model-metadata digests. At most100 activations are
admitted per host. A serialized writer advances the cursor and inserts one
deterministically identified occurrence together. An unresolved predecessor
records an overlap skip instead of allocating another wallet. Occurrences
record admission facts and canonical request links, not duplicated execution
state. Reads expose twelve recent occurrences; execution uses the active pointer.
Pause atomically withdraws pending work but does not cancel admitted Jobs.
Restart links an existing submitted request or holds an ambiguous admission for
reconciliation; it does not blindly replay it.
Settled occurrences reserve a deterministic room-message UUID. Publication uses
the existing durability-before-visibility room path, followed by a typed
`:schedule.occurrence/announced?` acknowledgement. A lost acknowledgement retries
the same envelope, not the execution. The room note links its canonical Job and
does not wake agents. Its unresolved pointer remains held until publication.

`model/workers.clj` declares the host control-plane vocabulary for separately
enrolled local workers and external assignment ownership. Enrollment records
hold the enrolling party/room, generation, credential verifier hash and a
GC-tracked immutable configuration. Assignment identities are the existing Run
UUIDs and link to the request, Job, selected worker and frozen input artifact.
Writer-serialized claims bind one client nonce and lease proof hash; the same
Run is never reassigned after expiry or cancellation. Plaintext credential and
lease proofs never enter database transaction arguments.
The client nonce UUID is also the lease UUID. Persisting Run, nonce and proof
before claiming therefore retains shutdown authority even if the claim reply
is lost and enrollment is subsequently revoked. Before commitment that proposed
bearer authenticates nobody; after revocation it grants shutdown operations
only. A lost reply never authorizes execution.
Each enrollment represents one physical worker slot: another assignment cannot
be claimed while a previous lease lacks its shutdown acknowledgement, even
after expiry or quarantine.
Host publication withdrawal can install a same-intent withdrawn tombstone
even if enqueue's result was ambiguous. A late enqueue cannot revive it. If
a worker already claimed, the same transaction quarantines that lease without
acknowledging shutdown. Closing-state reconciliation checks the transition,
not merely the existence of a matching intent.

These assignment states are transport receipts, not another execution model.
Expiry/revocation quarantines ownership without inventing physical shutdown;
only withdrawal before a successful claim, or an existing lease's explicit
shutdown acknowledgement, records that boundary. Completed output binds one
immutable digest and the worker's shutdown assertion. This is an enrolled
worker's assertion, not independent proof about a remote process. Immutable
configuration, input and output reads verify their original logical digest;
corrupt payloads are refused rather than substituted from the current workspace.
`ops/worker_credentials.clj` supplies separate enrollment/lease credential
validation and a worker-only authority predicate. It intersects the fixed room
with the enroller's current permissions, keeps only verifier hashes in process
principals, and retains narrowly scoped shutdown signaling after revocation.
`ops/worker-service.clj` implements closed, bounded pending/claim/input/heartbeat/
completion/stop operations; `ops/worker-api.clj` supplies their worker-only MCP
bindings. Application startup registers them through `runtimes/application-ops`
before browser requests are accepted. The separately configured loopback MCP
listener dispatches recognized worker/lease credential prefixes only to the
worker authenticator, never to human/admin authentication. Disabling new
admission does not remove an existing lease's shutdown path. These operations
add no agent sandbox capability.
Claim, heartbeat and completion check current permission and the host clock
inside the serialized writer transaction, including after output publication.

Frozen input contains only explicitly selected regular files (at most 100,
1 MiB each, 2 MiB total), task, pinned adapter configuration and execution
limits. The input JSON is delivered in 16 KiB chunks with a full digest; no
live room file read is substituted. Output carries validated base64, byte
length, SHA256 and regular-file mode. It may change selected inputs or create
`out/` deliverables, but cannot change other room files. HTTP filename keys are
normalized without dropping path components and duplicate normalized names
are refused. Responses omit internal payload references and verifier hashes.
Completion records immutable output and the worker's shutdown acknowledgement;
it does not import files, certify an Attempt or merge into the parent room.

`ops/worker-wait.clj` supplies a host-owned shutdown wait for protocol adapters.
Its database listener and sticky Run cancellation hook register before
assignment publication; both callbacks only offer a bounded wake token. The
owner rereads durable state after each event. The current lease deadline may
trigger cancellation, but quarantine, failed writes and unavailable reads
cannot stand in for a shutdown receipt. A running Dvergr protocol worker stays
physically owned while this wait continues, including after interruption.
Interrupt status is restored only when the owner closes its listener/hook
after acknowledgement and DB cleanup. Returned assignments are private host
data, not portable Run results or import authority: an adapter must construct
sanitized evidence and separately check current authority before importing.

`ops/worker-output.clj` is the trusted import primitive for that isolated Run
world. It refuses parent rooms, validates output against the frozen selected
inputs, and checks an adapter-supplied current-authority predicate at every
file/commit effect and before returning. Content reuses Dvergr's atomic batch;
executable mode changes use a second gated commit under the same workspace
lock. These two commits are not one atomic transaction. Final repository
read-back checks bytes, modes and deletions, including effects intercepted by
world handlers. A failure may leave partial changes in the isolated world,
never trunk, and must fail the Run. The adapter supplies the verified immutable
assignment payload and current canonical Run world; this helper neither
certifies successful task execution nor interprets a nonzero CLI exit code.

`ops/worker-work.clj` connects these host primitives to existing Dvergr Jobs,
isolated Runs and certified Attempts. Only the enrolling human, with current
room read/merge authority, may submit. The immutable request pins worker
generation/configuration, selected files, task and limits. Exact retries return
the original Job even after parent files change or enrollment is revoked;
changed intent conflicts. World setup refuses input drift before publication.

Local execution uses a separate conserved `local-worker-admission` coordinate,
explicitly provisioned by a human room owner. One admission is consumed before
remote publication; a failed publication attempt still consumes it. This is
neither a provider-call count, USD measurement nor proof of CLI startup.
Native money/dispatch balances cannot fund it. Typed
`:work.request/max-worker-admissions` metadata records the allowance/limit.
The shared work adapter accepts trusted prepared resource vectors; external
request schemas cannot inject them.

The Run waits for physical stop, then checks current enrollment/grants,
cancellation, exact live Run/Job and canonical world before each import effect.
Capture uses a fork-local Spindel context marker and rereads actual files/modes,
not the worker's asserted result alone. Nonzero exit fails the Run while
retaining captured diagnostics; completion reward requires a completed Run,
successful import and exit zero. Receipt spend is explicitly unpriced because
remote subscription usage is unreported. Provenance distinguishes local-worker
execution and host versus MCP submission. New human admission is opt-in.
Generic restart recovery excludes both `:local-worker` and
`:local-worker-workflow` and `:local-worker-benchmark`: a dead
local JVM cannot certify that a remote process stopped.

Benchmark assignments optionally add an immutable `:simmis.worker.assignment/cell`
UUID. Their v2 intent digest includes this binding; ordinary v1 digests are
unchanged. The host-only `:prepared` state pins a Run/case/input before server
calibration but is neither pending nor claimable. Only a separate serialized
transition can queue it after admission consumption. Cancellation withdraws
prepared/queued rows with a no-lease acknowledgement; it never invents shutdown
of a leased process or resurrects a withdrawn intent.

`ops/worker-recovery.clj` is a separate host-only reconciliation entry point,
called by startup maintenance and authenticated receipt handling. It validates the frozen request,
complete Job/Run graph and actual Run provenance before closing publication.
Missing/queued assignments get a same-intent withdrawal fence; leased work
holds its resources until physical stop acknowledgement. It can discard an
exact canonical uncertified deferred world through Dvergr's existing fork
capability, or preserve an exactly matched certified review world. Only then
does it return remaining wallets bottom-up and mark the request returned.
Before either world decision it joins the Job's detached evaluation cleanup
group and rereads the durable child/Attempt; terminal Runs do not stand in for
physical completion of capture or certification workers.
Missing runtime world handles remain `:awaiting-world-cleanup`, even when the
Run projection says `:discarded`: that projection precedes physical substrate
cleanup. A crash after discard but before resource return can therefore also
remain unresolved; this adapter does not fabricate a durable cleanup receipt.

`scripts/worker_inputs.py` supplies the local input boundary without starting
an executor or authenticating a client. It checks the pinned configuration,
closed policy, immutable chunk identities/offsets/size and full SHA256, then
each file's size/digest/mode. Duplicate JSON fields, non-JSON constants and
invalid UTF-8 filenames are refused. Materialization creates a fresh private
directory and uses descriptor-relative exclusive/no-follow opens, never an
archive or existing checkout. Runtime control paths such as `.codex/`,
`.claude/`, `.agents/` and `.mcp.json` are not accepted as task data that a CLI
could auto-load. No credentials or unselected files enter that directory.
`scripts/worker_outputs.py` collects results only after its caller has proved
process-tree quiescence. It reads changed/deleted selected inputs and regular
`out/` deliverables, not a recursive snapshot of the workspace. It preserves
binary bytes and executable mode, omits unchanged files, and enforces the same
file/count/total limits plus a bounded directory walk. A private owned root,
no-follow descriptor-relative reads, pre-open special-file checks and
post-open identity checks refuse symlinks, hardlinks, devices/FIFOs and
privileged modes. Collection itself grants no shutdown acknowledgement,
server-side import, certification or merge authority.

`scripts/worker_claim.py` persists the private client claim identity before any
network claim. Atomic no-overwrite publication and file/directory fsync preserve
one Run/nonce/proof across retries; corrupt or mismatched tickets are never
replaced. Its caller must first durably create the private state directory,
including its parent entry. Inspection alone is not the durability barrier.
A per-enrollment OS lock spans admission, process lifetime and cleanup; lock
files must not be removed/recreated while another controller could own them.
Process death releases the lock, not an orphaned process tree or server lease.
Replacement controllers must reconcile those before executing, and retain
ambiguous claim tickets until ownership/shutdown is resolved. Crash-left private
staging files are non-authoritative. This helper performs no network calls or
execution and keeps no shared application state.
The Python MCP transport accepts a proposed proof only as a session header,
starting with initialization, never as a tool argument. Its ordinary read
client is unchanged. The synthetic cross-language HTTP regression exercises
ticket preparation, client-side claim, verified input download/materialization
and bounded completion with binary data, executable modes and deletions; it
does not start a coding CLI or call a model.

## Objects, morphisms, instances

The model is a category, called **S**, stored as ordinary Datahike entities.

- An **object** is a type: `S/Page`, `S/Message`, `S/Block`, and the primitives
  `S/String`, `S/Number`, `S/Float`, `S/Boolean`, `S/Date`.
- A **morphism** is a property: it has a source object, a destination object, a
  cardinality, and a name like `S/Page/title`.
- An **instance** is a page, a message, a block. It points at its type with
  `:instance/of-role`.

So "a page has a title" is a morphism `S/Page/title : S/Page → S/String`, and a
particular page is an entity carrying `:instance/of-role` and a value under the
attribute that morphism names.

## Where a morphism's values live

This is the part that surprises people, and it has exactly one implementation —
`model/morphism.cljc`:

```clojure
(defn attr-of [morphism]
  (or (:morphism/storage-attr morphism)
      (name->attr-ident (:entity/name morphism))))
```

Two cases:

- **Derived.** `"S/Page/title"` → `:S.Page/title`. The attribute is computed
  from the morphism's name.
- **Reflected.** The morphism carries `:morphism/storage-attr`, a categorical
  view onto an attribute a domain system already owns — `:entity/created-at`,
  `:kontor.posting/amount`. The value lives where that system puts it; the
  morphism only describes it.

Reflected is the **majority** on an installed store — including `created-at`
and `updated-at` on `S/Page`, which is every wiki page. Code that derives the
attribute from the name and skips `:morphism/storage-attr` reads blank for most
properties, and does so silently. Always go through `morphism/attr-of`.

## Schema types

A morphism's destination object determines its Datahike `:db/valueType`, via
`schema/codomain->db-type`. The **server** derives this; a client never sends
it. `:db/ident` declarations are append-only, so a client that could declare an
attribute's type could fix it permanently. Every path that declares one — a
type page's `add-property`, a widget's `kb/install-attr!`
(`widget-install-attr`) and an agent's `kb/install-attr!` in its sandbox —
names the codomain object, and the server derives the type and records the
morphism (`attributes/declaration`, for the latter two).

Note `S/Number` is the integer primitive (`:db.type/long`) and `S/Float` is the
decimal one (`:db.type/double`). A UI number field that accepts decimals names
`S/Float`.

The attribute's type also decides what a property VALUE must be. The page
header's property box picks its editor from the attribute's `:db/valueType`
(`page-properties-query` puts it on each property), not from the morphism's
UI hint — katzen's `kind : Entity → Keyword` projects with hint `:text`, and a
text box sends a string. A keyword attribute gets an input that produces a
keyword, and `:S.Page/kind` a select. `save-property` refuses a value that does
not fit the attribute's type, with the reason, before it transacts
(`model/page_kind.cljc`, `value-fits?`).

A wiki page in the atom/overview/index layout records what it cites:
`S/Page/source : Page → String`, cardinality many (`:S.Page/source`, a set of
citation targets — `doc`, `doc#Section` or `dh://…`, e.g.
`transcripts/2026-10-16-1400-planning.md`), and `S/Page/entry-point : Page →
String` (`:S.Page/entry-point`, where a reader of the source starts). Both are
seed morphisms, so every store gets them at its next install. The layout
inside the page is block-level: a claim is a top-level block whose HTML starts
with `<h2>`, evidence one block `<blockquote>"…"</blockquote><p>[[doc#Section]]</p>`,
and a `<strong>Sources:</strong>` block closes it — the shape dvergr's wiki/v4
checker projects to Markdown.

A screen-share frame (the sharer's own screens DB, `runtimes/screen_intake.clj`)
carries exactly one of `:screenshot/transcript` (its description) and
`:screenshot/failed` (why there is none). A screen-share grant
(`:screen-grant/*`, system DB) is one WINDOW: reopening a closed share opens a
new grant entity, and the old one stays with its `:until` — or is closed at its
last heartbeat when its client died — so past windows bound what a room may
read after the meeting.

`:S.Page/kind` holds two sorts of kind. The wiki layout's — `:wiki/atom`,
`:wiki/overview`, `:wiki/index` — are a person's to set. `:bootstrap` (seeded
pages; the timeline hides them) and `:chat-summary` (the summarizer's record
pages; the nav hides them) are the system's: `save-property` refuses to give a
page one of those, or to change or clear one (`page-kind/change-refusal`), and
the property box shows them without an editor.

## One abstract schema, two bindings

A KB and a room store hold the same shapes under different names, and
[katzen](https://github.com/replikativ/katzen) supplies the abstract schema
both are bound to. `model/store.clj` installs the simmis half on every store,
so a KB can hold a book and a room store can hold wiki pages, and neither
acquires an attribute the other lacks by accident.

`model/katzen_projection.clj` projects the abstract schema onto a concrete
store, and records the exact attr-type on the morphism so the distinction
survives the round trip — the two tables disagree in one documented place
(`:Number`), and a test asserts the rest.

## Cross-store references

A reference between stores is a value, never an entity id. Branches share an
eid counter after a fork, so a cross-store ref by eid is a bug that presents as
data loss.

Within a KB, `[[Title]]` links a page. Across databases the explicit form is
`[[dh://…][Display]]`. Links are stored as datoms, so backlinks and
neighbourhood queries are ordinary queries rather than a maintained index.

The same rule governs what a backlink row can be clicked into. A row opens a
tab from a uuid — a page's `:entity/uuid`, a room's `:room/id` — never from a
display name. Room names are not unique, so a row that carries only a name
names no room; `uis/web/desktop/backlink_target.cljc` reports such a row as
unavailable rather than guessing one.

## Labels

A label is a person's verdict on reviewed work (`workflows/review.clj`), one
entity per decision in the ROOM's store, identified by `:entity/uuid`, with
attributes under `:S.Label/*` (`schema/label-schema`, part of `full-schema`):

| attribute | |
|---|---|
| `subject-kind`, `subject-id` | `:proposal` or `:run`, and its uuid |
| `about` | `:workflow` (the work) or `:reviewer` (the reviewer's approval) |
| `verdict` | `:accept`, `:reject` or `:edit`; about a reviewer, `:accept` agrees |
| `review`, `policy`, `reviewer-verdict` | the dvergr Review, its spec's content id, what the reviewer said |
| `workflow-id`, `workflow-version` | when known |
| `edit` | the diff a person applied before accepting (verdict `:edit`) |
| `by`, `reasons`, `at` | who decided, why, when |

Labels are append-only: nothing updates or retracts one. `review/labels`
reads them back as the `:label/*` maps of the workflows contract
(`{:label/subject [:proposal id] :label/about … :label/workflow {:id
:version} …}`). There is no `S/Label` object in category S yet; like
`S.EvalEntry`, the attributes are simmis's own.

## Ordering

Blocks are ordered by `:block/order`, a fractional index — a string chosen to
sort lexicographically between its neighbours, so inserting never renumbers
anything else. `model/fractional_index.cljc` generates them. The alphabet is
printable ASCII, not the base-62 of the reference implementation the algorithm
came from.
