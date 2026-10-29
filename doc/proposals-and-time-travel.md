# Proposals + time travel

How an agent's write reaches trunk, and how any store can be read at a point
in the past. The two are one mechanism: a *reference* names which version of a
store you are looking at, and a proposal is a set of references waiting to be
accepted.

Read [architecture.md](architecture.md) first for what the stores are.

## 1. Ref algebra — the semantic foundation

Systems (KBs, drives, room stores) are INDEPENDENT yggdrasil systems;
the "global system" is composed on the READER side. Three ref kinds with
distinct consistency guarantees — never conflate them:

- **LocalRef** `(system-scope, commit | branch | as-of-tx)` — writer-side,
  commit-addressed, per system. What the History subway shows and what
  clicks on commits produce.
- **GlobalCut** — a wall-clock instant `T`, resolved reader-side per
  system to `as-of(T)`. NOT an atomic snapshot: torn cross-system
  operations (e.g. summary page written, marker not yet) are an accepted,
  *documented* anomaly ("each system at its state nearest T").
- **ForkSet (= Proposal)** — a named `{scope → branch}` map + metadata in
  the SYSTEM DB: the only first-class JOINT multi-system state. Travel
  into the proposed future is *more* consistent than travel into the
  past: the future-ref is writer-intended.

Growth path: yggdrasil `workspace.cljc` coordinated commits become
writer-consistent "solid detents" on the global timeline; proposal
ACCEPT is the first joint-commit site.

## 2. What exists

- Drives ARE yggdrasil-registered (`drives.clj` → `register-kb-conn!`,
  `:branch-history? true`). Unification = rename to `register-system!`
  + neutral accessors + drive tx-listener parity.
- KB branches ARE client-replicated; `branch-as-db` projection ships in
  block_editor (1759-1827) — LIFT into the db-signal registry, deprecate
  `sig/projected-branch-dbs`. Drives have NO client replica (files panel
  is remote-snapshot) → drive as-of/preview stays remote.
- `kb-conflicts` = real 3-way conflict detection — use at accept.
- Sidebar search box exists (`nav-search-query`); time strip goes under it.
- `kb-commit-graph` returns `{:nodes {id {:parent-ids :meta}} :branches
  :roots}` — subway-ready.

## 3. In the UI

- **Global time strip** (new `views/time_strip.cljc`, under `.nav-search`):
  collapsed `● Now`; expanded = continuous time slider snapping to
  activity clusters; solid detents = joint commits (future); `⑂` stops
  beyond Now = open proposals. Non-Now ⇒ whole workspace renders at the
  GlobalCut + workspace banner ("Viewing <t> — Restore… / Back to Now";
  proposal variant: Accept / Dismiss / Back). Read-only everywhere
  (TipTap replaced by static rendering; editor keys extended with the
  ref token so flips remount; chat input + uploads disabled).
- **Per-column pins**: `:pinned-ref` IN the tab's data map (ifor-each
  item-equality ⇒ correct re-render), created via "open in new column,
  pinned"; 📌 chip; resolution = `pin || global-ref || :now`.
- **Proposals inbox**: new `:proposals` tab type + sidebar badge. Cards:
  AI summary, per-system sections rendering CONTENT-NATIVELY — KB ops as
  static block renders (NOT TipTap) anchored under page titles with
  green/red rails + before/after stacks; chat ops as chat bubbles; drive
  ops as file rows. Accept (conflict/overlap warnings → force) /
  Dismiss / Preview. Old branches tree rows belonging to a proposal link
  to the inbox.
- **History subway** (context panel, beside Backlinks): per-system DAG,
  clickable commits (trunk-past → global as-of; other → pinned column).
- **Undo**: `restore-subtree!` (page = block-subtree; restore is a new
  forward commit) on Feed audit entries + block context menu + sandbox
  `kb/rollback!`.
- Search under non-Now refs: fulltext is head-only → title-scan + hint.

## 4. Client ref plumbing

One `global-ref` signal (signals.cljc). `ensure-view-db-signal!
[scope ref]` in db_signal.cljc: `:now` → the UNTOUCHED existing per-KB
signal (zero regression); non-Now → lazily minted per-(scope,ref) signal
in the same plain-atom registry idiom, filled by a projector (as-of →
client `d/as-of`; branch/proposal → the lifted branch-as-db go-block;
drive scopes → remote snapshot descriptor). Resolution happens ONLY at
the existing db-resolution sites: `render-column`, `render-page-editor`,
nav section spins, chat-tab room-db resolution — tracks at spin tops
(sharp edge #1). Registry hygiene: drop (scope,ref) signals unreferenced
after Back-to-Now.

## 5. The proposal model

System-DB schema: `:proposal/id·title·summary·status·author·room·
created-at·resolved-at·forks(component,many)` +
`:proposal.fork/scope·branch·base-commit·system-type`.
Ops ns `ops/proposals.clj`: `file-proposal!`, `list-proposals`,
`accept-proposal!` (per fork: kb-conflicts + entity-overlap
`diff(base→trunk) ∩ diff(base→branch)` → warnings; merge; discard
branch; later: workspace joint commit), `dismiss-proposal!`.
Broadcast: `:proposal/filed|:proposal/resolved` on the existing
`:branching/event` topic.

### Canonical chat projection

A room-scoped Proposal reserves `:proposal/message-id` in the same system-DB
transaction that creates the ForkSet. The id is deterministic from the Proposal
UUID, so retrying an ambiguous proposal commit cannot mint another card.
`ops/proposal-publication.clj` then posts one top-level Dvergr message carrying
`{:object {:kind :proposal :id proposal-id}}`. Dvergr persists that immutable
envelope before making it visible. Only then does Simmis advance
`:proposal/message-status` from `:pending` to `:published`.

The two durable writes deliberately are not presented as an atomic cross-store
transaction. A crash after the room post leaves `:pending`; retry posts the same
message UUID, so Dvergr's atomic first-write-wins contract turns it into a no-op
and Simmis can finish the projection. The Proposal/ForkSet remains the governance
object, while its chat message is the conversational home and thread root.

Sandbox: per-(room,agent) overlay atom (NOT ctx-forking — matches the
conn-fn indirection agents already use). `proposal/start!` (lazy branch
mint on first write; overlay consulted by kb/* conn-fn AND the /drive
mount resolution — verify dvergr mount wiring, else v1 = explicit
proposal drive ops), `proposal/file!` (semantic diff → cheap-llm summary
→ registry row), `proposal/abandon!`.

**Code filing and retained evidence.** `proposal/file!` serializes with the
proposal's file-tool writers, stages the complete private worktree (including
additions, deletions and modes), and commits dirty files before publication.
Agents need not remember a separate Git commit. Empty code-only proposals refuse
filing without dropping their workspace. Publication/storage failures preserve
private handles and an identical prepared filing value for retry; cleanup happens
only after the canonical Proposal acknowledges that handoff. Native file-tool
mutations and `memory/write!` share this freeze boundary; the tool caller never
holds the lock while waiting for the interpreter worker. This ordinary proposal
adapter is not a whole-world fork: the current Dvergr SCI `git/*` and shell
bindings still select their ChatContext world rather than this private workspace.
Use file tools and `memory/write!` for proposal edits; Git/shell proposal routing
needs a separate integration with Dvergr's workspace resolver.

Each code contribution records immutable base and filed-head commit IDs, retained
under an internal `refs/proposals/` ref, plus typed check results naming that exact
head. Checks run in a fresh private workspace reset to the frozen commit, and can
read bounded files only through its virtual filesystem. Acceptance merges the
filed commit, not a later branch head; conditional ref cleanup preserves concurrent
newer branch work. Resolved code diffs and checks remain readable through the
retained-audit path after review-branch deletion. The storage policy remains
orphan-only/full physical history: this is not permission to prune historical
Datahike checkpoints with a time-based retention window.

**Campaigns (2026-07-27).** A second overlay, per ROOM rather than per
agent: `proposal/open-campaign!` opens it, `proposal/join-campaign!`
opts an agent in, and its members' forked writes record into the shared
overlay so `file!` produces ONE proposal carrying everyone's work.
Without it, four agents on one change filed four proposals and nothing
in the model said they were one change. `start!` is unchanged and still
private; the two are mutually exclusive per agent (each verb refuses
while the other mode is open) and `abandon!` inside a campaign withdraws
only that agent's forks.

Overlay forks are keyed `[scope agent]`, not by scope. Two agents
writing the SAME database inside one campaign therefore get a branch
each and are filed as two forks. That is what keeps `accept-fork!` /
`dismiss-fork!` meaningful — a fork is the unit a reviewer accepts or
refuses, and datoms from two agents fused onto one branch cannot be told
apart, let alone refused apart. The cost is that two agents editing the
same page produce two forks that may conflict at accept time, which
`fork-conflicts` surfaces before landing; the alternative was
last-writer-wins on a shared branch, silently.

**Book forks (2026-07-27).** `kontor/*` is routed through the same
overlay (`room-agents/add-book-ns!` re-installs the namespace dvergr's
generic injector bound, with a conn-fn instead of a fixed conn), so an
agent's `entry!` inside a proposal posts to a branch of the ROOM store
and the postings are proposed rather than live. The room store is
registered under `kb:<scope>` on the server ctx by
`room-databases/connect-room-database` — dvergr's own registration is
`room-msgs-<name>` on the ROOM ctx, which none of the branching paths
look at. Fork `:system-type` is `:book`.

Governance survives the merge: `kontor.governance/govern!` registers a
`datahike.tx-preds` predicate by store-id, and datahike's
`writer/default-write-fn-map` wraps `'merge!` in `with-tx-pred` exactly
as it wraps `'transact!` — so an unbalanced branch is rejected when it
merges, not only when it is written. Pinned by
`test/is/simm/ops/book_fork_test.clj`.

### Retained Run worlds

A Dvergr Run executed in an isolated subworld is an execution capability, not
itself a Proposal. When a completed Run has substantive forked substrate and is
explicitly filed, `ops/run_world_proposals.clj` transfers its affine authority
from Dvergr into a durable Simmis adoption and promotes that adoption into the
existing ForkSet model:

```text
Run world --adopt--> world adoption --partition by system--> Proposal forks
```

The adoption row is durable before authority leaves Dvergr. Every world,
including a singleton, is exhaustively partitioned so each Proposal component
governs exactly one system and can be accepted or dismissed independently.
Partitioning seals the scope and consumes the original settlement authority.
The stored
descriptors, Run/room linkage, status and settlement decisions are durable;
the actual fork handles remain process-local capabilities. Accept and Dismiss
therefore use the ordinary Proposal authorization and status machinery but
dispatch `:world` components through Dvergr's governed settlement frontier.

Landing requires both the deciding human's room merge authority and their
merge authority on the exact registry target; room ownership alone does not
authorize another person's KB. Runtime target identities use Dvergr's canonical
registry mapping, with unknown or ambiguous targets refused. Immediately before
journaling a merge, the durable originating room's current write grant is
checked against the actual fixed leaf and its delta. This preflight is not
atomic with independent grant updates. This origin-room write-grant preflight
does not gate discard or commit-only recovery; ordinary human Proposal-control
authorization still applies. Previously adopted unpartitioned singleton handles cannot
pass the fixed-leaf merge check; this is a fail-closed change, not a migration.

Failures preserve the last valid affine capability. A failed Proposal commit
retains the adopted world for `retry-promotion!`; failure while constructing
partitions retains the transferred root and retries partitioning rather than
filing an ambiguous aggregate component. A failed settlement retains the exact
component for `retry-settlement!`. Once Dvergr ancestry release succeeds, the
live handle is dropped immediately even if the final Datahike status projection
fails, preventing a consumed capability from being offered twice.

After a process restart the Proposal remains available for audit, comments and
history, but merge/dismiss actions are marked unavailable because descriptors
do not recreate affine handles. Durable handle rehydration is a future Dvergr /
Yggdrasil capability-store concern; Simmis deliberately does not reconstruct
authority from serialized data.

### Review escalations and labels

dvergr's review stage (`dvergr.agent.review/review!`) decides a retained world:
deterministic checks, an optional reviewer, then a merge pinned to the state
it reviewed, an escalation to a person, or a discard. Each decision is one
immutable Review record in the room's store. `workflows/review.clj` is the
simmis half.

`review-run!` runs a review on its own worker thread (a review blocks: diffs,
checks, a merge). A merge or discard goes through `ops.dvergr/invoke` AS the
reviewer's party, so simmis's authorizer decides whether the reviewer may land
anything. A room's `:merge` is its owner's, so until delegation exists only a
review run as the owner auto-merges; any other reviewer's approval escalates
with the refusal as its reason. Nothing starts reviews on its own yet.

**An escalated Review becomes a proposal.** The trigger is the Review RECORD:
a datahike listener on each simmis room's store (`ensure-listener!`, armed by
a dvergr register hook from boot, `install!`) sees `:review/decision
:escalated` and queues the filing on a single escalation worker. dvergr also
posts an escalation message, a typed room message since dvergr 0.1.291
(`:kind :review/escalated`, object `[:review <id>]`). The record stays the
trigger: the message files nothing, and the chat leaves it out
(`datahike-query/covered-by-another-item?`), because the proposal card says
the same with the controls. `sweep!` at boot files what a crash left behind.

The proposal's id is a function of the Review id, and `:proposal/review` is a
unique identity, so seeing a Review twice files nothing new. It is filed in
ONE transaction (`file-proposal!`'s `:attrs` and `:comments`): the Review's
id, state token, policy, spot-check flag and an EDN copy of its checks and
verdict (a Review is immutable, so the copy cannot go stale), plus the
reviewer's notes as the first comment, authored by the reviewer's party. Its
one fork is `:system-type :review-world`: scope = the Review id, branch = the
world's fork id, authority scope = the room's store scope. The world stays
dvergr's live fork; `branching`'s `:review-world` backend only reads it (state
token as head, changed paths as delta) and refuses `land!`/`drop-branch!`.
Filing posts the proposal card into the room, as every proposal does.

**Deciding.** `accept-proposal!` / `dismiss-proposal!` (and the per-fork
verbs) route a review's proposal to `review/decide!`. Accept invokes dvergr's
`room_merge` with `:expect-state` = the reviewed token, as the deciding party;
a world that changed since it was reviewed is refused with `:world-changed`
and the proposal stays open. A merge dvergr reports as failed (spindel's
`::overlay-lost`, a fork that no longer holds the writable copy it was minted
with, is one) is refused the same way, as `:proposal/merge-failed` with
dvergr's reason; nothing is recorded. Run-world settlement turns a lost
overlay into the same error. Sibling worlds that were each reviewed in turn
both merge: `:review/id` is a `:db.unique/identity` since dvergr 0.1.291, so
the second merge's replay of the Review it inherited upserts. Reject invokes `room_discard`. Who decides: `:merge`
on the room (its owner), asked by `decide!` and again by dvergr's authorizer
for the op. A dvergr fork id (`<slug>_fork_fork-<hex>`) answers as its room
(`access/dvergr-resource`). Run-world adoption (`adopt!`, above) is NOT used:
a transferred world leaves the registry, its settlement has no state pin, and
its merge would not commit uncommitted files.

A spot-checked review — the reviewer approved, and the review was sampled
for a person — escalates in dvergr rather than merging, so it is the same
kind of proposal, marked `:proposal/review-spot-check?` and titled "Spot
check"; the card asks "Agree — merge" / "Disagree — discard".

**Labels.** Every decision on a review's proposal records a label in the
ROOM's store (data-model.md, "Labels"): `:workflow` (was the work right),
and on a spot check also `:reviewer` (was the approval right; `:accept` =
agree). Append-only — no RPC writes or removes one. Read with
`review/labels` and the `list-labels!` RPC (a room read); the room settings
page lists them under "Review decisions".

### Retained file-output acceptance

A single `:file-output` component has its own fixed reviewed acceptance path;
the generic whole-world/fork Accept actions do not adopt it. The shared
Proposal inspector (also reached from chat and Tasks) retains its before/after
comparison and offers **Check current workspace** then **Accept reviewed
files**. The server validates current human authority and the closed filing
before hydration, then owns application and exact Room lifetimes throughout.
Acceptance recomputes canonical source/definitions/constraints and exact target
identity, compares the expected review revision before claim, and uses the
native prepared-snapshot/checkpoint guard before publication. Version-2 journal
intent includes the review revision; this digest is attribution, not a historical
database snapshot. Mixed-component Proposals remain review-only.

Lost acknowledgement stays uncertain, never permission to retry publication.
**Check recorded completion** reauthorizes separately and reconciles the
original scoped receipt, including accepted history. It does not reload old
bodies or overwrite subsequent edits/checkouts. Review/action tokens live in
context-backed card state; stale requests and connection/principal epochs cannot
restore an obsolete Accept action.

## 6. Semantic diff

Server ns `ops/semantic_diff.clj` over `DatahikeDiff` (has FULL
`:added`/`:removed` datom colls). Group by entity; classify block
add/edit/move/remove (anchor under page via `:block/parent` walk),
page create/archive/rename, drive file add/rename/move/remove/modify,
chat message ops; fallback datom counts. Wire shape: transit-safe maps,
block content as HTML strings (client renders natively).

`:book` (2026-07-27): the branch's new postings grouped into their
kontor transactions — narration, effective date, journal, and the
debit/credit lines with account path, amount and commodity, plus a
`:balanced?` per entry. Amounts are EXACT decimal strings (the attribute
is `:db.type/bigdec`; a double would round and a BigDecimal is not
transit-safe), padded to `:kontor.commodity/precision` only where that
adds trailing zeros. Unlike the KB and drive diffs this one is not
identity-keyed — kontor transactions and postings carry no
`:db.unique/identity` attribute — and it is additions-only, because the
governor refuses to retract a posted row. Rendered by
`views/proposals.cljc`'s `:book` branch as ledger rows.
