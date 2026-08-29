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

The adoption row is durable before authority leaves Dvergr. A multi-system
world is exhaustively partitioned so each Proposal component governs exactly
one system and can be accepted or dismissed independently. The stored
descriptors, Run/room linkage, status and settlement decisions are durable;
the actual fork handles remain process-local capabilities. Accept and Dismiss
therefore use the ordinary Proposal authorization and status machinery but
dispatch `:world` components through Dvergr's governed settlement frontier.

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
