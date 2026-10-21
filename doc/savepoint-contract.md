# Savepoints: state, continuation, ownership and recovery

Status: cross-layer contract and verification plan, 2026-10-20. Requirements
below are not a claim that every current path satisfies them. This document
separates the intended model from source-confirmed limitations and executable
evidence still needed.

Audit basis: Simmis `1c63261`, Dvergr `8b115db`, its pinned Spindel `0cda034`,
Yggdrasil `0.3.49` and Geschichte `0.1.22`. Sibling working trees may differ;
in particular, sibling Spindel is not the pinned savepoint implementation.

## Four different things

| Thing | Meaning | What it does not imply |
| --- | --- | --- |
| Storage snapshot | Immutable reference to a declared substrate state | A running computation, budget or complete process image |
| Forked world | Isolated execution context plus its registered systems and declared components | A durable continuation, duplicated spending authority or copied native tasks |
| Engine continuation | The rest of a computation at an effect site, with its slice environment | A closure that can be serialized and executed in another process |
| Published savepoint | A continuation pending in a world, offered to a handler | An automatic snapshot of arbitrary surrounding mutable objects |

Yggdrasil supplies the substrate operations; Spindel adds execution state,
continuations, scopes and ownership; Dvergr supplies agent/Run semantics,
conversation persistence, admission, resource accounting and effect policy.
Simmis projects these into workflows, rooms, proposals and UI.

## State scope comes before snapshot identity

For a substrate with capture `C` and restore `R`, the essential law is:

```
observe(R(C(S))) = observe(S)       within the declared captured scope
```

The scope must explicitly say whether `S` means a logical committed tree or
the current workspace including uncommitted work, index/staging, file modes
and reference configuration. Both are useful, but they are different states.
A logical commit ID is not a complete workspace checkpoint merely because
the workspace is implemented inside an immutable database.

Yggdrasil's existing logical-history contract is committed snapshots: the
explicit commit operation advances that history, and its compliance tests
mutate, commit, then obtain the snapshot ID. Geschichte returning logical HEAD
is consistent with that contract. The potential defect is the stronger
savepoint layer treating that reference as a substitute for all state copied
by an ordinary workspace fork, not the logical commit identity itself.

The reference must remain stable and resolvable while retained. A branch name
is a mutable locator, not a historical state identifier. An identifier hashes
only what its representation includes: hashing an incomplete descriptor does
not make the missing state recoverable.

Yggdrasil's snapshot-first algebra provides immutable states, ancestry and
explicit branching/merging. That alone does not establish that every adapter
captures the same scope, that a sequence of reads across systems is one
consistent causal cut, or that sequential publication across stores is an
all-or-nothing transaction. Those are separate contracts to specify and test.

## In-process savepoints

A savepoint has a site, structural occurrence address, sequence, payload,
world and live continuation. With the default handler it immediately returns
its payload. A handler can hold it, resume it with a value, abandon it, or
fork it and resume the resulting child.

The continuation is affine per pending world: consume once by resume or
abandon. To explore several futures, fork while pending. To revisit a site,
retain a frozen fork as an anchor before continuing the original. Do not
rewind a live parent in place.

The continuation must execute with the child's dynamic execution context;
world state must not be captured as unrelated mutable atoms/host closures.
Shared components must genuinely be safe to share; pinned components retain
an immutable version; forked components require actual isolation support.

Forks do not duplicate live provider requests, timers, undrained events or
native futures. A portable/forkable boundary must either be quiescent for the
captured computation or explicitly represent pending work and its ownership.
The publishing computation being suspended is not proof that its whole world
is quiescent.

## Portable savepoints

A portable savepoint replaces the live continuation with a named resume
function and portable arguments. Its data contains the declared world-state
paths, substrate snapshot references, seed and pinned component versions.
Hydration creates a new world at those references, restores declared state,
rebinds components and starts the named function.

The existing Spindel portability law is:

```
hydrate(persist(sp), v) ~ resume(fork(sp), v)
```

This is observational equivalence within the declared scope, not equality of
process IDs, live handles or random sibling outcomes. The seed derivation
policy must be explicit. The named function must implement the same rest of
the computation as the live continuation; this is an embedder obligation.
Its compatible code and component versions must still be available. Resolving
the same function symbol after a code change does not by itself prove that.

The same scope rule applies to agent SCI definitions and sandbox state. An
in-process forkable SCI component is not automatically a portable one: its
definitions/state must be reconstructible from declared retained data or a
supported component capture. Fresh sandbox initialization alone does not prove
equivalence with a live sandbox that the agent has extended interactively.

Persisted data is not authority. The receiving host resolves trusted system
registrations, checks current admission/permissions and allocates or claims
resources. It must not interpret arbitrary supplied identifiers as permission
to read another repository, spend money or run effects.

## Dvergr Run continuation

The pinned LLM program publishes a turn savepoint after a completed model
step. Its handler records the latest portable descriptor on the original
durable Run and continues. Failure to record is logged and costs resumability,
not forward progress.

On explicit resume of a stopped Run, Dvergr reserves a successor claim,
creates a new Run/world at the recorded substrate snapshots, hydrates the
descriptor and continues the loop. The successor is causally linked to the
old Run, inherits its role, and receives permitted remaining resources through
the existing ledger. It does not resurrect the old process or reset spending.
Single-use CLI Runs refuse resume.

A complete turn checkpoint needs a matching conversation boundary as well as
workspace state. Messages arriving after capture cannot silently become part
of the old prefix. A continuation may intentionally incorporate them as new
inputs, but that must be a separate explicit operation/provenance record.

The content identity of a captured prefix is distinct from an occurrence ID
such as `(Run, step)`. Identical content can occur twice. Keep causal history
and captured-state identity separate. Latest-only recovery is also distinct
from a retained per-step timeline for branching, UI inspection or training.

## Money, irreversible effects and cleanup

State can be copied; spending authority cannot. Child grants attenuate a
parent's authority and move conserved resources. An unfunded child cannot
inherit the parent's wallet. Snapshot restore never rolls back charges for
inference or external work that actually happened. A funded portable handoff
needs a single-consumer transfer/escrow; the descriptor alone is not a wallet.

Restoring files cannot unsend an email, undo a payment, retract an arbitrary
HTTP mutation or cancel a provider request already accepted. Effects require
stable intent IDs and durable outcomes. An uncertain non-repeatable effect
holds for reconciliation rather than being automatically replayed. A receipt
of uncertainty must not be promoted into proof of success or failure.

Every live world, anchor and recovery handle has an owner. Cancellation is a
request, not proof of physical completion. Cleanup waits for actual native
acknowledgment before returning funds, releasing capacity or forgetting live
handles. A retained checkpoint must root the substrate blobs, component versions
and conversation prefix it needs until explicit release/retention expiry.

## Invariants and required evidence

| Invariant | Required discriminating test |
| --- | --- |
| Captured scope round-trips | Commit A, dirty-write B, capture, advance parent to C; restore must reproduce the declared B or explicitly declare/exclude dirty state |
| Historical isolation | Advance parent and siblings after capture; neither historical reads nor a frozen child may follow them |
| Continuation equivalence | Resume an in-process fork and hydrate its portable form under the same declared inputs; compare outputs, world state and effect intents |
| Coherent capture | Interleave writes during capture; reject/match a defined causal cut, never silently combine incompatible versions |
| Affine execution | Race resume/abandon/fork; consumed sites cannot execute twice; resource claims remain single-consumer |
| Conversation boundary | Append messages after capture; old-prefix hydration neither invents nor silently absorbs later messages |
| Capability honesty | Missing capture/fork/hydration support or versions refuse explicitly; never share a mutable system while claiming isolation |
| Resource conservation | Funded/unfunded fork, resume race and failed cleanup cannot duplicate budgets, refund live work or erase charges |
| Non-repeatable effect safety | Crash before/after send and before/after durable receipt; uncertainty holds without blind duplicate execution |
| Retention and ownership | Retain checkpoint, run GC, hydrate successfully; then release and demonstrate eligibility for collection without dropping live handles |

These are obligations for the layer that owns each boundary, not ten separate
new frameworks. Existing ownership, request claims, ledger and cleanup paths
should supply the implementation wherever possible.

## Current evidence and gaps

Source-confirmed in the pinned stack:

- A provider-free runtime regression also confirmed a separate hard isolation
  defect: historical Spindel/Geschichte fork checked out A through the parent's
  connection, changing its worktree/index from C to A. Disposal then refused to
  delete the checked-out logical branch. The original run had one test, nine
  assertions and two failures. The ordinary frozen fork and immutable physical
  DB controls preserved dirty B2 work, staged B and executable mode correctly.
  With the merged Yggdrasil/Spindel fixes, that unchanged regression passes
  all nine assertions, both with exact source overrides and with only public
  Git pins (fresh JVMs, exit 0). Yggdrasil's four targeted native adapter/helper
  namespaces also pass 81 tests and 606 assertions; Spindel's six targeted
  namespaces pass 103 tests and 660 assertions. These are not a full-portability
  claim. Simmis selects Spindel `2e39475` and Yggdrasil `dee7438`; Dvergr's
  application code remains `8b115db`.
- Geschichte's Yggdrasil `snapshot-id` is its logical HEAD commit. Spindel's
  portable descriptor records `{system-id -> snapshot-id}`, and Dvergr resume
  passes those as historical snapshot bases. Dirty worktree/index state is
  therefore not represented by that identifier. Exact stopped-Run content
  behavior still needs an executable regression; do not claim all edits are
  lost. Ordinary in-process Geschichte overlay forks copy physical workspace
  state, and canonical file operations may commit their changes.
- Run persistence retains the latest descriptor as EDN, not an immutable
  per-step history. Resume loads the persisted conversation without a captured
  per-step cursor in that load. A provider-free stopped-Run regression against
  Dvergr `8b115db` confirms that a message appended afterward enters the resumed
  model input despite an unchanged savepoint descriptor: one test, 19 assertions,
  with only the two proposed captured-prefix checks failing (exit 2). Run
  completion, causal linkage, one-shot resume and cleanup checks pass. This is
  not an OS-crash test. A reproducible captured-prefix policy and an explicitly
  refreshed/live-input policy need distinct semantics; the latter remains useful
  for agents reacting to room activity.
- The Run resume API explicitly leaves already-performed `:once` effects in
  the interrupted step for the caller to reconcile. It is not a guarantee of
  transparent exactly-once recovery of arbitrary external effects.
- Portable `persist` currently refuses asynchronous snapshot IDs. This is a
  declared limitation to revisit with the asynchronous substrate work, not
  permission to fabricate synchronous success in the browser.
- Spindel's savepoint design explicitly lists whole-world safe-point enforcement
  as open. Capability checks exist on some fork paths, but complete admission
  coverage needs focused unsupported-system tests.
- Source-pinned workflows explicitly install retained source and inputs inside
  their execution child. That is a narrower guarantee than complete historical
  recovery of every later arbitrary tool mutation.

Existing Spindel tests cover default identity, continuation values, independent
forks, affinity/races, seed behavior, declared-state portability, component pins
and escrow. However its direct fork-versus-hydrate equivalence test explicitly
uses `:systems :none`: it does not establish dirty Geschichte workspace fidelity.
Dvergr's Run-resume tests cover turn continuation, failed resume and charging;
the missing discriminator is actual workspace/index content across that boundary.
These tests were inspected here, not newly rerun as a foundation-suite proof.

Verification order: first provider-free adapter capture/restore and physical
fork controls; then one deterministic stopped/resumed Run with dirty files,
staging/modes and late messages; then effect fault injection, retention/GC and
cross-system capture. Record expected scope and outcomes before choosing any
new capture mechanism. Do not change logical commit identity or auto-commit a
user's workspace simply to make an incomplete test pass.

## Capture options to evaluate after the regression

1. **Committed-only portability.** Declare that portable savepoints require a
   clean/committed workspace and refuse incompatible dirty state. This is an
   honest restricted contract, not full equivalence with every live fork.
2. **Physical immutable workspace checkpoint.** Retain the underlying Datahike
   state needed to reconstruct worktree, index and logical history without
   moving the user's refs. Prefer reuse of the existing storage mechanism if
   its restore and GC contracts support this precisely. Do not hydrate old
   control-store permissions, resource ledgers or process handles as authority.
3. **Explicit workspace capture descriptor.** Retain logical basis plus worktree,
   index, modes and referenced blobs under a versioned, GC-rooted descriptor.
   More adapter-specific work, but the captured scope is explicit and can be
   independent of unrelated physical database state.

All three leave logical commit IDs unchanged. A dedicated workspace-capture
operation can implement option 2 or 3 without making `snapshot-id` mutate or
silently commit. Choose between those mechanisms only after testing what the
existing physical checkpoint/restore path actually retains.

## Existing foundations to reconcile

- Yggdrasil: `docs/CATEGORICAL_SEMANTICS.md`, `docs/CONSISTENCY.md`,
  `src/yggdrasil/protocols.cljc` and the pinned Geschichte adapter.
- Pinned Spindel: `docs/savepoints.md`, `docs/world-scopes.md`,
  `effects/savepoint.cljc`, `savepoint/portable.cljc` and their law tests.
- Dvergr: `doc/run-resume.md`, `doc/unified-worlds.md`, `agent/program.clj`,
  `agent/run.clj` and the resume/accounting tests. Older proposal/status sections
  must not be mistaken for fresh executable evidence.

This document should be split into the owning foundation repositories as the
tests establish their precise guarantees; it is not a replacement snapshot API.
