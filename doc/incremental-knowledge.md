# Incremental knowledge discovery

This implementation is an internal, inactive foundation. Its schema and HOST
operations are not registered in the application. Existing wiki maintenance
continues to use its existing frozen whole-selection inputs. Discovery performs
no model dispatch, checker evaluation, source processing, or wiki publication.

## Source and stream identity

A stream belongs to one authenticated human, canonical room, explicitly owned
Memory target, normalized folder policy, semantic workflow program, and revisit
epoch. The program identity includes the rendered task, effective default
parameters, bundle, and auxiliary inputs. Model selection and schedule cadence
do not reset processing. The first HOST entry accepts default workflow parameters
only; cases and parameter/input overrides require retained invocation support
before they can be admitted.

Discovery reads published immutable Datahike checkpoints. It verifies the
checkpoint against the actual data: `d/with` can preserve a commit ID while
changing its contents. Both snapshots are bounded before structural index diff;
missing ancestry, reset sources, or unsupported index layouts hold the stream.
Renames appear as removal plus addition. Ordering is lexical path order at
discovery, not a claim about event chronology. Trigger metadata is enqueued;
context changes do not themselves certify processing.

## Retention protocol

1. Persist a bounded intent containing the original source locator and basis.
2. Only its original creation winner may create the permanent, unique native
   GC root. Recovery inspects; it never recreates an absent or partial root.
3. Verify the root registry, owner, and entire immutable commit record.
4. Atomically publish the discovery journal, pending item changes, and cursor.
5. Verify that joint receipt and its replacement root before releasing the
   predecessor. Record cleanup completion after observing its absence.

The intent moves through `creating -> pinned -> committed -> retained`; an older
retained intent becomes `released`. An active batch or unfinished cleanup blocks
another discovery. Discovery never marks an item processed.

Lost acknowledgements retain an inspectable intent. After a lost native release
acknowledgement, the old commit may already be garbage collected; absence from
the root registry permits cleanup completion without restoring that old commit.
A missing root before discovery publication remains held. Automatic abandonment
and stream retirement are not implemented.

## Ownership and authorization

`owned-knowledge-discovery/refresh!` uses the original application operation and
actual room lease, with a shared exclusive gate for refresh and recovery by that
human in that room. The operation physically joins native work before returning.
Writer fragments observe captured lifetime liveness and recheck current human
authority and default mapping using the writer's own control database. They do
not hydrate rooms, read source artifacts, resolve workspaces, or run providers.

Committed cleanup belongs to the original captured source. Explicit recovery
resolves its canonical registered system, independently of a new default Memory.
A logical Geschichte checkout does not redirect cleanup; the restored pinned
checkpoint must still match its full original basis. A retired or ambiguous
original system holds recovery instead of falling back to another workspace.

These are frozen-snapshot operations with current control checks. They do not
provide atomic transactions across source and control stores, serialize arbitrary
root writers, or establish distributed GC safety. Native root IDs are exclusively
owned and never reused. The test registry fixture substitutes runtime provisioning;
it does not prove deployed filesystem provisioning.

## Remaining integration

The inactive batch input planner now selects deterministic pending-order prefixes
without the canonical view's 200-document body limit. Context is either the whole
declared context selection or explicit named paths, with independent count/byte
bounds; exceeding them holds rather than truncating. Ordinary workflow fixtures
reserve both file slots and UTF-8 bytes. An oversized later source preserves the
valid prefix; an oversized head holds for an explicit decision.

A version-2 HOST envelope binds stream/cursor/item identity, context, limits and
remaining count to the existing version-1 exact frozen-evidence format. That
format already supports explicit subsets; it is not a live progress cursor.
Envelope hashes and byte verification confer no authority to claim a live stream.
The source cursor pin must remain retained until the HOST has durably frozen the
inputs, and while the active batch prevents discovery from advancing it.

The internal HOST preparation requires an explicit `input-batching.edn` sidecar
containing `{:version 2 :epoch #uuid "..." :policy {...}}`. It validates opt-in
before discovery effects, claims before copying source bodies, and verifies the
durably retained artifact before returning resource-independent data. It freezes
complete wiki publication occupancy from that same pinned snapshot, including
excluded and non-Markdown targets. Deterministic occupancy bounds are checked
before claiming. Active retries revalidate bounded workflow/fixture source but
never recapture evidence/context; unknown claim/freeze acknowledgements hold.
This preparation does not submit a request or grant dispatch ownership.

Source-backed output proposals require retained admission-time BEFORE bytes for
existing wiki targets. A bounded context therefore limits edits to existing
targets included in that context; omitted existing targets cannot be treated as
absent. The separate publication basis still inventories all wiki occupancy.

The inactive batch journal provides `capturing -> ready -> submitted` fragments.
Claiming compares the complete current stream, retained cursor-pin receipt, exact
canonical pending prefix and remaining count, then sets the active hold and item
claims together. Freeze attaches the HOST-verified artifact. First request linkage
must compose with canonical request insertion; it refuses a preexisting partial
request. Historical replay requires the actual matching canonical request and
cannot manufacture a replacement if that request is missing. These fragments are
not registered for default use and do not mark any source processed.

The inactive owned-workflow adapter recognizes the explicit batching sidecar.
It rejects case, parameter, and input overrides before discovery effects. It
retains the original App operation, uses short original-Room extents for source
reads and target checks, and releases each source lease before provider
preparation or execution. Preparation consumes the frozen evidence and complete
publication footprint read back from the verified artifact.

Admission reuses the ordinary native Job lifecycle. One guarded serialized
transaction inserts both the canonical request and the exact batch linkage.
Only its acknowledged original claim winner may start a Job; a lost
acknowledgement requires matching canonical receipts on both sides. The actual
submission must equal the retained intent, including its request ID and limits.
Original App/Room liveness, current human authority, and default mapping are
checked again at the writer and native handoff. App/Room owner handles are
removed before retaining execution and settlement closures.

Exact workflow retries verify retained intent integrity. Batch retries also
verify frozen bytes, typed request markers, and both sides of the original
linkage without opening current source files or preparing providers. A provider
preparation failure leaves a ready batch held for explicit recovery; it does
not silently recapture or dispatch a replacement. An interrupted post-claim
handoff leaves an interrupted request with its exact batch linkage intact.
Successful Job completion alone does not mark sources processed.

Certified filing now composes the existing hashed disposition and optional
retained-file Proposal with the exact batch transition. The disposition includes
the batch identity, and the writer compares the original verified batch's
immutable row, including artifact references and claim/owner/request fields.
The complete selected-item membership is checked through a bounded batch index;
context and later pending items cannot be processed accidentally.

A certified no-op marks precisely those selected items processed, closes the
batch, and clears only its own active pointer. A proposed update keeps the batch
in review and its selected items claimed. Historical and lost-acknowledgement
replays require the whole joint receipt; partial states are refused rather than
repaired. An exact closed no-op replay leaves a later active batch untouched.

A deterministic, model-free runtime test exercises actual owned admission,
world fork/setup, evaluation, durable Attempt certification, and no-op filing
with discard settlement. Provider preparation and registry-to-memory provisioning
remain fixture seams; this does not prove a provisioned deployment, scheduled backlog drain, or native
file adoption of a proposed change.

Freeze bounded batches and context independently; claim one batch per stream
across manual and scheduled invocations; reuse existing work-request admission;
and finish proposal-resolution integration with the existing positive native
file-adoption and guarded dismissal transactions. Pending proposals hold the next batch. Benchmarks use explicit frozen cases
and must never claim or advance a live stream. No default schedule is enabled by
this foundation.
