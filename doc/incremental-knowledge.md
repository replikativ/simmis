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

Freeze bounded batches and context independently; claim one batch per stream
across manual and scheduled invocations; reuse existing work-request admission;
and compose exact source-processing receipts with certified knowledge filing.
Pending proposals must hold the next batch. Benchmarks use explicit frozen cases
and must never claim or advance a live stream. No default schedule is enabled by
this foundation.
