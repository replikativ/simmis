# Request checkpoint retention

The internal HOST API in `ops/work_request_checkpoints.clj` retains an exact
physical source checkpoint independently of a knowledge discovery cursor. The
fixed native retained-batch workflow admission uses it for the explicit policy
below. The native root API is not exposed through MCP, SCI or the browser. It
creates no scheduler, execution engine or wallet.

## Explicit execution policy

Pinned workflow source opts in through `execution-origin.edn`:

```edn
{:version 1
 :workspace :retained-batch-checkpoint
 :retention :request-evidence-v1}
```

This closed policy cannot name a commit or store. HOST admission derives those
from the canonical retained batch proof. The existing `input-batching.edn`
must explicitly select `:context-paths []`, `:max-context-files 0` and
`:max-context-bytes 0`; trigger file/byte limits remain unchanged. Selected
trigger evidence is still copied and verified. Additional context is available
through ordinary virtual file operations at the exact initial repository
checkpoint, instead of copying every wiki body into the inputs.

This policy requires explicit `discard` settlement for the execution world.
Generated `/out/wiki` files remain retained and reviewable as ordinary artifact
Proposals with publication-basis conflict checks. A whole historical Run-world
merge could also include inherited C1 dirty/staged changes; it is not the
publication route for this policy. Incompatible requests and schedule
deployments are refused before discovery or pinning, without rewriting intent.

The policy selects the whole initial chosen repository, not a per-path read
restriction or an actual-read receipt. It does not freeze attached repositories
or external inputs. Batch processing still acknowledges only its selected
trigger prefix. The shipped v4 starter retains copied-context behavior. Workers
and benchmark entry points explicitly reject the new policy until they support
the corresponding checkpoint/clean-fixture admission.

## Identity and ownership

A canonical Request intent can contain `:knowledge/checkpoint`, a closed
version-one selection with a physical Datahike origin, complete published
source basis, original HOST target, and `:request-evidence-v1` retention policy.
The origin's commit/store/repository must equal the basis; its target must also
equal the canonical publication target. A batch-backed Request additionally
requires both typed batch markers and the canonical retained batch proof, with
the same source basis and original target. Missing linkage holds before pin IO.

After the Request has its Job pointer, `model.work-request-checkpoints/prepare!`
publishes a bounded immutable binding and inserts the retention relation in the
control writer's serialized transaction. Only the contender whose fresh root
UUID and creation token were stored receives `:new? true`. A Request claim by
itself does not make a later caller a creation winner. Ambiguous control writes
are read back using that attempt's fresh token.

These return values are inert HOST coordination data, not capabilities. The
owning HOST must retain its original physical source extent and current
authorization, invoke native creation once, and recheck authority before any
prediction or Run admission. No native handles are stored in Request data.

The synchronous Job handoff holds the existing physical Room-operation lease
through source resolution, intent publication, pin IO and readback. It rechecks
the original App, Room, control mapping, target and physical configuration after
IO, then releases that source lease before forking. The setup reference carries
the checkpoint through every wrapper. Preparation requires the actual Run Room's
native fork descriptor to name the exact checkpoint on the selected overlay;
matching repository/store identity alone is insufficient.

## Native acknowledgement

`ops.checkpoint-roots` shares physical pin checks with the existing discovery
cursor implementation. It uses the released Datahike GC-root API. A new pin
requires both its registry entry and raw record key to be absent: explicit
Datahike root IDs overwrite, so creation must never be replayed blindly.

Verification checks the original physical configuration, exact published basis,
registry owner/commit/permanent TTL and complete immutable commit record, with
Datahike parent links removed as required by pin semantics. Missing, partial or
mismatched publication holds. Recovery inspects; it never substitutes the current
Memory, recreates a missing root, schedules another Job or resumes execution.
`acknowledge!` verifies the physical result before marking the relation retained.

The root remains after Job settlement and after the discovery stream advances.
There is no automatic expiry or release API for these Request roots. Execution
cleanup acknowledgement alone does not end retained-evidence obligations.

## GC limits

A Datahike pin retains the checkpoint's datoms and referenced blobs. It does
not independently retain every older snapshot that Geschichte can reach through
logical history UUIDs. The existing Geschichte adapter's epoch-only GC contract
still applies; this API does not strengthen distributed GC or make arbitrary
history pruning safe. GC remains disabled by default in Simmis.

The native regression uses a disposable file repository without logical commits,
removes the old content from live state, and verifies that real GC collects an
unreachable control blob while the Request pin retains C1 and its content.
Other tests exercise concurrent contenders, lost acknowledgements, partial
registry/record publication, target mismatch, missing batch markers and Job
settlement. They perform no inference or operations on user stores.

Native workflow tests additionally change the parent from C1 to C2 before
execution, read C1 from a deterministic SCI candidate, and retain context beyond
the old whole-context copy limit without copying it. Fault tests admit no
resources/candidate after lost pin ACK, revoked authority, retarget or App fence.
A real Room-close test swallows interruption inside pin IO and verifies teardown
waits for physical completion and Job cleanup before closing the context.
