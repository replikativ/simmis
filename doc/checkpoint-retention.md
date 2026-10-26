# Request checkpoint retention

The internal HOST API in `ops/work_request_checkpoints.clj` retains an exact
physical source checkpoint independently of a knowledge discovery cursor. It
is not exposed through MCP, SCI or the browser, and ordinary workflow admission
does not invoke it yet. It creates no scheduler, execution engine or wallet.

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
