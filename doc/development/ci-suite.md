# CI suite measurements

CircleCI is the primary provider. Backend shards run in isolated processes:
fixtures mutate process-global registries, runtime Vars and host configuration.
Do not replace this isolation with concurrent namespace execution in one JVM.

The backend job enables the pinned Kaocha profiling plugin. Its timing summary
and `target/ci-timings/node-N.edn` contain per-test and per-namespace wall times.
`node-N-progress.edn` is flushed after each namespace, preserving completed
measurements if a later test hangs or the container is terminated.

A namespace's `outside-children-ns` is its elapsed time minus its timed children.
This includes namespace `:once` fixtures and runner/reporting overhead. Individual
test durations include `:each` fixtures and any fixtures called inside the test;
they must not be described as assertion-only work. Nested durations overlap:
**sum namespace durations once**, rather than adding test and namespace times.

The artifacts also record CPU time, heap, GC counters/time, thread count, load
average and available processors at startup, after namespace loading and after
each namespace. CPU and GC deltas can help distinguish allocation/compute cost
from waiting; heap samples are snapshots, not a retained-object analysis.
No explicit GC, heap dumps or sampling threads are introduced by this plugin.

Initial existing baseline: CircleCI job 1419 took 53m25, with backend execution
shards of 52m30, 28m46, 33m58 and 35m02. Setup was roughly one minute. These
numbers predate detailed instrumentation and do not establish the source of
Circle's slower execution compared with GitHub. Namespace count currently
controls partitioning, which explains part of the critical-path imbalance.

The cleanup acceptance target is approximately 20 minutes of aggregate test
execution work across backend shards, frontend, worker and Python jobs, and no
more than 10 minutes for a backend shard. Report each component, their sum, and
the longest shard separately; do not treat parallel wall time as serial cost.

## Required checks

Circle's `test`, `helpers`, `frontend` and `worker-package` jobs are the routine
PR gates. `worker-package` builds and cold-starts the credential-free worker jar;
the ordinary helper suite skips these checks without the explicit jar setting.
Main's uberjar build waits for all four gates. No job deploys the application.

The duplicated GitHub Actions CI workflow is retired. When landing this change,
replace legacy required checks such as `test-and-build` with the four Circle
contexts and update any merge watcher that names the old checks. Do not create
an unconditional-success compatibility job or bypass the primary checks.

## Fixture boundaries

`is.simm.fixtures.accounting/with-schema-only` is a deliberately synthetic
business-book setup for document and Git service tests. It installs the real
mandatory Kontor schema and fresh connection governors, while omitting the
business-accounting starter book and category-S ledger projections. It does
not replace the native conserved-resource kernel, wallets, Room creation,
repository ownership, Git transport protocol or operation under test. The
substitution ends when synchronous fixture construction returns.

Document projection, chart, page-table, typed-property and workflow-source
cases use this smaller setup where no business posting is exercised. Git
sync/push/policy cases also retain fresh native/control owners. The positive
push case explicitly opts into full accounting, while canonical provisioning,
profile validation, business-accounting, resource-kernel and reflected-morphism
tests retain the complete production installers. Synthetic tests do not claim
to establish the complete accounting defaults or ledger category projection.

Repeated per-feature source checks for cold RPC registration are consolidated
into the central boot/registry contract. It checks both directions, so a
handler loaded by another test cannot conceal its absence from production
startup. Actual inbound authority, captured-owner and wire-error tests remain.

The actual inbound chart, page-table, block-reference and backlink endpoints
share one read-only authority fixture. Each endpoint still exercises its real
wire dispatch, denial matrix, result and queued closure. The owner is fenced
only after all four positive paths; every old closure must refuse without
calling its adapter under a replacement App. Endpoint error sanitization tests
remain separate and cheap.

Focused probes can set `SIMMIS_CI_EXPECTED_TESTS` to an EDN file of exact
qualified test keywords. The metrics plugin writes the resolved selection and
refuses a mismatch before executing tests. Use `:kaocha.filter/focus`, not the
unrecognized top-level `:focus` shorthand. Run local native checks under a
bounded transient systemd unit with `MemoryMax`, `MemorySwapMax=0` and
`KillMode=control-group`; verify `MainPID=0` after stopping it. Shell interruption
alone is not evidence that a Java child has physically exited.
