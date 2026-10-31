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
Circle's slower execution compared with GitHub. Namespace count controlled
partitioning before this cleanup, explaining part of the critical-path imbalance.

The cleanup acceptance target is approximately 20 minutes of aggregate test
execution work across backend shards, frontend, worker and Python jobs, and no
more than 10 minutes for a backend shard. Report each component, their sum, and
the longest shard separately; do not treat parallel wall time as serial cost.

The first complete verification after cleanup, [CircleCI 1440](https://circleci.com/gh/replikativ/simmis/1440)
at `3b6b3976`, passed 2,888 tests / 39,714 assertions across 441 namespaces.
Backend command work was 112m52 aggregate (29m43 / 28m19 / 25m26 / 29m24),
including about 5.9 minutes of process/loading overhead. This is an 18.2%
aggregate reduction from the instrumented 138m01 baseline; the longest shard
fell from 47m55 to 29m43. The frontend job separately took 7m20. Worker packaging,
Python and job setup remain additional costs. **The 20-minute aggregate and
10-minute shard targets are not achieved.** Duration history uses these measured
post-cleanup namespace costs for subsequent partitioning.

The same run's scalar snapshots ended with 84–157 Datahike registry entries and
218–367 named memory stores per process. These are retention audit leads, not
proof that every entry owns a live runtime. GC time remained roughly 1.2–2.5%
of elapsed execution; low GC overhead does not discharge acquisition/owner
cleanup obligations. Compare per-namespace registry deltas before changing
fixture ownership or deleting known stores.

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

The two typed-property flows and four scheduled-occurrence flows use one adverse
attachment-first order instead of repeating the whole scenario in both orders.
Both orders remain explicit in the central owned-workspace, file-projection
and transport contracts. Historical versus live inputs, both K1/K2 occurrences,
actual adapter execution, races and file-once checks remain in the service tests.

Kaocha's suite `:elapsed-ns` excludes namespace loading and JVM startup. For the
aggregate acceptance target, use backend command wall times (and report startup
and loading separately using the resource uptime snapshots). Do not claim that
summing only leaf tests or completed namespace durations measures the complete
backend command. Namespace durations are appropriate for partition weights,
since each isolated shard pays its own startup/loading cost.

Resource snapshots also count already-loaded named memory stores and Datahike
connections. A named memory store intentionally survives connection release;
its fixture owner must explicitly delete it after safe physical retirement.
An increasing store count is retention evidence, while GC time and heap trends
help assess its performance impact. Never clear a global registry to make a
measurement look clean or delete stores owned by unresolved native work.

The shared work fixture owns the chat connection it creates. A successful Room
close acknowledges native drain/context retirement before the fixture removes
governance, releases that connection and deletes its named memory store. A
failed close preserves that chat acquisition/store for explicit recovery.
This is a chat-owner guarantee; older outer fixtures' control/repository
teardown ordering needs separate auditing before claiming whole-host retention.

Owned-workspace, MCP, owned-read and file-projection tests can explicitly select
`:system-schema :core` in their fixture. This installs the real 146 core Simmis
control attributes instead of all 517 feature attributes; every existing Dvergr
system/actor/assignment/pricing/task group, full chat schema and real resource
kernel stays. Unrelated fixture descendants and authority/provisioning defaults
retain the full schema. New feature-journal tests must install their actual
schemas or choose the full fixture, rather than accept an earlier storage error
as a successful authority refusal.

Seven workflow/benchmark/schedule/worker admission tests also remove only the
registration-order cross product: 24 native fixture executions become 12.
Both native/CLI/worker kinds and actual execution, capture, provenance and
physical-capacity retirement assertions remain; their shared selection contracts
continue to exercise both orders.

Control-only admission cases now use fresh real control databases, the actual
core/actor/request schemas, real EACL facts, and a genuine opened/fenced/joined
App lifetime. Their opaque execution token cannot run native effects: these
cases deliberately stop before hydration, source reads, provider preparation,
Jobs or resources. A positive production owner preflight guards against
vacuous negative results. Enrollment/revocation tests additionally install the
actual worker schema. Full public native execution/lifetime cases remain.

Seven feature-level admission registration-order repetitions use the adverse
attachment-first order; central owner/transport contracts retain both orders.
No lifetime, authority, retry, interruption or forged-helper assertions were
removed by these reductions.

The profiling plugin ignores nested `api/test-plan` calls made from an executing
test. They are introspection, not another suite; resetting the artifact there
would truncate the parent progress journal. Final measurements and exit counts
remain the original runner's values.

Read-only filing observers share one genuine certified failed Attempt and one
persisted successful Attempt. The malformed returned variants still go through
`make-attempt-receipt`, `make-attempt` and `validate-attempt`; reader counters
prove each intended seam is reached, and original DBs/baselines are checked
before and after. This removes 24 repeated two-store setups. Missing durable
Attempts, failed filing, historical/unbound/local-worker environments and actual
publication remain independent integration cases.

Nine file-output tests consolidate 31 fixture acquisitions into 10. Failed
observations share a genuine filing/claim only when they do not advance a state
machine. Scalar proof fields and the noncomponent journal link are restored
exactly and revalidated. Shared component ownership is corrupted last and never
rolled back by deleting a relation. Unknown acknowledgments, native dispatch,
terminal journal transitions, interruption and physical ownership keep their
own scenarios; no terminal transition is reversed for fixture reuse.

Further integration sharing removes 14 setup acquisitions: installer races
restore exact scalar owner/default/profile values and positively inspect the
original baseline between refusals; source customization is last. Direct
`install!` still proves customized installed bundles cannot dispatch writes.
Saved-view missing/positive reads recompute source request descriptors. Terminal
notification outcomes each retain their own real request/client/job; only their
immutable Scorecard and host are shared. Git's unchanged closure precedes a
new independent incoming operation, and every automatic changed update checks
identity of its planning/native budget. Irreversible ACK/lifetime cases remain.
