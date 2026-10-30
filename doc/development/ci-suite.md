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
