# Bounded recursive coordinators

`simmis_coordinator` is an explicit alternative to `simmis_task`, not a change
to ordinary file workers. It accepts the same task/model/finite limits and stable
request-id. It requires a work credential, current room membership and merge
authority, and an already provisioned native money/dispatch allowance. It never
funds itself or automatically merges into the room trunk. Existing credentials
retain their signed tool ceiling; mint a new work credential for the new command.
Identical retries check the original submission before resolving a model again;
changed submissions under the same request id are refused. Native startup
recovery includes coordinators: pointerless admissions become interrupted,
and terminal descendant wallets are reconciled without releasing held exposure.

The service uses the same work-request journal, Job, evaluation, Run and world
machinery as ordinary work. It offers only the Simmis completion verifier and
no world setups. Completion records execution, not task correctness; proposed
tests/checkers are not automatically promoted into trusted certification.

Inside SCI, `dvergr.agent/delegation-policy` lists the host's exact bounds.
Children may use only `#{:simmis/file-worker}` or `#{:simmis/coordinator}`,
the offered file/eval tools and accounted native providers. Combining profiles,
omitting a profile or requesting a broader tool/provider is refused before Run
admission. This policy and verifier scope follow recursive hires and experiment
cells. An experiment admits at most eight cells and parallelism two; these are
per-experiment ceilings, not a global CPU quota. Conserved grants bound aggregate
provider admissions and money across nesting.

The coordinator keeps the file worker's host registry, environment, mail and
room API restrictions. Only scoped composition functions are restored. World
effects still prohibit arbitrary hosts, processes, global writes and irreversible
publication. Ordinary workers remain unable to hire children. `agent/observe`
and `agent/cancel!` cannot target parent or sibling Runs from a child scope.

Results are available without an external orchestration script:

```clojure
(simmis.results/attempt-files attempt-id)
(simmis.results/attempt-file attempt-id {:path "/out/report.md" :offset 0 :length 16384})
```

These reuse the MCP/UI file projection over canonical immutable Attempt evidence,
including binary content/modes and base64 byte chunks. They do not copy file bytes
into another result store or reopen a discarded world. The reader accepts only
descendant Attempts certified by the explicitly offered completion verifier;
arbitrary verifier evidence and private fixtures remain inaccessible. Successful
reads record causal consumption on the parent Run. For other evaluators, the core
`agent/attempt-output` API reads only deliberately published `:agent/output` data.

This does not yet introduce pinned SCI AgentDef entrypoints, network research,
CLI recursive admissions, automatic checker promotion, process crash recovery or
deployment. The coordinator is still an LLM AgentDef whose Clojure tool programs
the shared harness; removing the need for that outer model is the next program
interpreter step. Capture transcription remains a separate room workflow.

## An inspected benchmark inside a coordinator

An optional `benchmark` on `simmis_coordinator` admits one existing owned
workflow as a fixed benchmark. Copy `expected-bundle` and
`expected-owned-source` from the same `simmis_workflow-check` inspection, and
supply finite candidate `limits`. Installation-operator status and room
read/merge authority are required for this custom checker admission.
Ordinary coordinators need no benchmark admission and keep completion-only
child experiments.

The coordinator's outer grant covers its own model calls and every child.
Candidate limits are an aggregate ceiling for each invocation; they do not
provision funds. This first interface admits one candidate and parallelism one
per invocation. Compare prompt/model revisions by invoking it repeatedly within
the remaining parent balance. One physical gate is shared by the entire
coordinator family, including recursively hired coordinators; a concurrent
invocation fails as busy before resource allocation rather than multiplying
checker workers. The global checker permit and fresh fixture remain owned
until the outer native Job physically settles.

Inside the coordinator's SCI interpreter:

```clojure
(simmis.benchmarks/offered)

(let [team (dvergr.agent/make-agent
            (dvergr.agent/roster)
            {:id :candidate
             :prompt "My candidate instructions."
             :tools #{:clojure_eval :read_file :write_file :edit_file}
             :sandbox #{:simmis/file-worker}
             :model-policy {:provider :codex-subscription
                            :model "codex-subscription-luna-6"}
             :program {:kind :llm :max-model-steps 4 :auto-compact? false}})]
  @(simmis.benchmarks/run!
    team {:resources {"microUSD" 20000M "model-dispatch" 4M}}))
```

Use an installed, admitted provider/model and a grant sufficient for the fixed
case set, within the benchmark's admitted ceiling and the current parent
balance. The result is the ordinary native result Spin containing its Scorecard.
Its entries use `:attempt/id`; `simmis.results/attempt-files` and
`simmis.results/attempt-file` read only captured `/out` files for descendants
certified by the exact admitted verifier. They never expose checker gold,
calibration or private setup evidence. Reads record causal consumption.

The host pins source bytes, source revision and target, selected dataset,
evaluator/setup references and policy in the original work request. Candidates
can change prompts/model choices; they cannot replace cases, tasks, checker,
setup, world parent or authority. Agent-authored checkers still need a separate
inspection and operator admission. This prevents an author from certifying its
own changed rubric merely by constructing another EnvironmentDef.

Candidate worlds fork a fresh fixture repository, rather than the author's
world or Git history. Run/Attempt records and all wallets remain in the original
control room; the returned Spin belongs to the coordinator's current context.
No second execution engine, billing ledger or automatic merge is introduced.
This interface currently accepts copied-fixture benchmarks; checkpoint-origin
benchmarks continue to require their separate native admission path.

An exact request retry observes the original Job before resolving current source
or model state. Restart recovery can retire only the original recorded fixture;
missing or uncertain ownership remains held. A failed physical gate release
keeps the child Job, result and parent acknowledgement pending until explicit
native recovery succeeds. Completion is still distinct from task correctness,
and benchmark results do not adopt proposed changes.
