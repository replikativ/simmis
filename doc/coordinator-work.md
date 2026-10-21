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
interpreter step. Transcription remains a separate, deferred agenda item.
