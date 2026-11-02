# Evidence-check starter

This ordinary workflow classifies a claim from a supplied note. Its optional
benchmark pack illustrates independent fixtures, evaluator-held answers,
positive/negative calibration and a declared held-out split. The synthetic
cases do not establish general research or business performance.

Save all files under `workflows/<your-name>/`. Inspect your existing files
before saving. Run `(workflow/check-files files)` on the relative file/text
map: this inspects syntax and structure, never runs the checker or certifies
that source was saved. An operator must admit the exact saved checker revision
before benchmarking. `(workflow/action-links "<your-name>")` provides scoped
Run once, Schedule and Benchmark confirmation links; it performs none of them.

The same task may run normally without invoking its checker. To adapt this
example, replace task, fixtures, gold and calibration together; keep the solver
program independent of its evaluator. Do not place gold in candidate inputs.
A fork of a room containing answers is not automatically a blind environment.
Use the host's clean benchmark fixtures and verify absence of private history.

Calibration proves this checker distinguishes these particular reference and
damaged outputs. It does not establish independent truth. Obtain source-backed
examples and human review for a real task; preserve valid alternative answers.
Tune on development, freeze the rubric, then inspect held-out results. Do not
claim a benchmark ran merely because files were checked or a smoke test passed.

Benchmark outputs can be inspected through retained Attempts. Publish the
standard Markdown report when appropriate: `reports/index.md` links to
`reports/benchmarks/<scorecard>.md`, whose tables query retained results.
Workflow code, evaluation packs and proposed file changes are versioned source;
execution allowances, admissions and certified results remain typed DB records.
