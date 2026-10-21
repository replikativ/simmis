# Wiki maintenance as an ordinary workflow

The version-1 starter is source data returned by
`is.simm.starter.wiki-maintenance/source`. It contains the files for
`/workflows/maintain-wiki/`, not a second agent, scheduler or resource ledger.
Loading it does not install anything, choose a model, allocate money or enable
a schedule. Installation must use the existing authorized owned-workflow writer
and preserve user edits. Automatic installation is a separate integration step.

```text
inbox/ + transcripts/ ── evidence ─┐
wiki/ ──────────────── context ────┴─> frozen manifest + byte copies
                                        │
                 ┌──────────────────────┼──────────────────────┐
                 │                      │                      │
             manual Run          scheduled occurrence    benchmark attempt
                 │                      │                      │
                 └────────── ordinary isolated Run ────────────┘
                                        │
                              /out/wiki/*.md
                                        │
                   retained output / proposed change ≠ adopted wiki
```

The existing knowledge collector selects Markdown in the declared folders.
Wiki context affects execution identity, but not the trigger identity: adopting
a wiki update must not by itself retrigger maintenance. Empty/unchanged scheduled
inputs can be skipped by existing admission before submitting a funded Run.
Chat messages are not collected directly; they need an explicit evidence-file
ingestion step. An overview is ordinary Markdown, not a new schema entity.

The starter asks for small source-backed notes and useful overviews, conservative
updates to existing documents, stable identities/properties, explicit uncertainty
and attributed contradictions. Only complete proposed Markdown pages belong in
`/out/wiki/`; the host retains them for the existing file-output proposal path.
No source deletion, schema migration, whole-world merge or automatic adoption is
enabled by the starter. A completed Run is not a claim of factual correctness.

## Benchmarks are explicit

`checker.clj` is a benchmark-only mechanical regression checker. Ordinary manual
and scheduled saved-workflow execution uses host completion and exact capture;
it does **not** execute the authored checker. Do not label every scheduled
proposal quality-checked on the strength of this file.

Evaluator-held gold defines expected output paths, required/preserved/forbidden
fragments, frozen source text, citations and quotations. Tests exercise actual
SCI execution and calibration with damaged answers. These checks do not prove
semantic truth, full Markdown isomorphism, arbitrary quote fidelity or resistance
to all prompt injections. The fixture tests are not model-performance results.

Synthetic evidence, gold and calibration are kept out of the installed starter.
The instruction-only fixture is `.txt`, not a wiki document: installing an
unidentified Markdown prompt into a canonical repository would invalidate its
knowledge projection. `/workflows/` is not globally hidden from that projection.
A benchmark derives a separate bundle by adding verified frozen input fixtures
and evaluator-held gold/calibration. Its complete bundle identity therefore
differs from the live starter; the task and checker bytes remain the same. Never
compare these as if they were one identical full bundle or seed synthetic
customer facts into a real scheduled Run.

## Remaining delivery gates

1. Validate prospective output against canonical Markdown/property rules, then
   integrate guarded file-only adoption and recovery receipts. Preserve the
   distinction between processed input, filed proposal and adopted result.
2. Add idempotent inactive installation for explicitly eligible personal
   workspaces. Do not overwrite a customized bundle or convert legacy stores.
3. Make configuration of an admitted model and finite allowance explicit before
   enabling its ordinary schedule. Expose status, proposals and failures in UI.
4. Benchmark actual models on retained cases before making quality claims or
   recommending defaults; expand checks and examples from observed failures.

Before default activation, bind an explicit execution-world source-memory
selection profile that distinguishes owned frozen-input/output staging from
canonical knowledge. Otherwise preserved IDs in staged copies and replacements
can collide with the inherited wiki. Ordinary room selection must remain
unchanged; retained evidence remains accessible as files, and `/out` pages do
not become canonical knowledge merely because a Run wrote them.
