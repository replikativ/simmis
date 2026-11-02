# ContractNLI workflow and grounded evaluation pilot

This is an ordinary file-based task: classify one contract/hypothesis and cite
source spans. Its optional benchmark pack supplies four frozen, source-backed
examples. The optimized task can run or be scheduled without running its
checker. Nothing in this folder launches a model, installs a host evaluator,
adds funding, schedules work or adopts proposed changes.

## Source and permitted use

Data: **ContractNLI**, Yuta Koreeda and Christopher D. Manning (2021),
[paper](https://aclanthology.org/2021.findings-emnlp.164/),
[dataset](https://stanfordnlp.github.io/contract-nli/).
The upstream dataset is published under CC BY 4.0. `LICENSE.data` and
`TERMS.data` retain its supplied license and terms; `UPSTREAM-README.md` retains
its specification. These notices apply to the derived case data, independently
of the surrounding project's code license. The selected contract text and span
annotations are preserved; this pack reshapes them into fixtures and evaluator
references and adds task/scorer/calibration code. Do not imply upstream
endorsement. Original document URLs are recorded, not fetched by this workflow.

`source-provenance.edn` pins upstream commit, archive URL/size/SHA256,
development JSON SHA256, selected document/hypothesis IDs and per-file hashes.
The archive was bounded during acquisition and removed after extracting the
small development JSON and notices. Source archive:

`https://raw.githubusercontent.com/stanfordnlp/contract-nli/eced6528dd3c1d14d73f9a87df8f7bdbc03126f9/resources/contract-nli.zip`

The fixed hypothesis is **nda-7**, third-party disclosure. The earlier room
prototype used nda-19 (survival of obligations); that development hypothesis has
no Contradiction examples, so it cannot supply a balanced three-label pilot.

| Local split | Case | Upstream document | Upstream split |
|---|---|---:|---|
| development | dev-71-nda-7 | 71 | dev |
| development | dev-482-nda-7 | 482 | dev |
| development | dev-456-nda-7 | 456 | dev |
| held-out | holdout-15-nda-7 | 15 | dev |

Development deliberately contains one case of each label. The local held-out
case uses a different document and was selected before candidate runs. It is
**not the official ContractNLI test split**, is not class-balanced, and does not
establish that a public document was absent from model training. Four selected
cases exercise the machinery; they do not support population performance claims.
All cases are public data, not a customer's confidential legal documents.

## Inputs, outputs and isolation

Each candidate receives `/inputs/case.edn`: full contract text, one hypothesis,
case/document IDs and the public inventory of sentence/list-item spans. It does
not receive annotation labels or the annotated evidence selection. Source
positions use Unicode codepoint offsets, as in the Python dataset representation.
Span texts are materialized from those offsets during preparation, so Clojure
code must not reinterpret them as UTF-16 `subs` offsets. Candidate and checker
use explicit IDs/text, not guessed character conversions.

Output is exactly one EDN map in `/out/answer.edn`:

```clojure
{:choice "Entailment"
 :evidence [{:span-id 10 :quote "The exact complete span text from this case."}]}
```

The example shape is not a reference answer for every case. Use `NotMentioned`
with empty evidence when the contract does not establish or contradict the
hypothesis. `/out/review.md` explains the decision, citations and uncertainty.
`review-rubric.md` is a human review guide, not an automatically enforced judge.

Run the benchmark through Simmis's **clean fixture path**. Merely forking a room
that contains the whole dataset, gold or previous conversations does not hide
answers. Do not copy `gold.edn`, calibration answers, source annotations or the
coordinator's history into candidate inputs. Do not claim a blind evaluation
until retained input manifests and history isolation have been checked.

## What the deterministic score means

The checker reports separate boolean checks for output shape, three-way label
agreement, valid source quotations, annotation overlap, annotation completeness,
and whether selected evidence stays within the published annotation set.
Duplicate IDs, unknown IDs, mismatched quotes, malformed outputs and extra EDN
forms are not silently accepted. A correct label receives 0.5 reward, valid
citations another 0.25, and evidence span-ID F1 contributes up to 0.25. A wrong
or invalid label receives zero. Partial evidence earns partial reward; the
boolean dimensions explain that score. Missing/invalid model output is scored
as invalid output; infrastructure failure remains a Run/Attempt fault and must
not be relabeled as a zero-quality answer.

Evidence F1 is the original dataset's sentence/list-item annotation comparison,
not a general proof of legal entailment or that the selected spans are the only
valid support. A full sentence ID must carry its exact supplied text, but the
checker does not require one fixed evidence vector/order. Experts may identify
valid alternative evidence or challenge reference annotations. The free-text
rationale is retained for inspection but is not semantically graded. This pilot
is neither legal advice nor a certification for legal practice.

`calibration.edn` uses the upstream reference selection and deliberately wrong
labels, invented quotations, omitted evidence and missing output. It shows that
this checker catches those faults, not that self-authored calibration proves
real-world validity. Review the case evidence and human rubric before expanding.

## Use through the existing workflow framework

Save the complete folder under `workflows/contractnli-eval/` in a room Memory,
inspect the existing source before replacing anything, and call
`(workflow/check-files relative-file-text-map)`. Checking source is inert and
neither certifies saved bytes nor runs/admit a checker. Then obtain
`(workflow/action-links "contractnli-eval")` for room-scoped Run once, Schedule
and Benchmark confirmations. Exact saved scorer execution requires operator
admission and a finite existing allowance. No new legal-evaluator plugin is
required, and an agent cannot self-authorize scorer execution.

Start with the development split, inspect retained outputs, and repair defects
before using the held-out split. Do not tune against held-out outcomes. Simmis
currently permits up to eight cells per submission: select cases/models within
that bound rather than claiming this tiny pack is a large benchmark.

The task's `/out/review.md` is a single execution's proposed file artifact. The
host's standard benchmark publication creates `reports/benchmarks/<scorecard>.md`
and links it through `reports/index.md`; those dynamic tables read retained
results. This pack adds no dedicated leaderboard or second result database.
No candidate models have been run or compared while authoring this example.
