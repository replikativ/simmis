# Normalized legal-matter content benchmark

These are reusable checker components and projection/aggregation groundwork,
not a directly installable workflow bundle: no dataset, `workflow.edn` or
standalone public authoring script is included. The local operator trial is
a Simmis adaptation, not a Harvey LAB standard-format or standard-judge
reproduction. Its recorded authoring script freezes inputs from the pinned upstream
matter. Only source-bound decoded evidence enters clean candidate fixtures.
Original Office bytes and exact XML are retained by the operator for inspection.
No LLM summarization prepares the source evidence.

The solver writes a Markdown memo and structured EDN tracker. The checker here
validates artifact presence/shape and literal source-quotation provenance only.
A passing reward does not grade legal reasoning, issue coverage or correctness.
Those require a separately admitted, subject-bound semantic grading workflow;
its result never rewrites the original Attempt/Scorecard.

Candidate sources exclude upstream task criteria, calibration, expected answers
and the authoring workspace. Missing evidence is uncertainty, not a license to
guess. Native Office output and original grading protocols remain separate
capability milestones. Keep source revision/hashes, extraction policy, adapted
task, output schema, judge model/prompt/scopes and grading version in reports.

## Word support and this experiment

Ordinary room agents have `office/*` support for opening Word packages,
extracting text, inspecting XML parts and editing those parts in their fork.
The room binding currently disables binary save-as. The native catalog
benchmark fixture and output-capture path also assumes text; the existence of
binary-capable file-result APIs does not make that path binary-safe.

This pilot therefore stages decoded paragraphs and spreadsheet cells before
inference. It preserves source hashes and paragraph/cell coordinates, not
Word layout or a full rendering model. The supplied projector is bounded and
rejects selected unsupported review/embedded constructs. It is not a general
lossless Office converter; exact inputs require an independent preflight.
Native Office input/output remains a separate end-to-end milestone.

## Assessment layers

1. The solver's native Attempt records completion and artifact checks. Reaching
   a step limit remains failure even if valid draft files were captured.
2. `judge-checker.clj` validates the identity, criterion coverage and literal
   output quotations of a separate assessment. Its reward measures assessment
   structure, never the correctness of the judge's verdicts.
3. Positive and negative controls test the judge before subject assessment.
   Small synthetic controls are not expert legal validation.
4. The pure `matter-grading/aggregate` helper uses a frozen criterion universe.
   Only completed, settled, subject-bound and structurally valid judge batches
   contribute verdicts. Missing/invalid batches and unassessable criteria stay
   unresolved in the fixed denominator. Report assessment completeness with
   all-pass status. The original solver's Scorecard is never rewritten.

The supplied judge checker is contract v2: each excerpt-based criterion allows
one to eight exact quotations. The original pilot's four-quotation limit was
too narrow for a discovery question covering three source/statement conflicts.
Keep that failed assessment under its original frozen checker; a v2
counterfactual is separate from native execution success. This wider provenance
bound does not validate a judge's substantive reasoning.

The initial custom protocol partitions the 50 published criteria into four
clean fixtures: 24 memo criteria, 23 memo criteria, two tracker criteria and
one cross-deliverable criterion. Each judge sees only its permitted outputs
and rubric, not the source corpus. This measures rubric agreement rather than
independent source-grounded legal truth. It uses a single model and batched
criteria, unlike the original per-criterion dual-judge Harvey protocol.

Freeze the representation changes before judging: C-042 waives XLSX production
in favor of a well-formed EDN tracker; C-043 retains the original tolerance of
up to two missing columns using substantially equivalent EDN fields; C-044
changes file representations while retaining the original numerical
thresholds. Preserve the original rubric text beside every adaptation. A
missing deliverable leaves its required batch unresolved and is separately
reported as an artifact failure; never manufacture placeholder content.

## Controlled discovery intake

`discovery-checker.clj` is the separate v2 artifact contract for a small
fictional question-answering pilot. It checks all expected question IDs, a
readable report and exact document/line-bound quotations. It does not require
a complete ontology, particular entity IDs, or every incidental source fact.
The private evidence annotations are not candidate fixtures.

Each answer contains `:id`, `:status`, `:answer` and `:evidence`. Status is
`:answered`, `:partly-answered` or `:unanswerable`. An unanswerable answer may
have empty evidence and explain the missing information. An ordinary answer
requires supporting quotations. Optional `:basis` is derived from those fields
and must agree if supplied: `:evidence` for quotations, `:whole-packet` for an
unanswerable answer without quotations. This removes a redundant required
label; it does not relax source-quote checks or establish semantic correctness.

Citation entries contain `:document`, one-based `:line-start`/`:line-end`, and
an exact `:quote`. Ranges include at most 21 lines and quotes at most 4,000
characters. A genuine but irrelevant quotation remains a semantic failure,
not a fabricated-quotation failure. Actual semantic review must receive the
authorized source packet as well as answers and a frozen per-question rubric.
Do not borrow facts from another answer without an explicit cross-reference.
Supply each exact public question alongside its candidate answer and rubric;
dates in a rubric alone do not define the referent of a leading yes/no answer.
A source-only quotation can establish a reference fact while failing to show
what the candidate actually said. Future judge protocols should check both
candidate grounding and source grounding, with explicit absence rules.

Keep artifact-contract revisions separate from historical results. A
counterfactual check of an existing answer under v2 does not change its v1
Scorecard or repair substantive contradictions and omissions. Later evidence
belongs to a separate phase with the initial output identity retained.
