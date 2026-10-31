# Optional original-evidence digest workflow

`is.simm.starter.conversation-digest/source` returns the shipped editable
`summarize-room` v1 workflow. Loading it does not install source, select a model,
start a Job, allocate resources or modify a room. It uses the ordinary manual
saved-workflow API with an explicit `evidence-manifest` string parameter and
returns `/out/digest.edn` from the isolated retained room fork. It has no
knowledge `input-selection.edn`, batch cursor or processing acknowledgement.

The HOST constructs the selection with
`is.simm.model.conversation-digest/selection`. Room-message evidence artifacts
supply message ID, source revision, path and original content ID; generic document
or capture originals supply an opaque source ID instead. Optional `file-content`
is the retained artifact file's exact content identity, distinct from the raw
message content ID. The HOST supplies informational authority/source-kind and
verifies retained bytes before admission. A digest cannot establish that proof.
For generic documents, the source ID is stable for the path and the source revision
combines that path with its exact file-content identity. Distinct paths containing
identical bytes therefore remain separate originals for coverage and citations.
Selections contain at most eight originals and must fit the existing 4000-character
parameter limit; oversized explicit selections refuse without truncation.

A digest contains the exact selection ID, one summary, five fixed section vectors
(decisions, proposals, observations, uncertainty and open work), and exact original
coverage. Each section item has plain text and structured source-revision citations.
Coverage distinguishes represented originals, deliberately omitted inspected
originals, and deferred unresolved originals. Citations may reference represented
originals only. Unknown fields, HTML, missing/duplicate originals, mismatched
selection identities and invented source references fail validation.

Before display, the HOST validates the parsed result with `validate!` against its
retained original selection. `digest-id` provides a portable integrity identity,
not authority or an acknowledgement. Render plain text and source links; never
interpret generated text as HTML. The contract preserves attribution metadata but
does not prove semantic classification, understanding or truth. Existing budgets,
Jobs, Runs and explicit resource admission remain the execution mechanism.

`examples/workflows/conversation-digest-eval-v1/` provides one synthetic assistant
proposal and a model-free reference. Mechanical calibration detects missing output
and a fabricated human approval. Neither the fixture nor its tests run a model.
The source task instructs the worker to read every admitted original, retain
uncertainty and avoid treating screenshot descriptions or links as direct vision.

Omitting an original from a digest never changes wiki-maintenance eligibility.
A deferred digest item never becomes a knowledge no-op or processing receipt.
The digest is optional navigation over originals, not another source of authority.

The optional browser bridge starts with **Prepare digest** after an explicit room
evidence batch has been saved. It previews at most eight selected already-retained
original paths, verifies native file content identities and freezes those exact
bytes into the ordinary workflow request. The default model recommendation comes
from the existing workflow readiness resolver; an explicit model field remains
available. Missing source can be installed only after confirmation into an empty
workflow subtree. Customized source is preserved.

A confirmed run uses the existing owned saved-workflow admission with visible
ceilings of 25,000 microdollars, four dispatches, eight model steps and 180 seconds,
with discard settlement. It neither adds allowance nor activates a schedule.
`digest-policy.edn` opts into a pure pre-admission check: the parameter selection
must be valid and every selected frozen input must match its mandatory exact
`file-content` identity. Missing/placeholder selections stop before model work.

**Load completed digest** is manual. The HOST checks the canonical Request and
retained payload, exact frozen originals, completed root Job/direct certified
Attempt and structured result before display. It does not fall back to today's
workspace. The expandable card shows typed sections and exact frozen source
excerpts; links to current files and message context are explicitly separate
navigation. Source attribution inferred only from a file remains unknown.
Uncertain installation or launch outcomes stay held and are never automatically
replayed after reconnecting or hiding the card.

**Keep digest in chat** explicitly publishes a durable generated navigation card
after result verification. It records the canonical Request and digest identity;
expanding the card after a browser reload loads the same certified result and
frozen originals through current Room permissions. Publication is claimed once;
an uncertain native post requires inspection and is not replayed automatically.
This does not write a derived digest file, acknowledge knowledge processing or
remove the original messages.
