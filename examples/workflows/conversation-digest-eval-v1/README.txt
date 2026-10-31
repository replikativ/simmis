# Optional conversation digest — model-free development fixture

This separate example uses the ordinary summarize-room v1 saved workflow API.
Its evidence-manifest parameter is an explicit bounded selection, not a knowledge
input-selection sidecar. The fixture contains one original assistant proposal,
its exact source revision/content identities and retained-file content identity.
It contains no provider client, ledger, cursor, knowledge acknowledgement or live
room write. Nothing runs or spends when the source is loaded.

Save these files as a separate ordinary workflow bundle, inspect its exact bundle
identity and use the existing manual saved-workflow operation. The example's
literal parameter names the supplied synthetic original; a real manual request
must override evidence-manifest with a HOST-validated selection of retained real
originals. Source metadata is not a substitute for reading those originals.
Normal workflow authorization, setup, resource limits and explicit admission apply.

Expected output is one closed EDN digest at /out/digest.edn. The pure model contract
checks the exact selection ID, original membership, selected/omitted/deferred
coverage and structured source citations. Sections distinguish decisions,
proposals, observations, uncertainty and open work. Attribution classes are
informational, not permission. Text is plain text; generated HTML is unsupported.

The positive reference keeps an assistant suggestion separate from human approval.
Calibration includes missing output and an invented human approval. The editable
checker checks fragments only. HOST validation and retained-original verification
must precede display; a checker result is not truth or publication authority.
This fixture and its tests make no model-quality claim and invoke no model.

A digest is optional navigation. An original omitted from it remains eligible for
wiki maintenance. A deferred source is unresolved, not silently inspected or
processed. Digests never establish knowledge incorporation or publication.

The explicit example builder is
is.simm.starter.conversation-digest-test/write-example!, which writes this separate
fixture bundle without a model or database. It retains the ordinary task and
checker, with a literal example selection in the task parameter and evaluator-held
gold/calibration alongside synthetic fixture text.
