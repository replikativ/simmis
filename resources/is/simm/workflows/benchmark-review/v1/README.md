# Advisory benchmark review

This ordinary workflow reviews frozen source material, a submitted answer,
reference data and recorded results. It can automatically diagnose execution,
format, annotation, answer and evidence problems, propose a change or abstain.
It changes no accepted references, checkers, results, schedules or permissions.

Replace the synthetic `fixtures/inputs/review-packet.edn` with an explicitly
authorized packet. Give it a content-derived `:packet-id`, at most 16 cases,
case-local source IDs/text, the task contract, submitted answer, reference,
recorded results and exact Attempt/content/environment/source provenance.
The synthetic example is not a semantic-quality benchmark.

Run in a clean review scope permitted to read these answers and references.
An ordinary workflow Run inherits room files/history; pinning the packet does
not make the rest of its room private. Keep gold-bearing review material out
of blind solver rooms and use clean fixtures for solver benchmarks.

Inspect and save under `workflows/<name>/`, then use the normal admitted
workflow-run with a stable request ID, finite resources and discard settlement.
Loading this starter performs none of those actions. After completion, collect
the certified Attempt and immutable `/out/review.edn` through the existing
result APIs. Never substitute current workspace files for captured outputs.

Ordinary workflow-run does not invoke `checker.clj` or calibration. The caller
must separately validate the captured review, using the exact pinned checker
and a `gold` map containing the packet ID and case IDs/sources (the full packet
also suffices). Update `gold.edn` and calibration when adapting this example.
Use the existing bounded checker executor; do not eval authored checkers on the
host. Validation checks structure, packet/case binding, source-quote fidelity
and advisory declarations. It does **not** certify the review's meaning,
legal correctness, independence or truth. An all-insufficient-context report
can be structurally valid and still provide no useful assessment.

Review the reviewer against independently adjudicated examples before using
its judgments for consequential decisions. A reference or grading-policy
change requires a human decision bound to the exact proposed source and
current target revision; then publish a new benchmark version and preserve
old Attempts and Scorecards. Neither editing a Markdown file nor a review
claiming approval creates that authority. This starter does not implement a
human-assignment API or automatic grading promotion.

See [the development process](DEVELOPMENT.md). Review artifacts can be ordinary
Markdown files linked from the standard benchmark reports, with captured raw
outputs and provenance retained. They are advisory reports, not native
Assessment records.
