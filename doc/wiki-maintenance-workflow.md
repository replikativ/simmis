# Wiki maintenance as an ordinary workflow

The version-5 starter is source data returned by
`is.simm.starter.wiki-maintenance/source`. It contains the files for
`/workflows/maintain-wiki/`, not a second agent, scheduler or resource ledger.
Loading it does not install anything, choose a model, allocate money or enable
a schedule. Installation into an existing room uses the authorized owned-
workflow writer and preserves user edits. An exclusive new canonical repository
initializer also includes these same ordinary files before its initial commit
and Room publication; reopen never reinstalls them. Personal alpha bootstrap
creates new canonical spaces this way, without enabling or funding execution.

`is.simm.starter.install-wiki/install!` is an explicit HOST-only inactive
installer for a selected canonical personal/project/group room. It uses the
existing room `:admin` policy (currently owner-only), read+write authority and
the original owned target. These checks are repeated at the native effect
boundary; a global administrator gains no content bypass. The entire named
workflow subtree is compared, including modes and extra files. Empty source is
installed with its inspected target-bound revision and whole-subtree CAS;
byte/mode-identical source is an authorized read-only repeat; customized source
is preserved. An uncertain native acknowledgement propagates as pending without
automatic replay. This operation never discovers/creates rooms, converts legacy
stores, runs a model, allocates resources or changes any schedule activation.

```text
inbox/ + transcripts/ + captures/ ──> frozen trigger manifest + copies
wiki/ ────────────────────────────> retained repository checkpoint
                                             │
                    ┌────────────────────────┼────────────────────┐
                    │                        │                    │
               manual native Run      scheduled native Run   native benchmark
                    │                        │               (selected-case
                    └──────── isolated Run ──┘                checkpoint)
                                  │
                            /out/wiki/*.md
                                  │
                      retained proposal → review → adoption
```

The existing knowledge collector selects Markdown in the declared folders.
The retained checkpoint identifies execution context, separately from the
trigger identity: adopting
a wiki update must not by itself retrigger maintenance. Empty/unchanged scheduled
inputs can be skipped by existing admission before submitting a funded Run.
Chat messages are not collected directly; they need an explicit evidence-file
ingestion step. An overview is ordinary Markdown, not a new schema entity.

Use **Save to Memory** on a human message to inspect and save an exact evidence
snapshot under `inbox/conversations/`. The saved confirmation links to **Review
knowledge update**, the existing room-scoped maintainer setup. Opening it starts
no Run, grants no allowance and enables no schedule. The workflow must exist and
its current setup still needs explicit confirmation. Imported Git repositories
are not silently modified to install a starter.

Capture inputs are the Markdown observation notes. Their relative image and
recording links and exact asset identities remain in the source, while binary
media stays outside the frozen model inputs. A visual description is sampled
model evidence: it can misread text and cannot establish that every event was
captured. Server receipt timestamps and estimated recording positions retain
their qualifications. An agent must verify an asset's recorded identity before
treating a retrieved current file as the original.

Version 5 opts into incremental batching and native retained-checkpoint execution
through `input-batching.edn` and `execution-origin.edn`. Each invocation receives
one frozen pending prefix, with at most 32 trigger files and 1 MiB of trigger
bytes. Wiki context remains under `/wiki/` in the exact retained repository;
it is read on demand instead of copying up to 32 pages into the evidence inputs.
This removes the copied-context selection cap, not model input, scoring or
resource limits. The execution envelope still bounds each copied file at
512 KiB and the total at 2 MiB, including manifest and instruction fixtures.
Oversized head evidence holds admission without truncation or silent skipping.

Native server execution requires discard settlement: `/out/wiki` files become
reviewable proposals rather than merging an entire historical Run world. Manual
and scheduled workflow invocations share the same retained-batch path. Native
benchmarks construct clean independent case checkpoints with the same structural
scoring profile; scheduled checkpoint benchmarks remain unsupported. CLI and
local-worker execution refuse version 5 before creating work. Use an explicitly
chosen historical/custom copied-context program for those executors; no backend
or program is silently substituted.

Historical versions 1–4 retain their original bytes and behavior. Version 4 uses
bounded copied wiki context, while versions 1–3 use whole-selection behavior.
Inspection distinguishes missing, current, pristine older, and customized source.
An explicit **Upgrade starter** action accepts only the exact inspected revision
of a byte/mode-identical older shipped bundle. It uses the existing authorized
whole-subtree source writer and preserves customized bundles. A changed revision
requires inspection again. After a lost acknowledgement, inspection observes
current source but does not certify the original write or clear its uncertainty;
the client never automatically replays it. The original App and Room lifetime own queued
inspection and upgrade work through final validation. Upgrading source changes
neither funding nor schedules. Already-admitted and historical work retains its
original deployment; new admissions hold on a named-source mismatch until the
deployment is explicitly reviewed and replaced. A changed program,
policy or revisit epoch identifies a different stream rather than migrating an
old cursor.

Installation leaves schedules inactive and supplies no execution resources.
Operational backlog and hold diagnostics remain a separate milestone before
recommending unattended default deployment.

For agent retrieval of canonical files, use `memory/search` and `memory/read`.
They return paths, source/content identifiers, semantic structure and preserved
properties in the caller's current file world. Capture metadata remains
ordinary source data; a media link does not inject an image into a model.
The legacy `wiki/*` vocabulary continues to address explicitly attached KBs.
Canonical search is currently literal and bounded; it does not promise a
full-text index or an unlimited archive scan. Non-ready views carry diagnostics.

The starter asks for small source-backed notes and useful overviews, conservative
updates to existing documents, stable identities/properties, explicit uncertainty
and attributed contradictions. Only complete proposed Markdown pages belong in
`/out/wiki/`; the host retains them for the existing file-output proposal path.
No source deletion, schema migration, whole-world merge or automatic adoption is
enabled by the starter. A completed Run is not a claim of factual correctness.

## Benchmarks are explicit

`examples/workflows/wiki-maintenance-checkpoint-eval-v5/` is a separate reusable
evaluation bundle. Its six live program files match the installed version 5
starter exactly. Synthetic cases cover mixed imported notes, human conversation
and capture observations, capture uncertainty, and already incorporated evidence.
Gold and calibration are evaluator-held; the bundle grants no execution resources.
Calibration and native preparation are checked by the existing checkpoint test.
These are mechanical checks, not fresh model-quality results. Use this bundle
for a finite native subscription benchmark, then deploy the ordinary live starter
after reviewing its output. Never install synthetic fixtures into a real wiki.

`checker.clj` is a benchmark-only mechanical regression checker. Ordinary manual
and scheduled saved-workflow execution uses host completion and exact capture;
it does **not** execute the authored checker. Do not label every scheduled
proposal quality-checked on the strength of this file.

`is.simm.documents.preservation/check` is a separate version-1 pure conservative
policy over bounded parsed BEFORE/AFTER Markdown. It retains existing explicit
page/block identities, type assignments, alias-to-property/type bindings and
block metadata scopes. Existing authored values must remain present but may be
updated, including false, zero or explicit null when independent canonical
validation allows them. Unknown page/block metadata, including nested values,
must remain equal. Mutable aliases come from BEFORE: adding a binding cannot
reinterpret a previously unknown field. New types/aliases are allowed, subject
to separate canonical validation. File-only proposal acceptance applies it with
canonical preflight. There is no automatic acceptance.

The retained comparison stays inspectable independently of the Run. A single
file-output Proposal offers **Check current workspace**, followed by **Accept
reviewed files**. The check is an explicit review decision, not a refresh needed
to load Memory. Acceptance recomputes the full canonical source/definition/
constraint/target fingerprint before claiming publication. A changed review
requires an explicit new check; it is never silently retried. The version-2
adoption journal retains the accepted review revision as immutable intent data.
This digest is audit attribution, not a replayable historical snapshot or proof
of factual correctness. Mixed-component Proposals remain review-only.

Retained output inspection also extracts bounded Markdown citations. **Inspect
cited sources** explicitly reads the original frozen manifest; only exact listed
paths acquire links beside the proposed text. Code, comments and unused link
definitions are excluded. Missing/unavailable sources never fall back to today's
Memory. Citation parsing failures are visible without hiding the full output.
This does not establish historical citation provenance for arbitrary manually
authored or already adopted wiki pages.

An uncertain completion offers **Check recorded completion**, a separately
authorized historical receipt reconciliation. It does not publish again or
overwrite later human edits. Browser actions own the application lifetime and
the exact Room through all body access, publication and finalization. Current
human authority and the closed component set are checked before hydration.

These restrictions belong to default maintenance, not the general editor.
Removing stale properties/types, renaming aliases and deleting explicit blocks
need explicit editing/review. Ordinary unbound `tags`/aliases are unknown
metadata too: even append-only
tagging needs a separate explicit policy exception rather than silent mutation.
The policy preserves parsed metadata semantics,
not comments, formatting or body text; it proves neither factual correctness
nor an unchanged reviewed basis. A new page has no fabricated empty BEFORE.

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

## Current integration boundaries

### Explicit inactive setup

Workflows observes whether an eligible canonical workspace has the exact shipped
maintainer, a customized version, or no files in its named subtree. An owner can
choose **Set up wiki maintenance** for an empty subtree. Existing customized
source is preserved and can be opened in the ordinary editor; setup never
switches away from or replaces an unsaved draft automatically.

The fixed browser service holds application lifetime, checks current human room
read/write/admin authority before hydration, then holds the exact native Room
lifetime before inspecting its canonical repository or calling the installer.
Nested installer/authoring lookups reuse that leased Room; a changed UUID/slug
mapping refuses before any newly selected repository is inspected.
It does not require or confer room merge authority. A profile marker establishes
repository eligibility, not that all documents are valid.

Inspection describes current saved source, not an earlier write acknowledgement.
An acknowledged setup result may be an exact-source or customization decision;
it does not necessarily represent a native write.
An uncertain setup remains pending: **Check saved setup** only observes source
and cannot replay installation, even if the subtree still appears empty. No
model, allowance, enabled schedule or automatic adoption is part of setup.

### Manual output collection

For a newly submitted hosted knowledge workflow, Workflows offers **Review wiki
output** beside the Run result. Use it after the Run finishes. The action reads
the original canonical request and its direct certified Attempt, retains the
exact output, and links an ordinary Proposal. It does not run the checker source,
repeat inference, recapture today's inputs, or accept the proposed files. A
verified empty output is reported as no wiki changes, not an empty Proposal.

An unfinished or uncertified Run, revoked permission, a benchmark request, or an
unavailable historical source is not a reviewable result. Collection may be
retried: an existing immutable disposition and reserved chat message are reused,
never a new Job. Inspect the resulting Proposal, check it against the current
workspace, and explicitly accept its reviewed files. Mechanical certification
does not establish that the content is factually correct.

This browser action requires the original request author and current room merge
permission. It is not added to existing signed MCP capabilities. Local-worker
output certification remains a separate integration; this control is not offered
for local-worker or benchmark submissions. Existing scheduled knowledge requests
already use the same filing helper automatically. Neither path advances an
adopted-memory watermark merely by filing a Proposal.

Canonical prospective validation, preservation and exact AFTER snapshot guards
support explicit reviewed file-only acceptance. Processed input, a filed
proposal and an adopted result remain distinct. The installer is separate from
exclusive new-repository initialization. Neither path
configures an admitted model and finite allowance nor enables a schedule. The
mechanical fixture checks are not actual-model quality measurements.

The shipped-source scheduling regression uses the actual packaged maintainer
under its own workflow name, an explicitly finite fixture allowance, and a
scripted HTTP response through the real provider adapter and native Job. The
activation target and admitted request retain the exact packaged source. Empty
evidence skips without spending. A certified empty capture produces a no-op;
manual collection before automatic filing and subsequent retries share that
same disposition. Unchanged triggers and context-only edits skip new capture,
funding and dispatch. The Schedules DTO exposes the resulting no-change outcome
without adopting files. This is deterministic integration evidence, not a live
subscription benchmark or a fresh-process recovery test.

After successful staging, the knowledge collector and benchmark setup bind an
explicit execution-world source-memory profile. This distinguishes owned
frozen-input/output staging from canonical knowledge: `in/knowledge` and
`out/wiki` are excluded only in the matching disposable execution world. The
immutable role follows copies but is not adopted into the ordinary parent by
system settlement. Ordinary room selection remains unchanged; retained evidence
remains accessible as files, and `/out` pages do not become canonical knowledge
merely because a Run wrote them.

### Model choice and shared allowance

The hosted execution form lists exact locally registered native models. Opening
it reads local metadata and credential presence only: it does not refresh provider
catalogs, initialize a model client or make an inference request. A detected login
is not proof of account access, remaining quota or model reachability. An
undetected default login/environment credential does not rule out a separately
configured host connection. Existing selections absent from the current options
are preserved, rather than silently changed to another model.

**Shared room allowance** observes the actual Room root wallet, excluding funds
already allocated to child Runs. It is separate from the form's per-Run or
per-occurrence ceilings and from historical provisioning receipts. Amounts remain
exact decimal strings. A real zero differs from an unavailable or inactive wallet;
a metadata read never hydrates a Room, opens a wallet, mints resources or starts a
Run. Observations are invalidated by existing connection/authority/room hints and
reread after acknowledged grants, submissions and schedule creation. The displayed
amount is a snapshot; execution admission checks current availability again.

The browser read retains the original App and exact admitted native Room while
reading the ledger, then rechecks current human read authority before returning.
Schedule status remains visible to readers when the owner-only starter setup card
is unavailable. A blank model disables new Run and schedule activation requests;
retries still refer to their previously retained immutable payloads. Installing
source, choosing execution settings, provisioning allowance and enabling a
schedule remain separate explicit operations.

The workflow form also offers **Save paused schedule**. It retains the inspected
source and explicit execution settings without making any occurrence eligible.
The existing activation action remains separate. An uncertain creation retry
uses its original initial setting and payload even after form edits, and its
receipt reports its acknowledged enabled/paused state; current status is read
separately in Workflows. Agents can use
`:enabled? false` on the ordinary schedule-create operation; no second schedule
or resource model is introduced. Existing pinned activations do not follow
later edits to the workflow files.

The reserved `:simmis/checker-profile` selector stays in the saved definition and
grading basis; it is excluded from editable task parameters, matching ordinary
execution admission. Users can launch and preview schedules for the packaged
workflow without editing that internal setting. An ordinary unqualified
`:checker-profile` string parameter remains a user parameter.

The opt-in `:wiki-checkpoint-structure-v1` scoring profile also observes ordinary
`/wiki/` Markdown from the selected case's shared and case-specific fixtures.
This supplies a preservation baseline without copying those pages into the
knowledge manifest. Its verifier identity differs from `:wiki-structure-v1`,
whose copied-context semantics remain unchanged. Conflicting copied and ordinary
wiki bodies, malformed paths and duplicate document/block identities refuse
scoring; the existing structural parser's file and byte bounds still apply.
Both calibration and final verification use this same observation path.

Ordinary wiki context does not become new evidence: quote and citation source
texts for this profile come only from trigger entries in the verified knowledge
manifest; copied context is also excluded. This profile observes
retained fixture data, not a live Room. It does not itself provide checkpoint
admission. The owned native benchmark facade supplies checkpoint admission;
CLI and local-worker entry points continue refusing this policy.
