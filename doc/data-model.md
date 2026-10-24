# The data model

## File-backed room Memory

Room Memory is the room's existing Geschichte repository, not another drive
or a writable browser store. Folder listings carry worktree path/content/size/
mode metadata; file contents are fetched by the captured content identity.
Markdown is recognized by extension at any path. `wiki/` is an organizational
convention, not a separate database boundary. Room-local source and workflows
use these same files; the loaded SCI interpreter state is not a file entity.

The Markdown codec retains exact source and derives source-positioned syntax
nodes separately from semantic Simmis blocks. Explicit paired block markers
carry stable block identities and child grouping; ordinary unmarked Markdown
has provisional blocks. A list inside a block does not implicitly become its
Simmis children. File YAML properties, typed property projections, and the
source-selected disposable query view do not install untrusted schema or
execute imported code/widgets. Navigation exposes canonical Room workspaces.
Account provisioning ensures the personal room without creating a separate
DB-backed Wiki. This surface does not import the old outliner stores.

New Team and Memory forms create the same canonical `:project` Room. The owner
and client creation UUID determine one native slug. `:room/creation-intent` holds
that UUID and `:room/creation-basis` holds the canonical request digest, both typed
UUID attributes written atomically with `:room/created-by`, content scope and
membership. These receipts distinguish completed Simmis publication from native
pending/ready preparation. They contain no EDN payload or transferable permission.
The original typed receipt remains observable after a lost final acknowledgement;
observing never provisions or resumes setup.

Folder listings are authorized server reads cached in context-owned Spindel
state, not a replicated Datahike file-tree index. The navigation cache retains
up to 64 listings; collapsing a folder does not discard a fresh listing.
Reconciliation covers visible folders and pending requests. Private room-change
hints invalidate closed caches, while open listings reconcile in place.
Reconnect clears cached listings and revalidates the target. File contents
remain on demand, and editor drafts and sidebar navigation have separate owners.

Rich-document code fences use presentation-only syntax decorations for explicitly
recognized language names and aliases. Tokens are derived from the original
code with the same shipped grammars as chat, without changing editor content,
block identities or saved Markdown. Unlabelled/unknown languages and cells beyond
the highlighting bounds remain plain code. Viewing, editing and highlighting
code do not evaluate it.

### File and block addresses

Memory files have `/room/<room>/files?path=<encoded-path>` addresses. An
explicit semantic block has `/room/<room>/files?repo=<repository>&block=<block>`;
an optional `path` is only a location hint. The repository UUID constrains the
room's canonical owned-default repository. It does not select another world.
Block links follow file renames and moves between Markdown files within that
repository. Rich editing exposes a small Copy block link control beside each
saved, explicitly identified block. These are live references, not historical
citations, embeds or the legacy outliner's `((UUID))` syntax.

Lookup reads one immutable repository snapshot and proves uniqueness across
all admitted Markdown: at most 20,000 metadata entries, 512 Markdown files,
8 MiB of combined source and 100,000 parsed nodes. Exceeding a bound or finding
malformed semantic source refuses lookup rather than returning a partial hit.
Duplicate IDs are ambiguous. The host rechecks current ownership, human read
permission and the complete Markdown descriptor after parsing. No projection,
schema or index is installed while following a link.

Inbound links open new tabs. Selection uses the actually loaded file's saved
analysis, not the resolver's earlier offsets. Native editor callbacks also
check that no unreported typing occurred; an unsaved draft prevents the jump.
The intent, acknowledgement and selection belong to the tab's Spindel context.
Native handles remain element-owned. Markdown source supports the same jump
when rich projection is unavailable. Opening or copying a link never evaluates
code or writes the file.

The collapsed Backlinks section in file context lists canonical Simmis file and
block URLs from saved Markdown in this workspace. Inline and defined reference
links count; code, comments and unused link definitions do not. Relative paths,
wiki-name spellings, external URLs and frozen experiment inputs are not resolved
into live files. A block destination follows its explicit identity despite a
stale path hint. Each origin opens its innermost explicitly identified block,
or the source file when there is no such block.

This is a bounded read of the selected repository, not another persistent index.
The source scan uses the block-lookup limits above; it admits at most 5,000 links
and returns at most 50 matches within 128 KiB, marking a truncated result as
incomplete. Duplicate explicit page or block identities refuse the observation.
The host checks the original application, room lease, current human authority,
repository binding and complete source descriptor before replying. Parsed trees
remain local to that read. The tab stores only a bounded reply and its marker in
Spindel state. Saved metadata changes and reconnects invalidate it automatically;
unsaved typing does not change the saved-workspace result. Closing a tab retires
its reads and navigation; moving the tab preserves its current column owner.

Frozen workflow sources have separate
`/proposal/<proposal>/source/<component>?path=<encoded-input-path>` addresses.
Proposal and component identify already-retained evidence; the path must appear
in that evidence's source manifest. These URLs convey no permission and never
select today's workspace file. The read-only source tab shows the original
source path and content identity, including after Proposal resolution.

Renaming retires old-path reads and history requests before reconciling current
metadata; late replies cannot reopen the removed path. Typing during the move
follows the file and resumes autosave at its new path. Address notifications use
the live execution context's stable home, so ordinary execution frames update
browser history while simulation forks retain their own layout.

Rich list serialization alternates standard Markdown markers for adjacent
lists so reparsing preserves their boundaries and numbering. Selective edits
to a region containing adjacent sibling lists serialize that region together;
other source regions remain untouched. The host validates both the serialized
model and the actual patched source before accepting an edit.

### Saved data-only charts

New canonical spaces install the optional `:simmis/typed-chart` version 1
definitions. They are separate from the mandatory canonical knowledge profile;
existing spaces remain valid without them. Opening a file never installs or
repairs definitions. Partial, colliding or modified Chart definitions refuse
activation, including changed storage, codomain or inherited constraints.

An explicitly identified semantic block assigns type
`2d4a6ef1-5159-45d6-b12d-683064cb8cee` (`S/ChartV1`) and binds its required
string property `0bcb85c0-405d-46b2-90b9-d34d81351975` (`S/ChartV1/spec-path`).
The property value is an exact path from the room repository root, such as
`data/sales.json`. It uses the ordinary `simmis.blocks` declaration format
below. Chart appearance is derived; no chart data enters the Markdown editor
model or changes the block's prose, identity or source serialization.

For example, a complete Markdown declaration looks like this:

```markdown
---
simmis-id: aaaaaaaa-aaaa-4aaa-8aaa-aaaaaaaaaaaa
simmis:
  version: 1
  blocks:
    bbbbbbbb-bbbb-4bbb-8bbb-bbbbbbbbbbbb:
      types: [2d4a6ef1-5159-45d6-b12d-683064cb8cee]
      properties:
        spec:
          id: 0bcb85c0-405d-46b2-90b9-d34d81351975
          type: 2d4a6ef1-5159-45d6-b12d-683064cb8cee
      values:
        spec: data/sales.json
---
<!-- simmis:block bbbbbbbb-bbbb-4bbb-8bbb-bbbbbbbbbbbb -->
## Sales
Ordinary editable explanation of the chart.
<!-- /simmis:block -->
```

The full-file Markdown view contains this frontmatter. The rich toolbar's local
Markdown editor contains only its selected prose region, so it does not include
the enclosing type declaration. The JSON is another file in the same Memory
tree. Changing it refreshes the saved chart projection without changing the
Markdown explanation.

The referenced UTF-8 JSON uses a closed versioned data format:

```json
{"version":1,"kind":"line","title":"Sales","series":[{"label":"Revenue","points":[[0,10],[1,15]]}]}
```

Kinds are `line`, `point` and `bar`; bar x values are category strings, while
line/point coordinates are numeric. Optional `x-label` and `y-label` name axes.
Series labels are distinct; category labels are distinct within each bar
series. Input order is retained and missing categories are not zero-filled.
The format admits at most 64 KiB, eight series and 2,000 total points, with
bounded text and finite coordinates of magnitude at most 10¹². Duplicate JSON
keys, transforms, expressions, URLs, raw HTML and arbitrary chart grammars are
unsupported. Metadata admission bounds ordinary file reads; the parser's
actual-byte cap is not a corrupt-storage allocation containment mechanism.

Read and continuous rich views show up to four saved Chart blocks with native
SVG, legends and an accessible data table. Ordinary fences and provisional
blocks stay inert. Source/local-source editing hides these projections until
the saved declaration is available again. The legacy Vega/SCI widget path is
not used to activate canonical charts.

### Inline chart fences

A `simmis-chart` fence embeds the same closed ChartData v1 directly in Markdown;
it does not require a typed block declaration or another JSON file:

````markdown
```simmis-chart
{"version":1,"kind":"line","series":[{"label":"Revenue","points":[[1,100],[2,140]]}]}
```
````

File analysis validates at most the first four such parsed fences. The preview
matches the exact language and projected code text shown in the editor, including
Markdown nesting and newline handling. It remains stable during unrelated prose
editing, hides immediately when its own text differs, and can return after
matching analysis/autosave or undo. Read and Outline use the same data and
source-independent SVG renderer. Invalid or oversized ChartData gets a bounded
diagnostic while its original fence remains editable; other implementation
failures propagate. `simmis-chart` code uses JSON syntax highlighting.

Typed and inline charts share four mounted previews per document view, including
repeated identical fences. Chart chrome and its native caches stay outside
editor models and source serialization; preview updates do not create undo
steps. Owner/target changes retire the presentation through existing Memory
fences and native element lifecycle. This is a pure data preview, without a
historical-provenance, execution, query or permission claim. Clojure cells retain
their separate strict saved-source Run guard. Vega-Lite, Plotly, arbitrary HTML,
URLs and SCI widgets are not activated by this fence.

### Portable typed declarations

`documents.metadata` provides a separate inert declaration codec, identified by
`:simmis/markdown-metadata` version 1. It does not replace the existing trusted
scalar-property decoder or activate types in the Memory editor. Its `:ready`
status means declarations parsed, not that schema, relations or ownership have
been validated. The codec performs no database reads, writes or evaluation.

Typed documents retain `simmis-id`; ordinary values remain ordinary YAML:

```yaml
simmis-id: "aaaaaaaa-aaaa-4aaa-8aaa-aaaaaaaaaaaa"
simmis:
  version: 1
  types: ["11111111-1111-4111-8111-111111111111"]
  properties:
    revenue:
      id: "44444444-4444-4444-8444-444444444444"
      type: "11111111-1111-4111-8111-111111111111"
revenue: 120000
tags: [research]
```

A binding names a property UUID and its declaring type UUID, never a storage
attribute. File aliases do not rename canonical morphisms. Multiple types are
explicit; tags alone assign none. `simmis.blocks` maps explicit paired block
UUIDs to independent `types`, `properties` and `values` scopes. A block alias
does not collide with the same alias on its page. Within one scope, duplicate
types or aliases targeting the same property diagnose ambiguity.

Raw values, unknown ordinary fields and reference-shaped data remain intact.
A UUID string is not inferred to be a relation; `{id, path}` is also just data
until trusted definitions establish a reference. Paths cannot select identity.
Unmatched block metadata, unsupported reserved fields/versions and missing typed
document identity produce diagnostics without rewriting source or installing
definitions. Plain imports remain provisional; decoding never inserts IDs.

`documents.properties/compile-scope` binds decoded declarations to one captured
trusted categorical DB. Types must be existing entity types; property UUIDs
must have exactly the assigned declaring type and matching category. There is
no inferred inheritance. The compiler enumerates all outgoing properties, so
omitting an alias cannot bypass an explicit required definition. Its digest
includes unaliased properties, their actual storage schema and optionality,
validation/computation descriptors and ownership. Codomain/category and actual
storage types must agree, honoring the shared exact `:morphism/attr-type`
mapping. It never evaluates descriptors. `decode-scope` reuses strict scalar
decoding. The captured digest also retains trusted UI rendering hints and
declared uniqueness; explicit uniqueness must agree with installed storage.
Rendering hints never override the actual storage value type. The decoder's
raw/unknown values remain intact; derived/enum constraints refuse until a
validator exists. Required owner-managed fields get a separate diagnostic,
not an instruction to author them in YAML. Shared storage across multiple
type properties is conservatively unsupported. Relations, unique properties
and owner-managed/computed values still require separate gates; this compiler
does not publish canonical instances or activate typed controls in Memory.

`decode-candidate` is a separate, versioned staging API for later canonical
validation. It always returns `:status :pending`, normalized `:candidate-values`
(not final `:values`), and explicit reference/uniqueness claims. Only trusted
reference-typed properties interpret canonical UUID strings or `{id, path}`
maps; the optional bounded path is an inert hint. Scalar UUIDs and unknown
metadata do not become relations. Targets are not resolved, uniqueness is not
certified and no queries, stubs or transaction data are produced. The strict
two-argument decoders retain their refusals. Canonical preparation consumes
these candidates through the whole-selection validator before publication.

The separate `documents.constraints` stage validates a whole selection of
trusted candidates against the **same immutable DB** used by their compiler
and decoder. Candidate profile/status tags are not provenance and must not be
accepted as certification from an external caller. Before target lookup it
checks duplicate IDs and aggregate entity/type/value/claim work. Planned
entities receive explicit tempids; relations resolve only to these entities
or existing UUID-addressed entities with the exact entity-type codomain role.
No missing target creates a stub, and paths never choose identity.

Both uniqueness modes are checked across owners and the base before any
transaction is assembled. Reference uniqueness compares logical target UUIDs
within the selection and real target EIDs against the base, never a nil
lookup. The result includes bounded codomain, external-target and unique-value
dependencies for freshness. A disappeared target, changed role or newly
conflicting unique value invalidates it; unrelated writes need not. This stage
constructs no transaction or DB; canonical preparation consumes its result
before constructing the immutable projection. This activates no Memory UI.

### Opt-in canonical typed views

`documents.space/provision!` explicitly provisions a new, privately owned,
unpublished connection. It accepts only non-attribute-ref, write-schema
Datahike genesis: empty schema/datoms and unchanged genesis counters. It reuses
the Geschichte/document initializer and the shared `model.store/install!`,
including the categorical seed and accounting governor. A repository profile
is recorded last. Existing or partly provisioned stores refuse; this is not a
migration or document-open callback. The marker indicates readiness, not
permission, exclusivity or continued process-local governor registration.

Passing that profile as `:entity-profile` to the existing `documents.view`
pipeline enables a bounded, immutable canonical typed projection. Roots need
explicit document IDs and acquire the trusted Page role; explicit semantic
blocks acquire the Block role. Additional declared types use the same
`:entity/uuid`, `:instance/of-role` and canonical property attributes as existing
queries. Provisional groups and syntax nodes remain noncanonical. Scope
compilation is deduplicated per view and bounded across the whole selection.

The generated Page title is the full repository-relative path. Its fixed
trusted property definition is validated and its path value goes through
strict scalar decoding and requiredness checks. File aliases cannot target
that generated storage, even with a nil value. Existing title/UUID collisions
refuse rather than upsert. Ordinary unique fields and relation values use the
same pending-candidate/whole-selection preflight, including explicit tempids
for forward and cyclic references. Missing/wrong-type targets, unique conflicts
or other diagnostics produce no partial queryable DB. Changed owner/validator/
computation constraints are not bypassed, and unsupported owner-managed writes
or validators still refuse rather than execute.

The canonical source may contain unrelated authored/domain facts but no
materialized document projection datoms. Preparation never retracts canonical
entities or reuses a derived DB as a new source. It only applies `db-with` to a
captured base, leaving source bytes, the connection and original stores intact.
Freshness checks source selection, codecs, actual compiled definitions,
external target types/existence and unique-value dependencies, including new
UUID/title collisions—not unrelated transaction counts. Checking
the exact immutable DB produced by the view is supported; that does not make
it an admissible source for another preparation. These host-side experimental
views do not migrate the existing wiki or activate typed Memory controls.

`documents.view/validate-replacement` is a distinct, host-only validation
preview for one existing selected Markdown file. It verifies the opened
descriptor against one captured canonical source DB, recalculates aggregate
byte limits and runs the same whole-selection parser, identity, type, relation
and uniqueness stages as `prepare`, with replacement bytes held only in memory.
It never stores a candidate blob, invents content entities or materializes a
queryable DB. Its `:replacement-preview` result has `:valid`/`:invalid` status,
candidate revision and diagnostics, not an actual-source selection or current
view; `current?` explicitly refuses it. A valid preview is neither permission
nor a publication receipt or lease on later schema changes.

`documents.view/validate-changes` shares that pipeline for a bounded batch of
Markdown creations and replacements. Each path carries exact `:expected`
metadata, or nil for a creation. Creations must be absent from the complete
worktree, including explicit/implicit directories and file ancestors; roots and
exclusions do not certify physical absence. Unchanged selected Markdown still
participates in relation, identity and uniqueness checks. Existing explicit
page/block identities must survive a replacement. Candidate bytes remain in
memory with no persisted content identity, source mutation or queryable DB.
The `:changes-preview` result carries source selection, compiled-definition and
constraint data plus candidate revisions, not authority or a complete native
publication witness. `current?` refuses it. Publication still needs physical
target binding and semantic validation against its exact guarded snapshot;
checks of touched paths alone are insufficient. Canonical validity and identity
preservation also do not guarantee preservation of optional properties,
declarations or unknown frontmatter; automatic wiki adoption needs a separate
conservative preservation policy. No adoption path is activated by this API.

`ops.file-output-validation/prepare-in` is an INTERNAL read-only composition
over retained exact file pairs and one fresh canonical destination. It repeats
body/descriptor validation, checks complete prospective canonical validity and
the versioned metadata-preservation policy, and returns bounded immutable host
data without bodies, DB values, handles or functions. Fixed normal repository
selection is used, never execution-world staging exclusions.
`assert-after!` checks the physical target and prepares the complete actual
AFTER source from the exact DB supplied by native publication. Touched metadata
must equal retained outputs; untouched selected files, compiled-definition
revisions and constraint dependencies must equal the fresh preflight. Only
then does it return literal true. This data is neither authority nor proof of
an earlier human review or unchanged agent admission context. Atomic native
checkpoint enforcement, current authority, original publisher ACK ownership
and historical duplicate-first recovery remain separate required boundaries.
`ops.file-output-controller` composes this preflight before
claim, binds its manifest and full target to the invocation's retained plan,
then passes the fixed AFTER guard to native prepared publication. It requires
the advertised native validation capability and owns an exact Room HOST lease
through synchronous completion. The caller must own the outer application and
control-connection lifetime. Its public service uses `preview-reviewed!` and
`adopt-reviewed!`: acceptance recomputes the complete witness and checks the
mandatory semantic review fingerprint before claim. The version-2 adoption
journal hashes this UUID into immutable intent and retains it through every
transition and historical reconciliation. A fingerprint attributes the reviewed
basis but cannot reconstruct it; neither it nor a fresh preflight proves factual
correctness or unchanged agent admission context. There is no auto-acceptance.

`documents.property-source` is a separate pure token-replacement codec for
existing scalar frontmatter leaves. It checks the exact source revision,
uses the same bounded safe YAML policy as the Markdown parser, and converts
parser code-point marks to original UTF-16 source positions. Only the selected
token changes; comments, unrelated metadata, line endings and the body remain
intact. A semantic no-op retains every byte. Reparsing verifies the complete
intended metadata delta. Anchors/aliases, explicit tags, multiline tokens,
missing fields and structured values refuse instead of receiving a lossy
whole-map rewrite. This API neither validates trusted property definitions nor
publishes files; it is not a schema guard or permission boundary.

`documents.property-model` provides host-only inspection and scalar edit
preparation over one supplied immutable canonical source DB. It reads the
actual file, accepts only its page or an explicit paired block target, and
reuses canonical implicit Page/Block roles and generated-title staging. The
inspection DTO carries safe field labels, values, actual storage value types,
rendering hints, editability reasons and exact file/source/scope revisions—not
executable computation or validation definitions. Reflected/generated fields,
unsupported validators, structured values and fields without an existing
editable source token remain read-only.

Its closed edit command names the scope, alias/property UUID and expected
revisions, not client-supplied definitions. Preparation re-derives the binding,
checks identity/ownership, patches one scalar and validates the whole prospective
selection through `validate-replacement`. It returns source for the existing
writer, never transaction data, a saved receipt or permission. The host must
still publish with that same fresh snapshot's atomic writer precondition and
current authorization.

Internal `ops.owned-files/properties!` and `property-save!` compose that model
with current room read/write permissions and the same owned-default target
binding used by ordinary file operations. The save accepts no client schema,
selection policy or writer basis. It prepares against one fresh immutable DB,
captures that DB's Dvergr validation precondition and passes it through the
existing file writer, with current authorization checked again at the native
effect gates. Source/schema races refuse or remain explicitly uncertain; the
original target stamp accompanies a successful canonical receipt. These host
operations also back authenticated `load-memory-properties!` and
`save-memory-property!` Memory RPCs. The latter accepts only a closed text or
explicit-null input, decoded on the host against the actual trusted storage
type. Long integers never roundtrip through JavaScript numbers; booleans,
empty strings and null remain distinct. Numeric input is not an EDN reader or
evaluation surface. The receipt includes the exact prepared source and its
precomputed analysis, avoiding a fallible post-publication read.

The Markdown editor's Show properties action opens scalar page controls from
that safe inspection model. Controls use exact host-rendered text, never raw
browser numeric values. Per-field drafts live in the existing context-backed
Memory signal. Schema/source observations refresh models but retain entered
values with their original scope/source stamps; stale drafts require explicit
discard, not automatic rebinding. Removed fields remain visible for copying.
Property saves and file moves exclude each other, and typing or newer incoming
file changes observed during publication survive its receipt. Unsaved source
drafts must be resolved before applying a property. Legacy/provisional
workspaces explicitly report unavailability rather than pretending to be
canonical spaces. New typed-space creation, type selection, structured edits
and MCP property commands are not activated by these controls.

## Existing KB vocabulary

Everything stored in a KB or a room store is described by one small vocabulary.
You cannot read the wiki, type or property code without it.

Property definitions preserve an explicit `:morphism/optional? false` (required)
separately from true (optional) and absence (unspecified). The shared morphism
writer does not turn a required declaration into an unspecified one.

Host-side work admission is control-plane data in the system DB, separate from
the speculative KB/room worlds. `model/work_requests.clj` declares typed
`:work.request/*` identities, ownership, limits, lifecycle and job-pointer
attributes. The complete immutable intent uses a Datahike `store-ref`, not an EDN
string or an exploded runtime object. Atomic insert/CAS transitions decide who
may schedule; Dvergr's room-store Jobs, Runs, Attempts and accounting remain the
execution and result records. Fork rollback does not duplicate the host admission
or its conserved resource authority.
The same journal admits `:task`, `:benchmark` and `:allowance` requests.
Benchmark intents pin the client submission, canonical ExperimentDef, resolved
candidate roster, grading policy and aggregate limits. Fixtures use the durable
request UUID as their host-side locator; this is not another result store.
The canonical ExperimentDef also freezes bounded workflow provenance (name,
title and admitted bundle ID). Completed leaderboards use that frozen label,
not current authoring files, and retain the experiment identity for inspection.
Calibrated WorldSetup references include the original bundle-bearing fixture
identity and execution policies. Successful setup/calibration evidence stays in
the existing Attempt evidence, not a separate calibration database.
Opt-in wiki structural scoring derives bounded immutable facts from verified
fixture bytes inside the scoring account. Facts do not persist parser ASTs or
add a second document store. The verifier/setup basis pins the observation and
selection profiles; multi-reference calibration verdicts remain in the same
Attempt setup evidence as single-reference calibration.
Restart recovery retires settled fixtures even if the wallet-return write
completed immediately before a process crash.

Pinned workflow activations and their occurrence outbox also live in the host
system DB (`model/workflow_schedules.clj`), not copied room worlds. Typed
`:schedule.activation/*` rows retain the delegator, room, target revision,
GC-tracked immutable target, enablement, cursor and unresolved occurrence.
The target retains source bytes, literal execution settings, normalized cadence,
host policy and concrete model-metadata digests. At most100 activations are
admitted per host. A serialized writer advances the cursor and inserts one
deterministically identified occurrence together. An unresolved predecessor
records an overlap skip instead of allocating another wallet. Occurrences
record admission facts and canonical request links, not duplicated execution
state. Reads expose twelve recent occurrences; execution uses the active pointer.
Pause atomically withdraws pending work but does not cancel admitted Jobs.
Restart links an existing submitted request or holds an ambiguous admission for
reconciliation; it does not blindly replay it.
Settled occurrences reserve a deterministic room-message UUID. Publication uses
the existing durability-before-visibility room path, followed by a typed
`:schedule.occurrence/announced?` acknowledgement. A lost acknowledgement retries
the same envelope, not the execution. The room note links its canonical Job and
does not wake agents. Its unresolved pointer remains held until publication.

`model/workers.clj` declares the host control-plane vocabulary for separately
enrolled local workers and external assignment ownership. Enrollment records
hold the enrolling party/room, generation, credential verifier hash and a
GC-tracked immutable configuration. Assignment identities are the existing Run
UUIDs and link to the request, Job, selected worker and frozen input artifact.
Writer-serialized claims bind one client nonce and lease proof hash; the same
Run is never reassigned after expiry or cancellation. Plaintext credential and
lease proofs never enter database transaction arguments.
The client nonce UUID is also the lease UUID. Persisting Run, nonce and proof
before claiming therefore retains shutdown authority even if the claim reply
is lost and enrollment is subsequently revoked. Before commitment that proposed
bearer authenticates nobody; after revocation it grants shutdown operations
only. A lost reply never authorizes execution.
Each enrollment represents one physical worker slot: another assignment cannot
be claimed while a previous lease lacks its shutdown acknowledgement, even
after expiry or quarantine.
Host publication withdrawal can install a same-intent withdrawn tombstone
even if enqueue's result was ambiguous. A late enqueue cannot revive it. If
a worker already claimed, the same transaction quarantines that lease without
acknowledging shutdown. Closing-state reconciliation checks the transition,
not merely the existence of a matching intent.

These assignment states are transport receipts, not another execution model.
Expiry/revocation quarantines ownership without inventing physical shutdown;
only withdrawal before a successful claim, or an existing lease's explicit
shutdown acknowledgement, records that boundary. Completed output binds one
immutable digest and the worker's shutdown assertion. This is an enrolled
worker's assertion, not independent proof about a remote process. Immutable
configuration, input and output reads verify their original logical digest;
corrupt payloads are refused rather than substituted from the current workspace.
`ops/worker_credentials.clj` supplies separate enrollment/lease credential
validation and a worker-only authority predicate. It intersects the fixed room
with the enroller's current permissions, keeps only verifier hashes in process
principals, and retains narrowly scoped shutdown signaling after revocation.
`ops/worker-service.clj` implements closed, bounded pending/claim/input/heartbeat/
completion/stop operations; `ops/worker-api.clj` supplies their worker-only MCP
bindings. Application startup registers them through `runtimes/application-ops`
before browser requests are accepted. The separately configured loopback MCP
listener dispatches recognized worker/lease credential prefixes only to the
worker authenticator, never to human/admin authentication. Disabling new
admission does not remove an existing lease's shutdown path. These operations
add no agent sandbox capability.
Claim, heartbeat and completion check current permission and the host clock
inside the serialized writer transaction, including after output publication.

Frozen input contains only explicitly selected regular files (at most 100,
1 MiB each, 2 MiB total), task, pinned adapter configuration and execution
limits. The input JSON is delivered in 16 KiB chunks with a full digest; no
live room file read is substituted. Output carries validated base64, byte
length, SHA256 and regular-file mode. It may change selected inputs or create
`out/` deliverables, but cannot change other room files. HTTP filename keys are
normalized without dropping path components and duplicate normalized names
are refused. Responses omit internal payload references and verifier hashes.
Completion records immutable output and the worker's shutdown acknowledgement;
it does not import files, certify an Attempt or merge into the parent room.

`ops/worker-wait.clj` supplies a host-owned shutdown wait for protocol adapters.
Its database listener and sticky Run cancellation hook register before
assignment publication; both callbacks only offer a bounded wake token. The
owner rereads durable state after each event. The current lease deadline may
trigger cancellation, but quarantine, failed writes and unavailable reads
cannot stand in for a shutdown receipt. A running Dvergr protocol worker stays
physically owned while this wait continues, including after interruption.
Interrupt status is restored only when the owner closes its listener/hook
after acknowledgement and DB cleanup. Returned assignments are private host
data, not portable Run results or import authority: an adapter must construct
sanitized evidence and separately check current authority before importing.

`ops/worker-output.clj` is the trusted import primitive for that isolated Run
world. It refuses parent rooms, validates output against the frozen selected
inputs, and checks an adapter-supplied current-authority predicate at every
file/commit effect and before returning. Content reuses Dvergr's atomic batch;
executable mode changes use a second gated commit under the same workspace
lock. These two commits are not one atomic transaction. Final repository
read-back checks bytes, modes and deletions, including effects intercepted by
world handlers. A failure may leave partial changes in the isolated world,
never trunk, and must fail the Run. The adapter supplies the verified immutable
assignment payload and current canonical Run world; this helper neither
certifies successful task execution nor interprets a nonzero CLI exit code.

`ops/worker-work.clj` connects these host primitives to existing Dvergr Jobs,
isolated Runs and certified Attempts. Only the enrolling human, with current
room read/merge authority, may submit. The immutable request pins worker
generation/configuration, selected files, task and limits. Exact retries return
the original Job even after parent files change or enrollment is revoked;
changed intent conflicts. World setup refuses input drift before publication.

Local execution uses a separate conserved `local-worker-admission` coordinate,
explicitly provisioned by a human room owner. One admission is consumed before
remote publication; a failed publication attempt still consumes it. This is
neither a provider-call count, USD measurement nor proof of CLI startup.
Native money/dispatch balances cannot fund it. Typed
`:work.request/max-worker-admissions` metadata records the allowance/limit.
The shared work adapter accepts trusted prepared resource vectors; external
request schemas cannot inject them.

The Run waits for physical stop, then checks current enrollment/grants,
cancellation, exact live Run/Job and canonical world before each import effect.
Capture uses a fork-local Spindel context marker and rereads actual files/modes,
not the worker's asserted result alone. Nonzero exit fails the Run while
retaining captured diagnostics; completion reward requires a completed Run,
successful import and exit zero. Receipt spend is explicitly unpriced because
remote subscription usage is unreported. Provenance distinguishes local-worker
execution and host versus MCP submission. New human admission is opt-in.
Generic restart recovery excludes both `:local-worker` and
`:local-worker-workflow` and `:local-worker-benchmark`: a dead
local JVM cannot certify that a remote process stopped.

Benchmark assignments optionally add an immutable `:simmis.worker.assignment/cell`
UUID. Their v2 intent digest includes this binding; ordinary v1 digests are
unchanged. The host-only `:prepared` state pins a Run/case/input before server
calibration but is neither pending nor claimable. Only a separate serialized
transition can queue it after admission consumption. Cancellation withdraws
prepared/queued rows with a no-lease acknowledgement; it never invents shutdown
of a leased process or resurrects a withdrawn intent.

`ops/worker-recovery.clj` is a separate host-only reconciliation entry point,
called by startup maintenance and authenticated receipt handling. It validates the frozen request,
complete Job/Run graph and actual Run provenance before closing publication.
Missing/queued assignments get a same-intent withdrawal fence; leased work
holds its resources until physical stop acknowledgement. It can discard an
exact canonical uncertified deferred world through Dvergr's existing fork
capability, or preserve an exactly matched certified review world. Only then
does it return remaining wallets bottom-up and mark the request returned.
Before either world decision it joins the Job's detached evaluation cleanup
group and rereads the durable child/Attempt; terminal Runs do not stand in for
physical completion of capture or certification workers.
Missing runtime world handles remain `:awaiting-world-cleanup`, even when the
Run projection says `:discarded`: that projection precedes physical substrate
cleanup. A crash after discard but before resource return can therefore also
remain unresolved; this adapter does not fabricate a durable cleanup receipt.

`scripts/worker_inputs.py` supplies the local input boundary without starting
an executor or authenticating a client. It checks the pinned configuration,
closed policy, immutable chunk identities/offsets/size and full SHA256, then
each file's size/digest/mode. Duplicate JSON fields, non-JSON constants and
invalid UTF-8 filenames are refused. Materialization creates a fresh private
directory and uses descriptor-relative exclusive/no-follow opens, never an
archive or existing checkout. Runtime control paths such as `.codex/`,
`.claude/`, `.agents/` and `.mcp.json` are not accepted as task data that a CLI
could auto-load. No credentials or unselected files enter that directory.
`scripts/worker_outputs.py` collects results only after its caller has proved
process-tree quiescence. It reads changed/deleted selected inputs and regular
`out/` deliverables, not a recursive snapshot of the workspace. It preserves
binary bytes and executable mode, omits unchanged files, and enforces the same
file/count/total limits plus a bounded directory walk. A private owned root,
no-follow descriptor-relative reads, pre-open special-file checks and
post-open identity checks refuse symlinks, hardlinks, devices/FIFOs and
privileged modes. Collection itself grants no shutdown acknowledgement,
server-side import, certification or merge authority.

`scripts/worker_claim.py` persists the private client claim identity before any
network claim. Atomic no-overwrite publication and file/directory fsync preserve
one Run/nonce/proof across retries; corrupt or mismatched tickets are never
replaced. Its caller must first durably create the private state directory,
including its parent entry. Inspection alone is not the durability barrier.
A per-enrollment OS lock spans admission, process lifetime and cleanup; lock
files must not be removed/recreated while another controller could own them.
Process death releases the lock, not an orphaned process tree or server lease.
Replacement controllers must reconcile those before executing, and retain
ambiguous claim tickets until ownership/shutdown is resolved. Crash-left private
staging files are non-authoritative. This helper performs no network calls or
execution and keeps no shared application state.
The Python MCP transport accepts a proposed proof only as a session header,
starting with initialization, never as a tool argument. Its ordinary read
client is unchanged. The synthetic cross-language HTTP regression exercises
ticket preparation, client-side claim, verified input download/materialization
and bounded completion with binary data, executable modes and deletions; it
does not start a coding CLI or call a model.

## Objects, morphisms, instances

The model is a category, called **S**, stored as ordinary Datahike entities.

- An **object** is a type: `S/Page`, `S/Message`, `S/Block`, and the primitives
  `S/String`, `S/Number`, `S/Float`, `S/Boolean`, `S/Date`.
- A **morphism** is a property: it has a source object, a destination object, a
  cardinality, and a name like `S/Page/title`.
- An **instance** is a page, a message, a block. It points at its type with
  `:instance/of-role`.

So "a page has a title" is a morphism `S/Page/title : S/Page → S/String`, and a
particular page is an entity carrying `:instance/of-role` and a value under the
attribute that morphism names.

## Where a morphism's values live

This is the part that surprises people, and it has exactly one implementation —
`model/morphism.cljc`:

```clojure
(defn attr-of [morphism]
  (or (:morphism/storage-attr morphism)
      (name->attr-ident (:entity/name morphism))))
```

Two cases:

- **Derived.** `"S/Page/title"` → `:S.Page/title`. The attribute is computed
  from the morphism's name.
- **Reflected.** The morphism carries `:morphism/storage-attr`, a categorical
  view onto an attribute a domain system already owns — `:entity/created-at`,
  `:kontor.posting/amount`. The value lives where that system puts it; the
  morphism only describes it.

Reflected is the **majority** on an installed store — including `created-at`
and `updated-at` on `S/Page`, which is every wiki page. Code that derives the
attribute from the name and skips `:morphism/storage-attr` reads blank for most
properties, and does so silently. Always go through `morphism/attr-of`.

## Schema types

A morphism's destination object determines its Datahike `:db/valueType`, via
`schema/codomain->db-type`. The **server** derives this; a client never sends
it. `:db/ident` declarations are append-only, so a client that could declare an
attribute's type could fix it permanently. Every path that declares one — a
type page's `add-property`, a widget's `kb/install-attr!`
(`widget-install-attr`) and an agent's `kb/install-attr!` in its sandbox —
names the codomain object, and the server derives the type and records the
morphism (`attributes/declaration`, for the latter two).

Note `S/Number` is the integer primitive (`:db.type/long`) and `S/Float` is the
decimal one (`:db.type/double`). A UI number field that accepts decimals names
`S/Float`.

The attribute's type also decides what a property VALUE must be. The page
header's property box picks its editor from the attribute's `:db/valueType`
(`page-properties-query` puts it on each property), not from the morphism's
UI hint — katzen's `kind : Entity → Keyword` projects with hint `:text`, and a
text box sends a string. A keyword attribute gets an input that produces a
keyword, and `:S.Page/kind` a select. `save-property` refuses a value that does
not fit the attribute's type, with the reason, before it transacts
(`model/page_kind.cljc`, `value-fits?`).

A wiki page in the atom/overview/index layout records what it cites:
`S/Page/source : Page → String`, cardinality many (`:S.Page/source`, a set of
citation targets — `doc`, `doc#Section` or `dh://…`, e.g.
`transcripts/2026-10-16-1400-planning.md`), and `S/Page/entry-point : Page →
String` (`:S.Page/entry-point`, where a reader of the source starts). Both are
seed morphisms, so every store gets them at its next install. The layout
inside the page is block-level: a claim is a top-level block whose HTML starts
with `<h2>`, evidence one block `<blockquote>"…"</blockquote><p>[[doc#Section]]</p>`,
and a `<strong>Sources:</strong>` block closes it — the shape dvergr's wiki/v4
checker projects to Markdown.

A screen-share frame (the sharer's own screens DB, `runtimes/screen_intake.clj`)
carries exactly one of `:screenshot/transcript` (its description) and
`:screenshot/failed` (why there is none). A screen-share grant
(`:screen-grant/*`, system DB) is one WINDOW: reopening a closed share opens a
new grant entity, and the old one stays with its `:until` — or is closed at its
last heartbeat when its client died — so past windows bound what a room may
read after the meeting.

`:S.Page/kind` holds two sorts of kind. The wiki layout's — `:wiki/atom`,
`:wiki/overview`, `:wiki/index` — are a person's to set. `:bootstrap` (seeded
pages; the timeline hides them) and `:chat-summary` (the summarizer's record
pages; the nav hides them) are the system's: `save-property` refuses to give a
page one of those, or to change or clear one (`page-kind/change-refusal`), and
the property box shows them without an editor.

## One abstract schema, two bindings

A KB and a room store hold the same shapes under different names, and
[katzen](https://github.com/replikativ/katzen) supplies the abstract schema
both are bound to. `model/store.clj` installs the simmis half on every store,
so a KB can hold a book and a room store can hold wiki pages, and neither
acquires an attribute the other lacks by accident.

`model/katzen_projection.clj` projects the abstract schema onto a concrete
store, and records the exact attr-type on the morphism so the distinction
survives the round trip — the two tables disagree in one documented place
(`:Number`), and a test asserts the rest.

## Cross-store references

A reference between stores is a value, never an entity id. Branches share an
eid counter after a fork, so a cross-store ref by eid is a bug that presents as
data loss.

Within a KB, `[[Title]]` links a page. Across databases the explicit form is
`[[dh://…][Display]]`. Links are stored as datoms, so backlinks and
neighbourhood queries are ordinary queries rather than a maintained index.

The same rule governs what a backlink row can be clicked into. A row opens a
tab from a uuid — a page's `:entity/uuid`, a room's `:room/id` — never from a
display name. Room names are not unique, so a row that carries only a name
names no room; `uis/web/desktop/backlink_target.cljc` reports such a row as
unavailable rather than guessing one.

## Labels

A label is a person's verdict on reviewed work (`workflows/review.clj`), one
entity per decision in the ROOM's store, identified by `:entity/uuid`, with
attributes under `:S.Label/*` (`schema/label-schema`, part of `full-schema`):

| attribute | |
|---|---|
| `subject-kind`, `subject-id` | `:proposal` or `:run`, and its uuid |
| `about` | `:workflow` (the work) or `:reviewer` (the reviewer's approval) |
| `verdict` | `:accept`, `:reject` or `:edit`; about a reviewer, `:accept` agrees |
| `review`, `policy`, `reviewer-verdict` | the dvergr Review, its spec's content id, what the reviewer said |
| `workflow-id`, `workflow-version` | when known |
| `edit` | the diff a person applied before accepting (verdict `:edit`) |
| `by`, `reasons`, `at` | who decided, why, when |

Labels are append-only: nothing updates or retracts one. `review/labels`
reads them back as the `:label/*` maps of the workflows contract
(`{:label/subject [:proposal id] :label/about … :label/workflow {:id
:version} …}`). There is no `S/Label` object in category S yet; like
`S.EvalEntry`, the attributes are simmis's own.

## Ordering

Blocks are ordered by `:block/order`, a fractional index — a string chosen to
sort lexicographically between its neighbours, so inserting never renumbers
anything else. `model/fractional_index.cljc` generates them. The alphabet is
printable ASCII, not the base-62 of the reference implementation the algorithm
came from.
