# Document storage prototype

`is.simm.documents.markdown`, `blocks`, `properties`, `repository` and `view`
provide the source-backed Memory model. The file/editor UI and authenticated
room tools read room repositories through their owned-target boundary. New
canonical knowledge rooms can be prepared explicitly by the host; existing
independent KBs retain their pages/HTML-block model. No existing store is
converted by booting this code.

## Lossless source

The pure `property-source` codec replaces existing scalar leaves, inserts
missing mapping paths and changes a selected explicit type sequence without
reserializing the surrounding frontmatter. Inserted values use bounded flow
syntax; existing unknown fields, comments, BOM, line endings and the Markdown
body remain intact. Each patch reparses and verifies the exact intended
metadata change and unchanged body. Anchors, aliases, explicitly tagged type
items and sequences whose replacement would consume comments require Source
editing instead. Aggregate emitted UTF-8 bytes, nesting and node counts are
bounded before constructing a replacement document. These are pure source
operations, not authority, schema installation or a UI type-assignment API.

The HOST preparation model also previews and prepares a closed type-assignment
batch against existing trusted definitions. It stamps both the original scope
and the prospective definitions, accepts only new alias/value bindings, and
validates required values and relation UUIDs against the complete prospective
selection. It refuses orphaned bindings and does not reinterpret unknown
metadata automatically. Preview DTOs distinguish new supported inputs from
unbound or owner-managed properties. Preparation is read-only: publication
still requires the native writer's fresh validated basis and current room
authority. Arbitrary definition/schema datoms are not assumed to travel with
Geschichte file proposals. Memory's **Edit types and fields** control selects
existing definitions and adds explicit source bindings and values. Whole-page
and explicitly identified block scopes remain separate; Outline and the
Property scope selector open block properties. Assignment drafts are
context-owned and retain their inputs across scope, source and definition
changes. In-flight file/property saves and moves exclude assignment admission
and cancellation; stale/hidden assignments remain inspectable and cancellable
after those operations settle. Authenticated Memory declaration preview/save RPCs
use the same owned-target stamps, current room permissions, blocking-worker
boundary and atomic native validated-basis writer as scalar property edits.
The type picker descriptor is bounded before metadata pulls and contains only
trusted type UUIDs and names, not executable definitions or capabilities.

The Markdown layer keeps original UTF-8 source, including BOM, comments,
frontmatter spelling, line endings and unknown syntax. Flexmark supplies a
disposable syntax tree with UTF-16 source spans, matching browser editor
coordinates. It does not serialize the tree back into source or execute code.

Frontmatter is parsed with safe YAML, duplicate-key rejection and bounded
depth/aliases. Malformed or unsupported metadata remains intact, with a short
diagnostic instead of a guessed repair. Typed projection requires explicit
trusted schema bindings as described below. Syntax projection and source sizes
are bounded; exceeding a limit is an explicit refusal.

The syntax profile pins Flexmark 0.64.8 with explicit table, task-list,
strikethrough, footnote and wikilink/embed extensions. It preserves heading
levels, task state and table-cell alignment. `references` extracts body
wikilinks/embeds only from parsed syntax nodes, keeping original target spelling,
aliases, heading/block fragments, source revision and exact UTF-16 spans.
References remain unresolved: no title/path guessing, permission decisions or
automatic network fetch occurs here. Literal code, escaped examples and HTML
comments do not become backlinks. Frontmatter relation values need a separate
schema-aware codec; they are not treated as body references.

This profile does not yet implement complete Obsidian callout/math semantics,
arbitrary legacy semantic-block identity or Simmis widget metadata. The separate
grouping codec below provides explicit identities for new files. Recognizing syntax is not a claim that the
semantic codec or rich editor can round-trip every feature.

The synthetic academic source corpus exercises named footnotes, dollar-delimited
formula source, Pandoc-style citation spelling, a local bibliography filename,
and nested Clojure fences, including literal lookalikes in code. Current tests
establish exact no-op preservation and unchanged academic source when a neighboring
heading is edited, for both LF and CRLF input. This does not claim formula
rendering, citation resolution, bibliography loading, or runnable Markdown cells.
Footnotes are syntax nodes; their containing rich paragraph can still require
source editing. Parsing and rich editing do not fetch references or execute code.

`patch` accepts a content revision and non-overlapping edits in that revision's
coordinates. A stale revision, overlapping edits or a range splitting a UTF-16
surrogate pair is refused. Untouched source is preserved. A no-op edit does not
rewrite the file.

## Semantic blocks and selective region edits

The experimental grouping codec distinguishes semantic Simmis blocks from
Markdown syntax nodes and a heading outline. A block can contain several
paragraphs and an internal rich-text list, plus separately identified children:

```markdown
<!-- simmis:block 00000000-0000-4000-8000-000000000001 -->

Two paragraphs or a list can belong to this one block.

<!-- simmis:block 00000000-0000-4000-8000-000000000002 -->

A separately identified child.

<!-- /simmis:block -->

More parent content.

<!-- /simmis:block -->
```

These are reserved, paired, top-level Markdown comment nodes, not regex matches
over arbitrary source. Literal examples in code remain inert. Invalid,
unclosed, misplaced or duplicate markers produce diagnostics rather than
automatic repair. Semantic nesting is bounded to 100 levels. Unmarked Markdown
still projects as ordinary blocks with **provisional revision/position IDs**;
opening it does not insert markers or promise stable identity across edits.

Each explicit group has source regions excluding its complete child ranges.
`blocks/replace-region` checks the expected source revision, preserves child
bytes and rejects changes to the explicit identity tree or document frontmatter.
Opening a code fence that hides a child is therefore refused, not accepted
because the child's original bytes happen to remain present. Full-source edits
remain a separate operation that can intentionally change structure/metadata.

The immutable view exposes source-spanned `:document.block/*` entities with
UUID, identity status, ordinal and child refs. They are separate from disposable
`:document.node/*` syntax entities. Document and block IDs are checked across
the selected files before building a queryable DB. This is not migration into
existing `:entity/uuid` HTML blocks, complete Obsidian anchor reconciliation or
the full codec for block properties/grouping widgets.

## Schema-bound document properties

`properties/compile-schema` accepts host-supplied bindings from portable YAML
names to existing categorical morphism UUIDs. It uses `morphism/attr-of` and
the actual captured Datahike attribute schema, and hashes relevant morphism
facts into the selection descriptor. Changes to ownership/schema invalidate
the view even if files and a caller's schema-version label remain unchanged.
Files cannot supply these bindings, install schema or execute computations.

Supported ordinary values are strings, booleans, bounded longs, finite doubles,
canonical UUID strings and UTC-offset-qualified instants, with explicit one/many
cardinality. Numbers/booleans are not guessed from strings. Explicit required
properties are checked; many-valued fields are bounded to 1,000 items. Raw and
unknown metadata remain intact. Successful values become typed facts on the
derived document entity, queryable alongside syntax and semantic structure;
the persistent source DB is not modified.

Reflected domain properties and formulas are owner-managed, not file-authored
facts. Component, unique, reference and programmable-validator properties are
also refused until their corresponding ownership/resolution/checker path is
implemented; uniqueness must never silently upsert unrelated documents. System
source/projection/identity/schema/authority namespaces cannot be targeted by an
apparent ordinary morphism. User-defined categories are not limited to `S`.
Diagnostics identify the property/reason without copying its value into logs.

## Colocated storage proof

An explicitly initialized disposable Geschichte database can also hold
`:document/*` and `:document.node/*` entities. Its document UUID is independent
of its path. Source bytes are Geschichte content; syntax nodes reference
source spans rather than holding another serialized document.

Syntax children have explicit ordinal attributes as well as source spans,
since Datahike cardinality-many references are unordered. These ordinals are
derived syntax order, not semantic block fractional order or stable identity.

`content/transact-content!` publishes the source's worktree reference,
document revision and syntax projection in one Datahike transaction. A pure
transaction function checks the expected document basis and destination, so
competing writers cannot combine one source with another projection. The
function receives no document body/frontmatter text, since failed transactions
can be logged by the underlying writer. File payload publication uses
Geschichte's store-ref GC guard.

Moves retain document UUID and source revision and refuse occupied paths.
Paths are validated, not silently normalized. Reading uses one immutable DB
view and checks that the worktree content agrees with the projection's source.
An out-of-band file edit causes an explicit stale-projection error.

## Boundaries demonstrated by the tests

Tests cover byte-preserving parsing/patching, typed YAML values, nested syntax,
safe metadata failures, no-op writes, competing edits, source/projection atomic
visibility, moves, destination collisions, retained history and byte export /
reimport with an explicitly retained document UUID.

`test/is/simm/documents/source_fixtures.cljc` supplies synthetic business,
research, nested-structure, literal-reference, legacy-HTML and inert-widget
documents. Source tests exercise LF/CRLF/CR and BOM variants. Each fixture also
names required semantic capabilities as an acceptance checklist; these are
not assertions that the current parser implements the complete dialect.

A raw Geschichte checkout restores repository files, **not arbitrary
application projection datoms**. A regression test demonstrates that this
produces an explicitly refused stale projection. The prototype therefore does
not register the store as a live Yggdrasil system or claim checkout/merge
integration. Colocation alone does not make those operations correct.

## Source-selected immutable views

`view/prepare` derives an immutable Datahike value from one captured source DB;
it does not transact into the source connection or create another registered
repository. The descriptor includes repository identity, selected folder's
exact Markdown paths/content IDs/sizes/modes, syntax/block/projection versions,
captured property-schema digest and a **trusted host-supplied** broader schema
revision. It selects the worktree, not
just HEAD: uncommitted edits matter. Projection writes and unrelated files do
not change this descriptor.

`view/current-view` rechecks that descriptor after preparing the view. If the
source changes during preparation it returns `:pending`, without a stale DB
masquerading as current. Each ready result remains explicitly as-of its
descriptor, not a promise to lock future writes. Recheck it for later current
reads and write/review/merge decisions. Malformed documents and duplicate IDs
return `:invalid` with diagnostics, not a queryable partial/empty success.

Raw writes, removes, moves and checkout therefore need no global projection
callback: they change the source selection. Tests exercise these operations,
an injected race during content reads, and independent historical DB values.
Import/reset/merge publication, restart and GC lifetime still need integration
acceptance tests before production registration.

A canonical `simmis-id` UUID in YAML frontmatter supplies explicit document
identity. Missing IDs are marked **provisional**, deterministically scoped to
repository/path, and are not promised to survive external renames. Invalid or
duplicate IDs are refused, never silently upserted into one document. This is
not yet the semantic block/property identity codec.

Selection is prefix-indexed and bounded to 200 entries, 2 MiB per source,
8 MiB total source and 100,000 syntax nodes, stopping between files. Only
regular non-executable Markdown is accepted; symlinks are not followed. There
is no persistent/global projection cache. The caller must retain the source
store and referenced content lifetime; keeping a DB value is not a claim of
a durable GC pin. Views are borrowed in-process query values: do not persist
their IDs as reconstructible retained snapshots or publish their DB as a new
connection. Solving that lifecycle and current authorization remains a
gate to live KB integration. A derived DB also is **not** a data-redaction or
permission boundary: it inherits the authorized captured repository DB.

Existing disposable projection rows may be replaced only with compatible
schema and exclusive ownership: authored attributes or incoming references
from non-projection entities cause refusal. Orphan syntax nodes and unexpected
component ownership are also refused. Unrelated authored schema/property
facts remain queryable. This deliberately refuses mixed legacy layouts rather
than silently deleting references or claiming a block migration.

The descriptor currently covers Markdown syntax inputs only. Attachments,
templates, schema declarations and link resolution outside the selected folder
must join the versioned inputs before they can influence a semantic view.
Typed bound properties verify actual captured morphism/attribute facts. The
broader semantic/schema token remains host-supplied; it is not proof of full
schema-manifest/widget support. `prepare` accepts the authorized source DB,
not a previously derived view as a writable or re-preparable source.

This read barrier supersedes the idea that every Geschichte operation must
atomically rewrite application projections. The colocated write proof above
remains a useful optimization, not a universal publication requirement.

Document UUID preservation in these tests is explicit; it is not automatic
identity recovery after an external file move. Syntax-node eids are disposable
and are not stable wiki-block identities. Legacy block migration, portable
schema/widget declarations and existing KB property integration are not
implemented here. There is no new independent authorization boundary: callers
must own/authorize the connection, and no remote caller can invoke this API.

## Explicit governed knowledge repositories

`documents.profile/policies` constructs a trusted host lifecycle map over an
already acquired immutable starter bundle for `:simmis/canonical-knowledge-v1`,
not a policy or source bundle supplied by room content. Its
initializer requires a pristine unpublished store, installs the existing
shared Simmis/accounting model and Dvergr chat schema, and creates a small
Markdown-backed wiki plus an ordinary code file. Stable document/block UUIDs
and explicit property/type bindings are in Markdown; definitions are the real
governed categorical entities, not copied YAML definitions. The complete starter
projection is validated and the whole file tree committed before publication.
Sandbox source libraries and guidance are packaged from one exact upstream
commit as a namespaced inert EDN resource, not a runtime dependency exposing
`user.clj`. The offline packaging script reads Git objects at that commit, not
live checkout files. Loading checks a reviewed manifest digest, individual
hashes/modes/lengths, strict UTF-8 and finite byte/file bounds. The host acquires
this bundle before native Room creation intent; no Git clone/network request
or source execution happens inside initialization.

Code and license bytes remain unchanged. Bundled Markdown receives
repository-scoped UUIDs and explicit modification notices; installed
`AGENTS.md` and `README.md` prominently correct the upstream auto-load claim.
Original and installed hashes are retained in `code/starter-provenance.edn`.
Pre-existing template frontmatter is refused rather than silently replaced.
Custom host bundles own their provenance claims: self-consistent content hashes
alone do not attest that arbitrary supplied bytes came from the claimed commit.
Markdown outside `wiki/` is also included in the same canonical selection used
by guarded property saves; the folder is navigation, not a semantic boundary.

Reopening validates the actual mandatory schema shapes and repository singleton
with its matching profile, then reattaches accounting and Attempt governance.
It performs no seed/schema/source transaction and starts no asynchronous worker.
An invalid authored Markdown file withholds the derived canonical query view,
not the whole Room: raw source remains repairable. An invalid mandatory schema
or missing/mismatched profile instead refuses repository preparation.

This host policy does not classify, migrate or replace an existing wiki. It is
not enabled merely by opening a file, and is not a new unauthenticated room
creation API. Portable definition export/import is distinct from preserving
Markdown values and identities; the policy does not claim an arbitrary Git clone
alone recreates the categorical type definitions.

`model.knowledge-rooms/create!` is an explicit HOST-only project creation path,
separate from legacy chat room creation. It validates member identities and
acquires the packaged source before durable intent. After native preparation,
it installs Simmis's messages schema on the exact messages connection and
registers the prepared live Room without reprovisioning. Only then does one
guarded control transaction publish creator, membership and content sync scope.
The transaction rechecks the exact ready Room metadata and three owned systems,
current identities and absence of an archive or prior membership publication.
The sync scope remains the messages store; repository source is not silently
substituted for the existing conversation replica.

Failure preserves prepared resources and reports the known Room UUID rather
than deleting stores or retrying creation. A lost final acknowledgement may
already have committed membership. Startup supplies the fixed trusted profile
policy when hydrating these rooms. Live Room lookup consumes only an already
prepared context and respects the native recovery gate, even if an uncertain
constructor left a registry entry behind. This does not enable a new transport
endpoint or convert existing rooms.

## Generic source editor

The Workflows panel uses `uis/web/desktop/source_editor.cljs`, a controlled
CodeMirror view with Markdown, Clojure/EDN, JSON and plain-text modes. Saving
still uses the existing workflow source/revision APIs; opening a source file
does not execute it. This is not yet a rich editor for Markdown-backed wikis.

Draft text and selection belong to the caller's Spindel signal state. A native
custom element owns its editor handle and releases it on disconnect; there is
no global editor registry. The controlled owner includes session/room/file and
execution-context fork identity. Owner changes recreate the native view, and
authoritative source replacements reset native undo history. Local edits that
are echoed back through controlled props keep their history. Undo history is
ephemeral device state, not serialized simulation state; draft source/selection
can be restored without sharing a native handle or another context's history.

## Place in the knowledge-work campaign

The next gates are ordered to make memory work an ordinary workflow rather
than a second special-purpose subsystem:

1. Define and test the semantic codec: page/block UUIDs, nested grouping,
   references, typed/programmed properties and inert widget declarations.
   Syntax nodes and heading outlines are not substitutes for those blocks.
2. Integrate the source-selected read barrier with one existing repository
   owner, including merge/import, source retention and permission revocation.
   Derived facts are rebuilt from final source, not independently merged.
3. Add a small rich-editor slice alongside Source and Read, then Outline and
   properties where supported. Edit a supported source region without
   reserializing unrelated/unknown Markdown. Keep native handles context-local.
4. Install memory maintenance as an editable, versioned workflow with the same
   manual execution, benchmark and durable schedule path as ad-hoc workflows.
   Use bounded budgets, durable source watermarks, source-linked proposals and
   scoped revocable merge policy. No-change runs should need no inference.
5. Exercise the complete loop locally: source change -> scheduled workflow ->
   cited memory proposal -> checked adoption -> query/UI -> retained benchmark.
   Then land reviewed releases/CI and deploy the verified slice.

Office work is a bounded parallel track, not a prerequisite for Markdown.
Original DOCX/XLSX/PPTX binaries remain authoritative. The existing SCI ZIP/XML
API can support programmatic edits without a visual editor, but well-formed
XML alone does not establish valid relationships, layout or document fidelity.
Before expanding it, make staging execution-context-owned, enforce handle
scope and revision/permission checks, and test that untouched archive members
survive edits. Evaluate an optional human editor adapter against preservation,
visual fidelity, licensing and deployment cost; do not silently convert Office
files into Markdown or deploy a full office service as part of this prototype.

Meeting/screen transcription, broader learning loops, mobile clients and
simulation remain on the overarching agenda. They consume these same
source/proposal/workflow interfaces rather than blocking this milestone.
