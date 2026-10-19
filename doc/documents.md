# Document storage prototype

`is.simm.documents.markdown`, `is.simm.documents.repository` and
`is.simm.documents.view` are an isolated
prototype. They are **not** connected to live KBs, room tools, RPC, user
provisioning or the document UI. Existing wikis still use their current
pages/HTML-block model. No existing store is converted by booting this code.

## Lossless source

The Markdown layer keeps original UTF-8 source, including BOM, comments,
frontmatter spelling, line endings and unknown syntax. Flexmark supplies a
disposable syntax tree with UTF-16 source spans, matching browser editor
coordinates. It does not serialize the tree back into source or execute code.

Frontmatter is parsed with safe YAML, duplicate-key rejection and bounded
depth/aliases. Malformed or unsupported metadata remains intact, with a short
diagnostic instead of a guessed repair. This is raw metadata, not yet a codec
for Simmis's programmable property schema. Syntax projection and source sizes
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
stable semantic-block identity or Simmis grouping metadata. Those constructs
remain in the original source; recognizing syntax is not a claim that the
semantic codec or rich editor can round-trip every feature.

`patch` accepts a content revision and non-overlapping edits in that revision's
coordinates. A stale revision, overlapping edits or a range splitting a UTF-16
surrogate pair is refused. Untouched source is preserved. A no-op edit does not
rewrite the file.

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
exact Markdown paths/content IDs/sizes/modes, syntax and projection versions,
and a **trusted host-supplied** schema revision. It selects the worktree, not
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
Typed projection must verify its schema revision against captured trusted
schema facts; the prototype only requires a trusted caller-supplied token.

This read barrier supersedes the idea that every Geschichte operation must
atomically rewrite application projections. The colocated write proof above
remains a useful optimization, not a universal publication requirement.

Document UUID preservation in these tests is explicit; it is not automatic
identity recovery after an external file move. Syntax-node eids are disposable
and are not stable wiki-block identities. Legacy block migration, portable
schema/widget declarations and existing KB property integration are not
implemented here. There is no new independent authorization boundary: callers
must own/authorize the connection, and no remote caller can invoke this API.

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
