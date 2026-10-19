# Document storage prototype

`is.simm.documents.markdown` and `is.simm.documents.repository` are an isolated
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
