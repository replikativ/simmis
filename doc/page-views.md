# Saved page tables and forms

A Markdown document can contain a `simmis-view` fence describing a table of
source-backed pages. Opening its associated view reads the saved declaration;
the fence itself remains ordinary portable Markdown. The first slice supports
page tables and a form for one explicitly selected page.

```simmis-view
{:version 1
 :id #uuid "270a88f9-e6da-4501-bdb1-86f3743bd224"
 :source {:kind :pages :roots ["companies"]}
 :presentation :table
 :columns [{:kind :title :label "Company"}]
 :order [{:kind :title :direction :asc}]
 :limit 50}
```

Add `:types [#uuid "..."]` to `:source` to require page types. Multiple types
mean all must be present. A property column is `{:property #uuid "..."}` with an
optional `:label`. These UUIDs identify existing type/property definitions;
they are not native database entity IDs. Ordinary source-owned properties are
supported; computed, reflected and component properties are excluded.

A form uses `:presentation :form`, `:limit 1` and `:page #uuid "..."` within
`:source`. Its page must belong to the selected folders and types. A missing
page produces an unavailable result rather than an apparently successful empty
form. Ordering currently supports the title in ascending or descending order,
with page UUID as a deterministic tie breaker.

## Reading and editing

The associated panel is outside the document's editable surface. A table's Edit
button loads the selected page's ordinary property form. Each field save uses
the existing revision-checked Markdown property writer, preserving document and
block identities. There is no multi-file transaction implied by editing several
rows. The page's Markdown remains authoritative.

Read permission is sufficient to inspect a view. Editing additionally requires
current write permission and current file/property definitions. A dirty source,
property or type draft in another tab, or a pending write there, prevents a
competing property write. Drafts are kept when a row moves or leaves a result;
the interface explains why editing is held.

The client refreshes from its existing metadata signals. A compatible result can
remain visible while it updates; replies for an earlier target, saved declaration
or metadata generation are ignored. Hiding and reopening a view obtains a fresh
read. A failed read offers an explicit retry rather than repeatedly issuing work
on every render.

## Boundaries and limits

Descriptors are inert bounded EDN, not executable queries. They cannot supply
functions, renderers, permissions or write commands. The server resolves the
exact saved file revision, fence span and view identity, enters the original
application/room lifetime, and projects one immutable authorized source. It
rechecks that room and source after native reads before returning portable facts.
No derived database or native entity handles cross into the response.

At most four declarations per document are mounted, each at most 16 KiB, with
16 columns and 50 rows. The existing canonical projection bounds also apply:
200 Markdown files, 8 MiB of source and 20,000 worktree entries. Invalid selected
source produces an unavailable result, not a table built from partial data.
Narrowing folders can reduce read scope; property publication still uses its
existing canonical validation scope.

Cell previews are bounded independently of result completeness. Numeric values
are transported as text to preserve integer precision. Reference labels and
paths are exposed only for source-backed pages inside the selected projection;
other valid reference identities remain UUIDs. The view never expands arbitrary
native entity labels. These are preview limits, not changes to stored values.

The current frontend receives bounded file/projection reads rather than a full
canonical database replica. Future query adapters, benchmark/media sources and
simulation components should reuse these source, lifetime and property-writer
boundaries. Arbitrary SCI evaluation on page opening is outside this contract.
