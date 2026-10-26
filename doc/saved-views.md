# Saved Markdown views

A Markdown document can contain a `simmis-view` fence describing a table of
source-backed pages. Opening its associated view reads the saved declaration;
the fence itself remains ordinary portable Markdown. Fixed sources support page tables/forms and saved benchmark Scorecards.

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
16 columns and at most 50 page rows. For page sources the canonical projection bounds also apply:
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
canonical database replica. Future query adapters, media sources and
simulation components should reuse these source, lifetime and property-writer
boundaries. Arbitrary SCI evaluation on page opening is outside this contract.

## Saved benchmark results

The same fence can select one exact native Scorecard in the current Room:

```simmis-view
{:version 1
 :id #uuid "fc4ca343-c926-4050-8010-1b823654885a"
 :source {:kind :scorecard :room :current
          :scorecard #uuid "67b31691-aa8e-40d5-85d4-26661da87f4c"}
 :presentation :table
 :columns [{:kind :candidate} {:kind :attempts} {:kind :pass-rate}
           {:kind :pass-rate-interval} {:kind :cost} {:kind :notional-cost}]
 :limit 32}
```

Replace the Scorecard UUID with a completed result's content identity. This is
an immutable result selection, not a request to run or fund a benchmark. A UUID
never grants access, joins another Room or selects another store. The server
resolves the native store from the authorized Room; the browser matches its
reply against the Room's store identity in its existing membership roster.
The Markdown repository and the receipt store are separate sources.

Simmis workflow benchmarks keep records in their author/control Room. A
standalone Dvergr experiment may have a separate result Room; join that Room
through ordinary membership before viewing its records there. A result absent
from the current Room is unavailable, with no cross-Room search or fallback.

The fixed source reads a managed payload through Dvergr's bounded reader and
retains native certification and summary semantics. It does not load Attempt
receipts, resolve current workflow files, execute a query, or expose tools.
Its source does not scan the canonical page projection, so unrelated archived
Markdown files do not consume the page projection allowance for this report.
Read permission suffices; benchmark rows have no page property/edit bindings.

Supported column kinds are `:candidate`, `:attempts`, `:passed`, `:pass-rate`,
`:pass-rate-interval`, `:reward`, `:reward-interval`, `:cost`, `:notional-cost`,
`:cost-per-pass`, `:notional-cost-per-pass`, `:cost-known`,
`:notional-cost-known`, and `:usage-reported`. Optional labels are plain text.
Candidate order is the native leaderboard order. The display limit can show a
prefix without changing the complete matrix or recomputing any statistic.

Model cost is measured reported usage, not wallet consumption, outstanding
grants, forfeits, or a settlement certificate. Unknown cost displays a dash,
including unfunded runs with no priced usage. Known notional cost remains
visible independently. Native cost-per-pass is displayed without redefining
it. Recurring fractional currency values are marked approximate and rounded to
12 decimal USD places; integer microdollars and finite decimals retain their
exact decimal conversion. Extreme decimal scales are refused before expansion.

Native read ceilings are 4 MiB, 32 candidates, 4,096 matrix cells, 128
environments and 128 repetitions. The portable report has a separate 128 KiB
presentation ceiling. A complete certified matrix does not assert that its
parent job's accounting/adoption settlement has completed. Timing, nested model
attribution and pairwise tables require separate bounded adapters later.
