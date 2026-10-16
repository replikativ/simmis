# The data model

Everything stored in a KB or a room store is described by one small vocabulary.
You cannot read the wiki, type or property code without it.

Host-side work admission is control-plane data in the system DB, separate from
the speculative KB/room worlds. `model/work_requests.clj` declares typed
`:work.request/*` identities, ownership, limits, lifecycle and job-pointer
attributes. The complete immutable intent uses a Datahike `store-ref`, not an EDN
string or an exploded runtime object. Atomic insert/CAS transitions decide who
may schedule; Dvergr's room-store Jobs, Runs, Attempts and accounting remain the
execution and result records. Fork rollback does not duplicate the host admission
or its conserved resource authority.

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
