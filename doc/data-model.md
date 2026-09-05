# The data model

Everything stored in a KB or a room store is described by one small vocabulary.
You cannot read the wiki, type or property code without it.

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
attribute's type could fix it permanently.

Note `S/Number` is the integer primitive (`:db.type/long`) and `S/Float` is the
decimal one (`:db.type/double`). A UI number field that accepts decimals names
`S/Float`.

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

## Ordering

Blocks are ordered by `:block/order`, a fractional index — a string chosen to
sort lexicographically between its neighbours, so inserting never renumbers
anything else. `model/fractional_index.cljc` generates them. The alphabet is
printable ASCII, not the base-62 of the reference implementation the algorithm
came from.
