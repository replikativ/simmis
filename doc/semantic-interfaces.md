# Semantic interfaces

Design direction for the next integration slice. This describes contracts to
converge on. The first implementation boundary is described below; this does
not introduce a generic component registry or a new metadata schema.

Simmis exposes one programming model through documents, forms, collections,
rooms and custom interfaces. Markdown and SCI are authoring representations.
Standard components make their shared semantics predictable for people and
agents. Presentation can vary without redefining an entity or its operations.

## Existing foundations and gaps

- The categorical schema records property domain/codomain, cardinality,
  optionality, storage type, validation and UI rendering hints.
- Source-backed page/block property models derive portable field facts from
  trusted definitions and exact saved source. Page forms and saved page views
  already share property controls and the revision-checked source writer.
- Saved page, Scorecard and capture views share table presentation. Scorecard
  and capture rows have different sources and remain read-only.
- The older type editor still selects controls separately. Its raw value,
  storage type and rendering-hint handling must not become another definition
  of property semantics.
- Legacy SCI widgets query an effective KB and expose KB transaction helpers.
  These are not the canonical Markdown property writer. Reusing widgets for
  canonical documents requires an explicit source/action adapter.
- Room apps currently use an isolated authenticated HTML serving path. They do
  not yet share an interactive entity/form component contract with the app.

## Shared contracts

### Entity and property meaning

An entity keeps its identity across page, table, form and custom interface.
A property declares its domain, codomain, cardinality, optionality and validation
independently of its visual control. Storage type takes precedence over a UI
hint when decoding input. Decimal input must not pass through an imprecise
browser number representation before validation.

A component receives a bounded projection with explicit source identity and
revision/basis. Entity/property identity alone does not select writable state:
operations also bind the source/world and expected basis. The same UUID in two
forks must not imply one shared mutable entity. An editability hint describes
the current read; it is not a write capability. Concurrent source, schema,
target or permission changes are
handled by the existing command/writer boundary.

Unsupported property edits stay explicit. A custom style cannot convert an
unsupported structured edit into a scalar write, or turn a computed field into
an ordinary writable property. Query rows require an explicit direct-property
edit binding; aggregates, formulas and ambiguous joins remain read-only unless
a separately named operation defines their edit meaning. Property inheritance is a semantic policy, not
an accidental consequence of visual nesting.

### Views and editors

Start from the existing small set: value/field display, field editing, entity
form/card, collection table, navigation item and content pane. Extract and
compose those implementations before adding another framework.

Use explicit named component choices with documented defaults. Resolve a
specific requested choice, a type/property preference, or the standard
fallback. An unknown choice uses a contract-compatible default, a read-only
presentation or an explicit unavailable state; it never silently reinterprets
structured input.
Inert saved-view declarations continue to select bounded source adapters;
they do not acquire arbitrary renderer callbacks or executable commands.

Allow labels, field order, density, layout and scoped theme/style choices to
vary. A presentation preference does not alter property identity, validation,
permissions, the selected source or accounting interpretation. Agents should
compose the standard components first and supply a new component when the
interaction needs one. A custom component declares which contracts it supports
and is checked against the corresponding acceptance cases.

Data acquisition belongs to the source/model adapter. Renderers consume the
existing Spindel signals and coordination model instead of creating their own
fetch, cache or subscription systems. Share ready, pending and unavailable
states, source/basis identity and explicit completeness. Authorized asynchronous
frontend Datahike queries and bounded host reads can both supply that model.
Missing or pending data must not look like an empty successful selection.

### Commands

Editors emit semantic intents using the property's/entity's stable identity.
A source adapter applies them to canonical Markdown or the appropriate native
model. Different backing stores may have different writers; the meaning and
validation of the command must be stated rather than guessed by the renderer.

The host derives caller identity, current authority and operation lifetime.
Native App/Room handles, connections and lifetime tickets stay host-private.
Component data or code cannot grant itself authority by submitting an actor,
store selector, editability flag or renderer name.

For source-backed properties, preserve the existing source-span patching,
expected revision/schema checks, retained drafts and conflict behavior. Do not
route a new UI through legacy raw KB writes merely because it looks similar.

### Content mounting and state

Markdown pages, saved views and explicit SCI components share a content target
and mounting model. A target can occupy a tab/pane or be embedded in a page.
Its mode and available operations are independent of where it is mounted.
A full-page SCI interface does not require a second application framework.

Keep entity identity separate from component-instance identity. Mounting the
same entity twice must not accidentally alias focus, DOM element IDs or
asynchronous ownership. Mounts may explicitly share an editing session through
the existing property/write coordinator, including its drafts and status.
Interaction state stays in its owning Spindel execution context;
copying/forking that context must follow Spindel's semantics. Avoid closures over
mutable state outside that context that entangle otherwise independent forks.

Persist intentional content, named views and navigation configuration. Treat
focus, selection, loading and transient filters as context-owned interaction
state unless the user explicitly saves a workspace/view preference.

Opening downloaded Markdown does not itself authorize arbitrary SCI evaluation
or effects. Execution follows an explicit or previously authorized policy.
Mounted components own and release subscriptions, listeners and foreign
resources through the existing lifecycle. A later canvas or WebGL component
must participate in that lifecycle rather than introduce its own global state.

### Room websites and publication

The same display components should support a supplied data snapshot, while a
connected editor additionally needs an authorized action adapter. A static
export cannot perform a live edit without such a connection.

An authenticated private Room app and a publicly published site may use the
same components with different data projections and available commands. Public
publication specifies the exposed fields and operations; it does not expose
the private entity graph or inherit the Room editor's identity. The current
isolated room-app route needs an explicit integration path before shared live
editing can be claimed. Do not weaken its isolation as a styling shortcut.

### Dynamic Room-site handlers (planned)

Dynamic sites are a later extension of the same source/action adapter contract.
The existing Simmis HTTP server can own a fixed dispatch boundary for deployed
Room endpoints; agent code need not mutate the global Ring router or receive
http-kit server handles. A versioned Room-owned manifest can declare methods,
paths, input/output shapes and operation names, with restricted SCI functions
providing the implementation. This is a design direction, not a deployed API.

Resolve the actual Room and deployment once per request. Bind the request to
that deployment's code revision, world/source and host-owned execution context.
Handle cancellation, revocation and Room/App retirement through the existing
operation lifecycle. Keep request-local evaluation state separate from durable
Room state; publication must not introduce mutable closure state that entangles
forks. Code changes may use the existing fork/proposal/merge loop. Whether
public deployments follow the worktree or an explicit published revision must
be decided before implementation; never silently mix those semantics.

Datahike hardening contributes to the data boundary but does not establish
safe public handler execution by itself. Host adapters must restrict data,
queries and commands to the published projection and operation policy, apply
ReBAC/delegation rules, and keep raw host connections, server handles, filesystem,
network and unrelated agent tools unavailable unless deliberately granted.
Public visitors do not inherit the publishing agent's identity or authority.
A declared editable field remains only a UI affordance, not permission.

Bound request bodies, returned projections, query/evaluation work and concurrent
admissions using mechanisms whose enforcement is verified. HTTP admission must
not let untrusted work exhaust shared request workers. Long simulations, model
calls and workflows should use the existing asynchronous Run machinery and
return a job/result reference, with an explicit resource policy. Ordinary reads
must not implicitly start a model call. Reuse revision-checked semantic writes;
retryable effectful commands additionally need explicit idempotency semantics.

Preserve private app script isolation. Browser access from a sandboxed iframe,
authenticated site or public origin needs a deliberate credential/CSRF/CORS
contract; do not enable ambient Simmis credentials or weaken CSP merely to make
an embedded component work. Keep public and private data projections explicit.

The initial dynamic slice should expose one bounded read and one typed command
for an existing business component. The same form then mounts inside Simmis or
on a Room site with a different adapter, while retaining entity/property IDs,
validation, conflict handling and display semantics. Test cross-Room access,
revocation, stale deployments, request retries and fork isolation before
expanding to arbitrary agent-authored endpoints.

## Consistency laws

These are practical acceptance conditions, not a request for a new categorical
runtime. They apply under the same source and authorized operation context.

1. **Identity:** navigating between representations preserves the target entity
   and property identities; presentation paths and database entity numbers do
   not replace them.
2. **Read correspondence:** standard and custom views agree on the underlying
   values, unknown/missing states and relation targets. Shortened previews or
   incomplete selections remain explicit.
3. **Edit correspondence:** at the acknowledged committed basis, an accepted
   edit reads back as the validated, documented normalized value. A subsequent
   concurrent edit may change the current value. An unchanged value is a
   semantic no-op; this does not promise zero bookkeeping transactions or
   byte-identical formatting from every source adapter.
4. **Source preservation:** property edits preserve unrelated source and stable
   page/block identities. Concurrent changes produce a retained draft/conflict
   rather than an implicit overwrite.
5. **Authority:** a change of style or mounting location does not grant access
   or enable a command. Published and private projections retain their own
   independently checked authority.
6. **Context:** independent mounts and forks do not share accidental interaction
   state or apply obsolete asynchronous replies to a replacement context.

## First integration slice

Use one typed Company example with ordinary text/number/reference properties.
Reuse the existing field/form/table implementations. Show it in a page and a
standalone pane, with a scoped appearance override. Validate identical values,
reference targets, allowed edits, source preservation and conflict behavior.

Then produce a static Room-page presentation from the same field model, with commands absent. Compare null/unknown states, explicit
reference targets, coordinated draft conflicts and acknowledged revisions too. Live
website edits follow only through an explicit data/action adapter. Verify that
changing an editor or view cannot widen its read/write authority. Keep existing
unrelated Room-app isolation and writer behavior intact.

Use these concrete mounts to settle the smallest implementation API. Broader
navigation configuration, custom simulations and external publication should
reuse the resulting contracts rather than define parallel entity/edit models.

## First implementation boundary

`documents/property-presentation` adapts the existing inspected property model
into typed display cells without parsing edits or acquiring another source.
`uis/value-view` shares value display with saved tables. `uis/entity-view` is a
display-only composition of those cells, usable by Spindel DOM and static HTML
rendering. Presentation options select density, layout and a host-owned CSS
class; they do not supply actions. Pending and unavailable models render
explicitly, rather than looking like an empty successful query.

The property model now carries explicit page/block entity identity alongside
its existing target, saved entry and source/schema revisions. This identity is
for correspondence; it does not select a world or confer authority. Field
controls remain the existing revision-checked controls. Their shared
`fields-view` function only composes the keyed field loop used by page and
saved-view forms; it creates no new draft owner or writer.

The verification boundary includes exact scalar text (including integers above
JavaScript's precise range), missing/null/false/zero/empty distinctions,
structured values with misleading scalar hints, incomplete reference lookups,
HTML escaping, real canonical page/block models, and two independent DOM
mounts of the same entity. Static output has no commands or editable controls.
This is component infrastructure: it does not yet publish a website editor,
replace the legacy type editor, or expose a general SCI component API.
