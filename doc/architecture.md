# Architecture

How simmis is shaped, and where to look for each thing. Read this before the
code; the rest of `doc/` assumes it.

## The one-sentence version

A Clojure server owns a set of Datahike databases and is the only writer; a
ClojureScript client holds a real Datahike replica of the ones it may see and
queries it locally; agents run in a sandbox on the server and write to
branches rather than to trunk.

The proposed [semantic interface contracts](semantic-interfaces.md) describe
how typed documents, saved views and custom SCI interfaces should share
property meaning, commands, mounting and context-owned state.

## Room files and execution state

The **Memory** tree in the resizable left navigation browses each room's
Geschichte worktree on demand rather
than downloading file contents as a browser database replica. Markdown,
workflow files, and room-local Clojure source share the same hierarchy.
Filename search and folder-focus breadcrumbs avoid unbounded indentation.
Files open in independent tabs. Markdown offers Read, directly editable Rich
text, semantic Outline, and exact Source views; unsupported rich regions remain
source-backed. Tables and quotes have inert display previews in both Read and
Edit, while their exact Markdown remains outside the editable rich codec.
Comments stay in source without rendering as page content. Read shares Edit's
compact spacing and bounded explicit-language code highlighting. Rich typing
patches supported source spans locally; each automatic
save validates the complete bounded batch on the server before one canonical
publication. A compact file header carries path, persistence status and view
selection; secondary file actions and properties live in its overflow menu.
Formatting controls follow the focused semantic block without taking document
height. They support keyboard access and cannot bypass protected block topology.
Edited text encodes boundary spaces/tabs as Markdown character entities so
ordinary intermediate keystrokes satisfy the same strict round-trip gate as a
finished sentence. Entities decode once into inert text, never executable HTML
or reparsed Markdown; unchanged source retains its original spelling.
Formula source is recognized by bounded Flexmark parser hooks before Markdown
escape/emphasis decoding. Single-line `$...$` and standalone top-level `$$`
blocks project to frozen inline/display atoms. Same-line `$$x$$` and display
blocks nested in lists/quotes remain ordinary source in this dialect. Currency,
code and escaped delimiters retain their ordinary Markdown meaning. Exact raw
tokens and UTF-16 spans survive nearby prose edits; only generated separators
use the selected newline style. Rich saves validate token type and complete
source provenance, refusing fabrication, duplication and cross-region borrowing.
Formula changes use Markdown source editing. The shared element-owned KaTeX
projection uses local fonts, MathML accessibility, a fresh macro scope, disabled
trusted commands and finite expansion/size limits. Invalid TeX remains inert
source; formula rendering never starts SCI or a workflow.
YAML document properties and explicit document/block identities remain in the
source; programmable property schemas are separate trusted definitions, not
automatically installed by an uploaded file. Existing independent KBs
and drive resources remain accessible while this file-backed surface is
introduced; they are not silently migrated or deleted.

Actual text edits enroll a file in automatic saving; mounting, selecting text,
or reopening an existing dirty draft does not publish it. Automatic dispatch is
restricted to the live browser's home execution context; simulated fork edits
remain local rather than acquiring implicit host publication. The existing stamped,
authenticated compare-and-swap writer permits one request per file per view.
Newer typing coalesces while that request runs, then drains using its accepted
descriptor without resetting the editor or undo history. Exact own-write
observations reconcile at acknowledgement; another revision or deletion remains
a conflict. A context-owned hold survives errors, Refresh and connection changes,
so uncertain writes never retry automatically. Explicit Save remains available
for pre-existing drafts and recovery. Automatic saving affects only the selected
workspace branch, never proposal adoption. There is no typing-delay policy yet:
coalescing occurs during in-flight requests, not across an entire typing session.

Reference opening and its tab/new-column mutations likewise preserve an already
bound execution context; unbound device callbacks default to the live UI.
Forks update their own roster-derived tabs, selection and chat-scroll state.
Native focus classes and URL notifications belong only to the live world's
stable home, including ordinary execution-frame copies of that world.

Memory buffers use stable session/room/tab identity, so moving a tab between
columns retains its draft, selection and pending request callbacks. Late file
selection/rename callbacks locate the tab's current column and require its
unique ID, Memory type and original room before retargeting it. Closed,
repurposed or ambiguous tabs are not redirected. This preserves semantic
buffers; native editor undo history across a DOM-parent move is separate.
Tab close and direct move/activation helpers preserve the caller's context.
Physical replica disconnection and shared query-cache eviction on close run
only in the live home; a fork may close its local view without releasing the
live browser's resources.

Selected-region Markdown is a continuously validated source presentation, not
an Apply/Save transaction. Accepted input reprojects the exact span, retaining
the native source editor and current caret; invalid/intermediate input stays in
its separate local buffer. Block identities, children and frontmatter remain
protected. Closing unvalidated edits requires explicit discard; Escape retains
them. These drafts are execution-context state, not browser-restart recovery.
Office XML parts and typed-property forms still have explicit publication.
This gate does not make all explicit host actions simulation-safe; an explicit
Save retains its existing host publication behavior.

Markdown automatically loads page properties from trusted definitions in an
explicitly provisioned canonical workspace. Explicitly assigned semantic types
appear beside the file title; implicit structural roles and ordinary Markdown
tags are not presented as user-assigned types. Page and selected explicit-block
forms share a scoped renderer but retain independent models and drafts. Populated,
required and drafted fields appear first, with unused optional properties in a
disclosure. Generated structural titles are not ordinary form controls.
Its context-owned
drafts and model reads share Memory's target/socket/read-epoch fencing. Property
saves patch source through the same native writer with an atomic fresh schema
basis; they do not write disposable projection datoms. Existing unprovisioned
workspaces remain readable and report that typed editing is unavailable.

Single-valued relations expose their stable target UUID separately from ordinary
UUID-valued properties. Scalar reference tokens and closed `{id, path?}` mappings
can update that identity through the same guarded source writer. Mapping edits
retain their original comments, layout and inert path hint; the hint never
selects the entity. Clearing such a mapping remains an explicit Markdown edit,
not a button that silently discards structure. These controls currently accept
exact UUID text, not a searchable entity picker. Fresh whole-selection validation
still checks the target's existence and trusted codomain before publication.

Type assignments select existing trusted definitions for a page or explicit
block. Required new fields and values form one source edit, checked against
both original and proposed definition digests and the whole knowledge selection.
The editor retains declaration drafts across property-scope changes and refuses
to interleave their admission/publication with a file move or another save.
Definition creation and arbitrary schema publication are not part of this
control: Geschichte proposals transfer files, not arbitrary schema datoms.

Personal-workspace bootstrap has a separate HOST coordinator in
`model.personal-rooms`. It selects an existing default by ownership, never by
membership in somebody else's personal room. Existing legacy rooms remain
unchanged. Ambiguous defaults and partially marked provisioning are recovery
problems, not invitations to create another room. A new personal workspace has
its native type recorded in the original pending intent; canonical preparation
precedes membership publication. New canonical projects and personal spaces
include the ordinary editable `workflows/maintain-wiki/` bundle in their clean
initial commit. Reopening never reinstalls edited or deleted defaults.

This bootstrap is used by alpha account initialization and is not a public
provisioning RPC. It creates no workflow activation, work request, model grant
or room spending allowance. Legacy default-KB creation/attachment remains a
separate compatibility data/API path; this does not migrate those contents to
Markdown or prove immediate legacy-tool mounting after a live attachment.

Add Team and Add Memory both create canonical `:project` workspaces through
the same human provisioning service. Team opens chat; Memory opens the starter
wiki file. The form stores one frozen request in its execution-context Signal.
Its owner and client UUID derive the native slug; native atomic exclusion refuses
a repeated creation rather than retrying it. Final membership and the typed
creation receipt publish in one control transaction after native preparation.

The service captures the authenticated human and original application before
queuing blocking work. Its application operation covers physical provisioning,
registration and publication, with current-owner checks before the final steps.
The publication transaction rechecks human identity, member enlistability and
the exact prepared Room basis. Fencing during preparation joins the actual worker
and leaves prepared resources without publishing membership.

A separate read-only operation observes that exact intent. Pending/native-ready
states do not prove an active worker or a completed workspace; an absent record
may mean the original request is still queued. Reconnect never resends creation.
An interrupted observation can be checked again, with old-socket replies fenced.
The form's recovery state is session-local: browser reload persistence and native
setup recovery are not implemented. Memory navigation lists canonical room
workspaces; standalone legacy Wiki/Drive lists and legacy-wiki Settings controls
are removed. Existing stores are not migrated or deleted. Normal new-workspace
forms create canonical workspaces, without a separate Wiki/Drive entity.

The experimental source-selected knowledge view derives semantic document/block
rows from one immutable Geschichte snapshot without writing a second repository.
Its default selection includes root and nested Markdown, with trusted exact folder
roots/exclusions, a 20,000-entry metadata scan bound, and separate 200-document,
8-MiB source and syntax-node bounds. Invalid identities or syntax return diagnostics,
not a queryable partial view. The resolver-backed `agents.memory-read` vocabulary
offers document descriptors, exact source reads and bounded literal search; it
does not expose its backing DB. Participants with a writable room repository get
this vocabulary beside the unchanged legacy `wiki/*` API. Saved functions resolve
their selected world and live room/repository authority on every call through
Dvergr's effect boundary. Whole-world descendants read their exact registered
repository; an ordinary participant instead follows its existing proposal file
workspace when one exists. Neither route creates a proposal or hydrates trunk.
Selection/content identifiers do not pin storage against GC: these read receipts
explicitly borrow the source lifetime rather than promising archival retention.

`ops.knowledge-inputs` prepares bounded frozen inputs for ordinary saved
workflows: incoming evidence in declared inbox/transcript folders and supporting
knowledge in separate folders. A metadata-only trigger fingerprint excludes
supporting wiki changes; an execution fingerprint includes them. Workflow,
staging and output folders cannot be selected as inputs. Capture copies exact
UTF-8 sources plus a content-addressed manifest into the normal workflow input
map (100 files/2 MiB total, including the manifest). This collector does not
submit work, spend funds, advance maintenance cursors or install a schedule.
An inert `input-selection.edn` sidecar pins the declared knowledge policy with
the ordinary workflow bundle. Server and local-worker submission capture it
after authorization and request retry lookup; the original submission remains
the retry identity. Captured bytes and collector provenance are retained with
the work request. Pure preparation and benchmarks require verified frozen
inputs instead of resolving a live policy.

`ops.conversation-capture` is a HOST composition for explicitly
selected human messages. It reads at most 50 exact native message IDs from one
immutable room-store snapshot and checks sender identity against the trusted
control DB. A user role alone is insufficient; agent, Run, transcript and other
generated records are refused. Mention audiences are routing, not privacy ACLs.
The preview preserves exact text inside inert fences with source IDs, authors,
timestamps, byte counts and content hashes. Its exact captured application
operation and native Room lease cover source/repository access; a binding or
permission check alone does not retain those resources. Publication requires the inspected
capture ID and original Memory target stamp, then rechecks current permissions,
source eligibility and selection at the existing native writer's effect gates.
One deterministic `inbox/conversations/<capture-id>.md` destination prevents
duplicate files; an occupied path is a conflict, never proof that an earlier
unknown write acknowledged. There is no automatic retry, chat cursor, ingestion
policy or spending grant in this composition. Published
evidence is an ordinary input to the same file-backed knowledge workflow.
It is a deliberate exported snapshot, not a continuously mirrored chat view;
later edits/deletions of a message do not rewrite an already published file.

The chat's single-human-message **Save to Memory** action previews those exact
server-selected bytes before explicit confirmation. Preview/publish RPCs require
room write permission and capture both authenticated identity and the original
application registration before queuing blocking work. That registration admits
native access only off-engine and refuses after fencing; it cannot adopt a newer
installation. Client state lives in execution-context signals, with account,
message, originating tab's room/thread, socket and request fences. Hiding/reconnecting after
an uncertain save retains an inspection-required hold, never automatic replay.
Only a validated native saved receipt enables **Open saved file**; destination
inspection remains a separate read and does not reconstruct an acknowledgement.
This surface exports evidence, not a summary, wiki adoption or paid workflow.
Commands dispatch only from the live home UI context, not simulated forks.

The `ops.retained-knowledge-sources` host reader addresses a canonical
Proposal plus its file-output component, not an arbitrary artifact or current
workspace path. It verifies the original request, typed filing, source revision,
publication target and frozen workflow before listing manifest-selected sources
or returning at most 16 KiB of base64 bytes. Every chunk rechecks current human
room-read permission. The captured application's original connection, execution
context and physical operation lifetime cover the full read and projection.
Its 128-MiB encoded-payload ceiling accommodates bounded duplicated workflow
metadata; it does not widen admission limits. Missing/corrupt/oversized evidence
fails closed. It reads no Attempt, live branch or current workspace selector,
and confers no adoption authority or permanent storage retention guarantee.
Authenticated proposal-source RPCs capture both the caller and original App
before moving work off the engine drain. Current room permission and component
ownership are checked again after the physical read and formatting. Proposal cards, including resolved audits, expose these frozen
citations as read-only text, separately from current Memory. Client paging is
bound to the exact proposal/component/request/selection and content identity;
list/read tokens plus authenticated socket/session fences reject stale replies.
Bytes stay in immutable signal state until strict UTF-8 decoding after the final
chunk. Inspection does not execute code, navigate a current file or accept work.

`/proposal/<proposal>/source/<component>?path=<encoded-input-path>` opens a
separate read-only source tab. It lists the authorized retained manifest, then
reads only the exact listed source path. Its bounded observation belongs to the
session and stable tab ID in a Spindel signal, independently of the Proposal
card's selected source. Connection, roster/control invalidation and the actual
tab identity fence callbacks; moving a tab preserves its owner, while closure
or repurposing retires it. Concurrent source tabs cannot replace each other's
text. Actual tab closure releases that tab's text/partial-byte cache in its
own execution context; moving a tab preserves it. Source links in the existing manifest list use ordinary anchors with
native modifier-click/copy behavior; ordinary clicks open a Simmis tab. The
viewer offers Open proposal and read retry, without edit/evaluate/adopt actions.
A bare `/in/knowledge/...` path has no inferred live-file or historical origin.


Setup replaces the collector-owned `/in/knowledge/` subtree so inherited stale
staging cannot augment the manifest;
unrelated room files remain. Recurring deployments pin the policy, then select
one immutable source snapshot per occurrence. An initially empty or unchanged
trigger set skips before body reads, allowance checks or provider preparation.
Deleting all previously processed evidence is a change that requires another
attempt. Selected bytes are retained before submission; uncertain admission is
held after restart, never recaptured from newer room state. Only a physically
settled Job whose direct candidate has a successful certified Attempt against
the request's exact EnvironmentDef advances the processed-input cursor. Job
completion alone does not. Failed or uncertified work holds the activation for
explicit resumption. This cursor records processing/proposal, not adoption of
any generated knowledge.

Fresh host knowledge admission resolves the logical room's explicitly owned
default repository, not an attachment-order or current-workspace fallback.
Input selection and complete wiki occupancy use the same immutable database
value. Retained target metadata includes the registry system and runtime
system identities alongside the physical repository/store/branch/ref identity.
These are historical evidence, not permission or a lease over the live target.
Older unmarked captures remain readable but do not gain publication authority
by matching today's workspace. Benchmark and supplied frozen inputs remain
unmarked. A changed host target policy requires explicit schedule redeployment;
the server does not silently upgrade an existing activation.

Room source and execution state have different lifetimes. A daemon room's
repository is initially seeded from the sandbox library (or its configured
import source); participants load source into their own SCI interpreters,
selected through their Spindel execution contexts/worlds. Saving a file in
Memory neither evaluates it nor reloads any interpreter. Host-injected
functions and permissions are capabilities, not editable source files.

Clojure-family files (`.clj`, `.cljc`, `.cljs`, `.bb`) follow the existing user
Code View preference: editable Clojure or a read-only Superficie projection.
A per-tab override and Follow preference control do not change the file bytes
or saved preference. Conversion failures explicitly retain source editing;
the file writer and evaluator never consume the projected syntax. `.cljs`
display support does not enable ClojureScript execution on the JVM evaluator.

For `.clj`, `.cljc` and `.bb` files, explicit selection, top-level-form and
whole-file evaluation targets the authenticated human's separate room-scoped
SCI world component. It does not borrow an agent interpreter or inject its
room tools. Definitions persist for that human in the room until its context
closes; descendant worlds select an independent interpreter. The session index
is itself a fork-owned non-reactive world component: a following child's index
must not observe a later parent retirement and lose its existing interpreter.
Evaluation is
owner/`:merge` gated, serialized per interpreter and bounded to 64K source
characters and one ten-second deadline including admission. The actual worker
owns the original application ticket, actual Room exclusive lease and interpreter
monitor through execution, bounded value/output printing and cleanup. Current
read/merge authority, original Room incarnation and application admission are
checked again after waiting for that monitor. A caller receipt and physical
worker exit are distinct: cancellation quarantines an entered heap, and retires
its exact world reference only after evaluation and awaited cleanup exit. A
cancelled admission waiter never enters SCI or retires an untouched heap.
Failed retirement transfers the same Room and application native ownership slots
to explicit cleanup recovery and fences application admission, including already
admitted shared-interpreter waiters at their final entry check. Stop/restart
cannot release the old installation until recovery acknowledges completion.
These runtime handles are not retained in forkable session state. Results, printed output and
the exact evaluated source are retained in a bounded twenty-receipt editor signal
history even when the draft changes or moves; older replies are marked stale
and do not overwrite newer draft text. They are not a durable
Run or a deployed workflow. The old chat inspector Quick eval input is removed.

Saved Markdown `clj`/`clojure` fenced cells have an explicit inline Run control
in the continuous rich editor. Opening, editing and saving never run code.
The host derives the exact parser literal from the authorized saved file entry;
the request contains only its repository/stamp, revision, source span, optional
explicit block ID and namespace, never caller-supplied executable text. Source
identity, original Room/application owners and current read/merge authority are
checked again under the execution lease after the interpreter monitor. The
16K-character whole cell uses the same human SCI session and ten-second boundary.
Indented code, other languages and extra fence-header tokens are inert. Header
tokens do not configure execution.

Run is offered only for a clean saved rich projection with exact structural
correspondence. Fresh source-derived region/opaque offset keys may differ from
the retained native undo baseline; explicit block IDs, opaque bytes and all
content must match. Offset-based math/footnote token keys remain strict, so a
shifted token can temporarily disable Run until the document is reopened. This
specialized cell check does not weaken source-write or token-provenance gates.
Results are disposable inline chrome and retained in the same twenty-receipt
human history, including the executed input, exact original source span, saved
entry/revision, target stamp and Room incarnation. They never enter Markdown,
pretend to be stable cell IDs across revisions, or become durable workflow Runs.


Saved-cell results may opt into one bounded table or chart by returning an exact
`:simmis/presentation` envelope. Conversion happens beside printing inside the same
owned SCI worker; ordinary values remain textual. Invalid presentation shape/size
adds a diagnostic while retaining the successful computation's printed evidence.
Tables have at most 20 columns, 50 rows and 32 KiB of displayed text; integer and
finite decimal cells become exact strings before transport. Charts reuse the
existing strict ChartData validator and fixed scene, with bounded direct numeric
representations before floating-point conversion. No arbitrary serializers, DOM,
functions or callbacks cross the receipt boundary.

Only the newest eligible saved-cell receipt receives an inline rich preview.
A chart result reserves one of the document's existing four chart slots; prior
receipts retain text. Context can also show the newest current table, without
mounting a duplicate chart. Native result chrome remains outside editor content,
export and undo, is removed when correspondence is lost, and is reconstructed
from the retained receipt on undo. Loading or saving source never executes it.
The ordinary scratch REPL remains textual; the internal saved-cell adapter opts
into presentation without adding a public request option or permission grant.

The Context footer for Markdown and code-file panels includes a dual-input **Room REPL**.
It uses the same tab-owned buffers and human SCI session; the file body contains only
its document and explicit code-cell controls.
Separate Clojure and Superficie scratch buffers follow the Code View preference unless locally
overridden; switching syntax never converts a buffer or changes the file draft.
Both inputs use the same human SCI world component, with an explicit namespace
target. A submitted `ns` change updates that target only if the user has not
changed it while the request was pending. Parsing and evaluation run within
the existing sandbox evaluator's ten-second boundary, with a 16K-character
input bound. Clojure and Superficie both parse/evaluate incrementally. Symbol
and keyword resolution consult the selected SCI interpreter, not JVM host
namespaces. No host eval or mutable user block/shape registration in global
Superficie registries is used; ordinary user-defined macros can be called, but
custom Superficie block-shape registration is not exposed.

The last 20 REPL receipts live in the context-owned editor signal, recording
original input, syntax, namespace, printed value/output and generated Clojure
(preview bounded to 64K characters). They are not durable Run receipts. Failed
evaluation retains a receipt and warns that earlier forms may have executed;
input is not transactional. A timeout quarantines the selected interpreter until
physical exit and successful retirement; it does not claim hard termination of
arbitrary inline native work or synchronous user cleanup. The
UI targets the live room's human session; this does not introduce an arbitrary
world picker or an agent-interpreter attachment. Underlying component-based
fork isolation is covered by the evaluation tests. Superficie input is a
separate language input, not an assertion of lossless source-view conversion.

The file tab's Context footer previews changes from a bounded 31-commit window
of the canonical Geschichte ancestry, including merges. It compares file
metadata against parent snapshots and loads historical text only on demand.
Previews are read-only and do not change the draft, current branch or GlobalCut;
this is not yet the legacy wiki's interactive history subway. Renames appear
under their respective paths.
Imported commits without materialized snapshots are reported as unavailable;
a readable snapshot with an unavailable parent is labelled a boundary, not
asserted to be a file change.

Folder listings and open buffers follow private workspace invalidation hints.
An owned host notifier listens to the canonical concrete connection's durable
Datahike commits, queues/coalesces room identities off the commit loop, and
rechecks each recipient's current read authority before publishing. Speculative
connections do not publish to this feed. The private `:memory/<party>` stream
carries only room identities, never paths, branch IDs or file bytes. It is not
raw Geschichte-store replication: Datahike store walking can traverse blobs,
and a trunk-scoped handshake alone does not filter subsequent store writes.

After each authenticated socket handshake, Memory reconciles immutable bounded
metadata snapshots, even when no hint was received. A hint arriving during a
read causes a second reconciliation on completion, not a timer. Changed open
files load on demand; clean buffers adopt them, while dirty text, selections,
rich regions and Office edits remain with an incoming-revision notice. Deleted
or externally moved files retain their buffers and cannot be implicitly
recreated. External renames are deletion/addition, not guessed from equal hashes.
Per-view read epochs fence old folder/file replies against newer metadata;
background refresh never changes the selected file. Metadata requests cover up
to 64 folders and buffers per view; collapsing a folder releases its listing.
The sidebar and file tabs share one context/session/Room metadata coordinator.
It derives live demand from the current layout and expanded navigation tree,
combines whole view requests within those bounds, and serializes the reads.
Separate batches retain separate immutable observations, including dispatch-time
draft generations for each participating view. Their publication sequences are
local ordering tokens; several batches never claim one atomic snapshot. A view
consumes its own covered observation once before issuing followup reads.

The shared Signal contains metadata, admission frames and bounded batch results,
with no file bodies, mutable editor models, native handles or callbacks. File
tabs and the sidebar track it at the top of their rendering Spins. Closed tabs
and collapsed trees stop contributing demand; remaining navigation rendering
prunes unused batches. Unsupported demands are refused per view rather than
blocking valid siblings. Write acknowledgements invalidate the shared Room
observation; private source hints and reconnects also trigger reconciliation.

Socket identity, owning world, current Room roster and source-hint generations
fence publication and consumption. Removing a Room from the loaded roster
removes verification and eligible listings while preserving human drafts.
Restored access requires a new observation. A newly observed workspace target
immediately retires older verification in other batches, without inventing
coverage for their files. Read replies also retain their original roster basis;
original write and execution receipt ownership remains separate.

Metadata discovery captures the authenticated human and original application
before entering its blocking worker. A fixed host service leases the actual
Room, projects one owned-default snapshot, and rechecks current read authority,
Room incarnation and default binding before returning. Permission flags come
from that final control snapshot. A same-target source write may leave an older
coherent observation; private hints drive catch-up. Metadata hashes are neither
ordered versions nor a guarantee that several requests saw one atomic snapshot.
Discovery does not read file bodies or grant authority to later operations.
The client separates pure per-view metadata reconciliation from followup reads.
Each observation uses draft generations captured at dispatch; a reply cannot
relabel newer typing as its baseline. Followup planning uses current pending
intents, distinguishing omitted paths from explicitly observed absence. Target
changes retire current-file evaluation displays while retaining historical
receipts and outstanding evaluation bookkeeping.

Metadata also carries a separate conservative `:definition-basis` token from
the physical store/branch, commit ID and max-tx. Schema-only native transactions
change it even when file descriptors and logical HEAD do not. It is an
invalidation hint, not replicated schema, permission, an opened-file revision or
a target stamp. Existing draft conflict comparisons still use the file entry
and target stamp; this token must not turn an unchanged text buffer into a
source conflict. Typed property controls use the current definition basis.

The notifier owns routing interests for the process lifetime, not one callback
per mounted editor. Room shutdown releases concrete listeners; retained party/
room identities allow rebind after canonical room replacement, followed by
catch-up. Application shutdown joins its one notifier worker. Local durable
writer commits, including logical checkout/reset, are observed; independent
external-process writes to a shared physical store are not a supported feed.

Editor drafts, selections, request tokens, and read/mutation generations live
in a session/room/column/tab-keyed Spindel signal. Native CodeMirror/Tiptap handles
and download object URLs belong only to their DOM elements and are released
on detach. ProseMirror JSON crosses a JSON-data boundary before ClojureScript
conversion: its native attribute objects must not enter draft state or break
controlled-model equality. Authenticated RPCs capture the principal before dispatching bounded
store/parsing work off the FRP drain. Saves use the canonical Dvergr file
writer's opened-revision precondition, effect gate, and applied entry receipt;
a late pre-save read cannot replace a new save acknowledgement. A revoked
room write permission is checked again at the actual effect boundary.

Markdown rich editing uses one Tiptap document. Explicit semantic block IDs
and nesting live in protected editor containers; adjacent supported syntax
within one owner shares an editable region. Paragraph splits/joins do not
allocate or delete knowledge-block identities. Unsupported syntax remains an
inert, source-backed node and is included as text when copied. Selection can
cross containers, but transactions that remove/reorder protected containers
are refused with feedback; structural knowledge changes remain Source edits.
The source map, baseline models and selections live in the context-owned draft.
Unmodified vertical arrows enter the start/end of adjacent editable regions
across protected semantic wrappers at visual text-block boundaries; this
transition does not preserve the horizontal caret column. Ordinary wrapped-line motion
and modified range selection remain native editor behavior. Gap selections keep
their native position through the controlled selection round trip rather than
being interpreted as text positions. Navigation does not change block topology.
Saving sends disjoint original source spans through the existing authenticated
rich-save path. The host reconstructs supported spans, validates Markdown
round-tripping and unchanged frontmatter/explicit identity nesting, and uses
the canonical file writer's revision precondition. Unchanged syntax siblings
and source outside the changed spans retain their exact spelling and newlines.
Empty-file/block insertion is allowed only at reconstructed insertion anchors.
Empty top-level rich paragraphs represent Markdown blank-line layout, not new
knowledge-block identities. Their blank lines within otherwise nonempty content
are serialized; an entirely blank insertion remains empty. Semantic
round-trip comparison ignores only those empty top-level paragraphs, since the
Markdown AST has no corresponding node. Reopening reconstructs paragraphs from
source rather than retaining transient empty editor nodes. Nested list shape
and significant text whitespace remain subject to exact validation.
Bare Markdown list markers project to a list item containing an empty paragraph,
as required by Tiptap. This lets the empty item created by Enter round-trip and
save while retaining its place in the list; it is not discarded as blank layout.

List indentation uses the native Tiptap list keymap: Tab sinks beneath a preceding
sibling, Shift+Tab lifts one level, within the same protected semantic region.
Scoped list styles preserve visible indentation through successive nesting levels.
Block handles expose a separate **Copy reference** action producing a portable
canonical Markdown link; the ordinary link still supports browser sharing/menu
commands. Plain canonical URLs from this app, and copied reference Markdown,
paste as link marks in one undo step. Foreign origins, arbitrary HTML and code
pastes keep their ordinary behavior; clipboard data never imports block identities.

Saved data-only charts use a separate fixed read RPC. It captures the original
application owner before queuing work, holds the native Room operation lifetime,
and checks current human read authority without requiring merge permission.
One immutable owned-default DB supplies the saved document, explicit typed
block, full feature definitions and referenced JSON bytes. Before replying,
the service rechecks authority, Room incarnation, target identity and relevant
source/spec definitions. The finite response includes document/spec entries,
source/scope revisions and feature digest; native exceptions are normalized to
a constant unavailable error without forwarding helper messages, data or causes.

Chart state uses the existing session/room/column/tab Spindel ownership and
socket/target/generation request fences. Room notifications invalidate all
displayed charts through the existing Memory stream marker and definition basis,
including spec-only commits whose JSON file is not an open buffer. A metadata-interest
refresh with the same source/definition basis keeps an accepted chart visible;
request acceptance still uses the exact read epoch. No extra polling or file cache
is added. Optional analysis follows a saved baseline once per identity; a
baseline-only reply cannot replace newer prose, selection or `rich-base`.
Analysis is usable only when its revision matches the saved content identity.
Chart responses are not accepted into a dirty draft. Already accepted saved
charts can remain visible during protected prose editing; Source editing hides
them. One closed pure scene supplies Spindel's Read DOM and Tiptap's inert
element-owned NodeView projection. Read/Outline roots are keyed by file path
and block identity, and loop items carry their chart data as an explicit
incremental dependency. Inline `simmis-chart` fences reuse the strict ChartData decoder during ordinary
file analysis. At most four parsed previews are returned; native and Read
renderers separately cap actual matching occurrences, including duplicates,
within the remaining shared four-chart budget. Exact projected code text and
language select pure presentation data; unrelated prose can change without
invalidating it. No block identity, whole-document saved claim or additional
RPC is needed. The native code node's element-owned cache uses current document
and preview props, and its disposal clears the projection. Run controls still
use their original strict saved-document correspondence.

Charts never become editor attributes,
editor clipboard serialization or source edits; element detach releases their
native cache. The accessible chart table remains ordinary selectable content.

Word `.docx` files open in a bounded saved-document reading view. The main
document is selected through package relationships and content types, with
namespace-aware strict/transitional parsing. Native paragraphs, direct emphasis,
outline levels and simple tables use application styling. Page layout, style
inheritance, headers and footers are not reproduced. Fields, embedded objects,
structural revisions and other unsupported content have explicit placeholders;
field scopes spanning paragraphs and tables cannot expose cached results as
ordinary final text. No document relationships, images, styles or external
resources are fetched or executed.

Document/Advanced XML is context-owned view state. Office reads load with the
file and refresh from replicated metadata. A saved reading projection can update
while XML typing, selection and its save baseline remain intact. Entry,
generation, target and read-epoch checks reject stale asynchronous replies.
Reading-only replies are independent of the selected advanced XML part.

Office containers retain a bounded advanced XML-part editor. Each operation
reopens the selected immutable file revision;
there is no retained server document handle. Validation disables DTDs, and an
edit copies untouched part bytes rather than re-emitting their XML. Exact
no-ops retain the original container. Saving modified XML does not certify
Office schema validity or preserve a document signature's validity. The XML
editor accepts UTF-8 parts only; other supported encodings may be read without
conversion. A failed optional reading or default XML projection leaves the
original download available. DOCX reading never converts a package to Markdown
or supplies publication authority.

Eligible saved Word text runs offer an explicit whole-run correction. A bounded
lexical locator is cross-checked against the hardened SAX element sequence;
the host patches the original UTF-8 character-data span and verifies the
resulting semantic tree. Duplicate text is selected by source identity rather
than text search. Exact no-ops retain original package bytes. New leading or
trailing whitespace requires an existing effective `xml:space="preserve"`.
Fields, review content, hyperlinks, content controls, mixed-content text leaves,
signed/macro packages and parts with bookmark/review ranges are outside this
profile. Application ReBAC governs file writes; this is not an implementation
of Word password/protection semantics.

Correction tokens bind the file path, descriptor and main-part source revision;
they grant no authority. Publication uses the same current owned binding,
fresh authority gate and native binary CAS as other Office writes. Correction
drafts are context-backed; incoming revisions and late acknowledgements cannot
rebase newer typing onto the wrong saved XML version.

## Processes and ports

One JVM runs everything in development (`clj -M:dev`):

| | |
|---|---|
| Web UI | http://localhost:8080 |
| WebSocket (kabel) | ws://localhost:47295 |
| nREPL — server, Clojure | 47888 |
| nREPL — browser, ClojureScript | 9631 |

Those are the default instance's ports. `SIMMIS_PORT_OFFSET` adds one number
to all of them (and to shadow's own 9630), so a second instance can run beside
the first; `src/is/simm/runtimes/instance.clj` resolves the ports, the state
home (`DVERGR_HOME`, default `.dvergr`) and the data root (`SIMMIS_DATA_DIR`,
default `data`), and `user/shadow-config` applies the ports to shadow.

`dev/user.clj` is the lifecycle; `src/is/simm/runtimes/web.clj` is the server
boot — system DB, mail sync, room hydration, kabel + auth + sync, telegram,
then the HTTP routes.

Private room apps use `runtimes.room-apps` over Dvergr's native static handler.
The HTTP route authenticates, then the native callback authorizes the exact
room resolved for serving under an App operation. The synchronous read remains
owned until permission and host identity are rechecked. Response CSP sandboxes
scripts with an opaque origin and Referrer-Policy suppresses referrers. No
capability/iframe or public-publishing endpoint is installed by this adapter.

Two boot steps are switches. **Storage GC** (`runtimes/storage_gc.clj`, which
drives dvergr's `gc-stores!` at boot and every 6h) is OFF unless
`:storage-gc {:enabled? true}` or `SIMMIS_STORAGE_GC=true`: on datahike without
#1117 a mark that dies on an Error comes back empty and the sweep deletes the
whole store, which is how the dev.simm.is system DB was lost. simmis pins
datahike 0.8.1910, which includes #1117 (a mark that dies raises
`:gc-incomplete-mark` and nothing is swept); the switch stays off by default
regardless. While enabled on an older datahike (or a source checkout) it warns on every sweep, and it logs a sweep that timed out,
failed or skipped rooms as an error rather than as an empty one. dvergr refuses
a deleting sweep unless it was enabled for the process's home
(`enable-storage-gc!`, dvergr 0.1.291): the loop's `start!` enables it for
simmis's home and `stop!` (also on server stop) revokes it, so with the switch
off nothing is ever enabled and a sweep run anyway is logged as refused. **Mail sync**
for stored accounts is ON unless `:mail {:sync? false}` or
`SIMMIS_MAIL_SYNC=false`, which isolated instances use. The **transcript
watcher** (`runtimes/transcripts.clj`) runs only when `SIMMIS_TRANSCRIPTS_DIR`
names the directory jigasi saves call transcripts into; unset, boot logs one
info line and watches nothing. A system-DB directory
with no `:db` head stops the boot with that diagnosis; it is never created
over or deleted.

An optional private MCP listener (`runtimes/mcp.clj`) starts in the same JVM
when `SIMMIS_MCP_PORT` or `:mcp {:port ...}` is configured. Its bind is always
loopback, its base port receives the instance offset, and it serves the
existing rooms through Dvergr's embedding host hook. It owns no databases.
The default surface is selected room/message/run, workspace, Attempt, job and experiment
reads for an explicitly authenticated human. Explicit work credentials add canonical
chat dispatch and one bounded isolated candidate, not a raw database writer or
unrestricted eval endpoint. The typed system-DB admission journal makes request
retries observe existing Dvergr Jobs/Runs; it is not a second workflow engine.
The opt-in `simmis_coordinator` profile composes scoped child Runs and experiments
within an existing allowance. Ordinary file workers retain their narrower
surface; see [coordinator-work.md](coordinator-work.md). The current alpha
runtime pins classified resource wallets and independent Run sessions. Existing
unclassified financial stores are not migrated automatically; use a separate
fresh alpha home when testing this dependency pin, not a copied or reset home.
Workflow bundles are versioned Geschichte files. An explicitly admitted,
exact-revision `simmis_workflow-run` uses the ordinary bounded candidate and
completion evaluator: declared literal parameters and frozen source/inputs
are materialized in its inherited room fork, not trunk. Its source pin and
input replacements appear in the proposed diff; it never implicitly executes
the bundle's checker or treats completion as correctness. Workflow preparation
separates `workflow-runs/prepare-source` (revision validation,
literal rendering and input selection, without a provider) from native
candidate construction. Portable worker input construction uses the same
`worker-files/freeze-files` descriptor validation and content identity as
explicit room-file selection; it does not read additional room files.
Room-header workflow
tabs carry the room replica scope into their Run inspectors and open results
in the column that owns the action. Restored workflow and Run tabs resolve
missing scopes from the authenticated room roster, like chat/thread tabs.
Pinned recurring deployments use a host-owned scheduling adapter, not another
execution engine. A listener on Dvergr's root clock only coalesces notifications
into a one-element queue; one owned off-engine worker recovers and drains the
system-DB occurrence outbox through the existing workflow/benchmark admissions.
Copied clock listeners reject non-root contexts. Shutdown unsubscribes and joins
the physical worker before replacement. Current authority, retained-source
identity and host/model policy are checked at each admission. Money and dispatch
allowance come from the same finite shared room wallet as manual execution;
scheduling never creates funds. Overlaps are skipped and missed ticks coalesce
to the latest intended slot. The Schedules view keeps these deployments distinct
from legacy agent-authored prompt/code tasks.

Creation accepts an optional `:enabled?` boolean, defaulting to true. False
creates a paused activation atomically; no create-then-pause interval can admit
an occurrence. A separate immutable `:schedule.activation/initial-enabled?`
records creation intent, while `:schedule.activation/enabled?` remains the
current pause/resume state. Exact request retries preserve later control
changes; opposite creation settings under one request ID conflict in the
writer. The initial setting does not alter retained code/execution target
hashes. Historical rows without it have the previous initially-enabled meaning.
Paused creation still validates a concrete model, finite execution limits and
source configuration; it does not choose a default model, grant resources or
prepare a provider. Later enabling uses the existing cadence/coalescing rules.
Owned control services provide bounded schedule metadata,
pause/resume and metadata-only request status without hydrating a room or
resolving its current file default. Inspection and pause do not prepare a
model. Resume rechecks the stored human creator and policy; its transaction
rejects changed retained identities and uncertain or foreign active occurrences.
Legacy activations may be inspected and paused but must be explicitly recreated
before owned resume. Application startup captures one canonical host for MCP
and the scheduled-work dispatcher. Its connection and execution context must
remain the original live owner; repeated starts reuse that exact host. The
optional MCP listener does not own the registrations. A native installation
owner is the single admission-phase authority. Shutdown fences it, then joins
scheduled work and synchronous HOST operations without holding the installation
monitor. A failed physical join
retains a non-ready installation and host until a later successful stop; no
replacement can start underneath it. Older deployments are explicitly held,
never silently replayed or upgraded, and their existing Runs are unchanged.
Installed MCP operation calls and file-resource body reads capture that exact
owner; an old captured host cannot use a replacement installation. Native
ownership is not semantic fork state or permission. `application-ops/host!` is
only a non-owning lookup; `with-operation!` admits before handing its host to a
synchronous body. Already-admitted same-thread owner-stack nesting may finish
during shutdown; inherited cross-thread or expired tickets cannot grant access.
Results must not defer native resource access beyond the owning call. Separate
Room-specific operations also need an exact Room lease. Manual two-arity MCP
hosts and older browser adapters still require caller-owned lifetime; this does
not silently turn every existing adapter into an owned operation.
Browser workflow adapters capture a
normal-human canonical host, without an MCP marker or a prehydrated file
default. They share the fixed metadata authorization policy and original save
acknowledgement service with MCP; the browser projects the saved definition
while MCP retains its bounded receipt. The four legacy allowance/enrollment
controls authorize before hydrating their native Room. The Schedules dashboard
loads retained workflow targets on a host worker through the same owned metadata
listing. Owned execution requires its
target-bound source revision separately from the portable bundle identity.
Occurrence metadata validates its exact creator/room/request binding before
projecting Run links; live Job status additionally matches native room, owner
and Run kind. Typed filing receipts expose only a proposal link or no-change
outcome. These observations do not collect output, approve it, hydrate a file
workspace or retrieve artifacts. Proposal navigation retains its existing
authorization. Corrupt typed receipts fail closed rather than becoming no-ops.
Worker workflow/benchmark retries inspect canonical metadata before checking
fresh-admission readiness; only normal absence may enter new admission. Saves
project their original sealed acknowledgement, never a subsequent source read.
Application startup enables the owned dispatcher, but does not install or
enable default maintenance workflows.
The workflow editor's pure reconciliation model advances clean source snapshots
without resetting explicit form choices or admission ledgers. Dirty drafts keep
their original base and expose incoming changes; known-stale source cannot
start fresh work. Observation frames are immutable data scoped to the socket
and owning execution world, not shared native futures or mutable UI gates.
Workflow and Schedules views reconcile automatically from the private Memory
hint stream. A watched room receives room-only source/control invalidations.
Workflows also lists that room's existing activations beside the workflow,
including paused, held and earlier-source revisions. Unknown or invalidated
observations are shown as unverified, never as an empty schedule list. Opening
the Maintain knowledge card does not install source, fund work or activate a
schedule; configuration reuses the ordinary workflow form and admission ledger.
The card links recent Runs and filed proposals through the existing authorized
navigation. Evidence is the room's inbox and transcripts, not automatic chat
ingestion, and outputs remain proposals for review.
The global dashboard registers a separate party interest in the SAME notifier
without opening room repositories. Its content-free schedule-index hint rechecks
the current human identity before publication. Neither stream carries a raw
control database, claims, wallets, files or read authority. Watch registration
acknowledges authorized interest only, not installed-source readiness or retained
authority. One worker owns source reconciliation; concurrent views enqueue
interest without competing to install bindings. It installs a listener before
publishing the initial hint. Unexpected worker failures are logged as errors.
Authorized bounded RPC snapshots remain the projections' source of truth; this
is not a replicated control database. Initial observation and subscription-readiness catch-up close
the registration race, and new socket epochs force reconciliation. In-flight
reads coalesce later invalidations without timers; callbacks recheck current
dependencies even before the render resumes. Normal refresh controls are
replaced by explicit error retry. Controllers are constructed inside the
render Spin, binding native callbacks to that rendering world; they are not
transferable callback handles for an arbitrary ambient fork. Copied worlds
replace inherited fetch frames rather than awaiting a parent's native reply.
A rejected observation acknowledges its slot without completing an unpublished
generation. A save's own durable hint may arrive before its sealed ACK: the
ACK advances only its original target-bound base and sent bytes, preserving
subsequent typing and never rolling back a newer observed base. A stale ACK
does not restore permission or verify the current default; a new authorized
snapshot reconciles it before fresh execution.
Separately, an
exact-revision offline benchmark reuses Dvergr's experiment runner and room
Attempts/Scorecards; it requires both installation-operator and room merge
authority. Its clean fixture room is provisioned outside the authoring room,
so candidate workspaces do not inherit checker/gold files. The Job runs in
the fixture's execution context while durable results stay in the control
room. Synchronous SCI grading stays inside Dvergr's physically tracked scoring
worker; it is cooperative trusted-operator execution, not hard process isolation.
Optional reference/damaged-answer calibration runs on the owned world-setup
worker before each candidate; answers remain outside its workspace and do not
promote checker trust.
Optional `fixture-pack.edn` and `cases/<id>/fixture-pack.edn` files hold bounded,
inert path-to-text data. Dvergr decodes them into the same selected fixture maps
without reformatting their contents. Copied Markdown identities inside those
strings remain outside the authoring room's canonical document projection.
Root and case pack source participates in the dataset fingerprint; the bundle
identity continues to cover every raw source file.
Local-worker transport lives separately in `ops/worker-service.clj` and
`ops/worker-api.clj`, backed by typed enrollment/assignment records. Its six
narrow operations use separate worker credentials, immutable selected-file
inputs and bounded output receipts. Application startup installs these bindings
with disjoint worker/lease authentication on the private MCP listener. Separate
human control operations require an explicit worker-control credential and
opt-in new admission. Completion alone never
imports files or certifies a Run. See [data-model.md](data-model.md).
The separate `ops/worker-frames.clj` provides bounded LF-delimited JSON for
private process pipes: string keys preserve filenames; duplicate fields,
trailing values, invalid Unicode and oversized/deep frames are refused.
A malformed frame or I/O failure poisons the channel rather than retrying.
This is framing, not authentication or executor attestation: every guest
message (including ready/result) is candidate-controlled. Shutdown evidence
comes only from the external controller, never a guest report. The framing
module is not a process launcher or installed transport. The separate
`ops/worker-process.clj` stdio entrypoint can run a single credentialless guest
or a serial inference broker. It captures the protocol streams and redirects
Java and Clojure standard I/O before loading Dvergr. The broker's opening
configuration must come from the trusted parent before forwarding guest
traffic. These endpoints are not installed by server startup and supply no
physical containment by themselves. `build/worker` supplies the minimal pinned
runtime; the local SCI controller retains guest and broker in one finite
systemd unit, with physical capture and durable admission before GO.
The host-only `ops/worker-work.clj` adapter connects these to Dvergr's existing
Protocol/WorldSetup/Evaluator capabilities. It owns no alternative execution
engine or result store. Separate conserved admission allowance funds local
work; remote subscription usage remains explicitly unpriced. Cancellation
retains the native supervisor until remote stop, and output import stays in
the canonical isolated Run world with current permission checks.
The separate `local-worker-workflow` control uses the same provider-independent
saved-workflow preparation as native server execution. It freezes the inspected
bundle, literal parameters, fixture/input overlays and regular-file modes before
admission. Only those files reach the guest. Ordinary workflow source may include
checker/gold files, but authored checkers are not executed and this is not a
benchmark secrecy boundary. Saved workflows reject `:review` and
`:workflow/writes` declarations: this adapter does not silently enable retained
world adoption or whole-world auto-merge. Exact retries observe the original Job; recovery
reconstructs its input only from the stored snapshot, not newer room content.
`ops/worker-benchmarks.clj` is a host-only local-worker adapter for the same
experiment runner: one enrolled candidate, up to eight case/repetition cells,
serial execution and discard settlement. It uses the existing clean fixture
parent and trusted server-side calibration/checker. Guests receive only pinned
fixture inputs, never authoring-room files, checker, gold or calibration data.
Each cell consumes one explicitly funded admission. Before calibration, a
nonclaimable prepared assignment binds its Run to an immutable case/input;
publication is separate and serialized against cancellation. Recovery fences
independently identified siblings even if another child is unbound, but holds
all refunds and retirement until the complete graph and world cleanup are
proven. A pre-binding crash remains an explicit held incident, not a guessed
case or permission to rerun work. Bounded MCP controls and the Workflows backend
selector expose this adapter to authorized operators; enrollment and explicit
admission funding remain separate. Subscription billing is unpriced, and
unreported usage or cost dimensions remain unknown rather than measured zero.
Certified reports preserve independently known billed and notional costs.
The separate host-only `ops/worker-recovery.clj` reconciles orphan exposure
without execution or late import. Missing world disposal authority keeps
cleanup unresolved; a terminal Run or discarded projection is not physical
cleanup proof. Startup invokes it after orphan Run reconciliation and before
MCP claims are accepted. `runtimes/worker-maintenance.clj` owns one periodic
reconciliation resource independent of fast receipt delivery. An initial hold
or unhealthy resource prevents new claims/admission but preserves existing
lease shutdown. `scripts/worker_driver.py` supplies private operator enrollment,
explicit allowance/task/workflow/benchmark submission and one-shot local SCI execution/recovery;
its login binds the exact MCP destination, including explicit tunnel aliases.
Benchmark retirement failures retain their exact process-local cleanup callback
and physical checker permit; reconciliation retries disposal, not execution.
Recorded Attempt evidence is distinct from mutable workspace files. See
[authority.md](authority.md#private-mcp).
Read-only current-workspace Git bundles use Geschichte's Git object/pack
implementation and a synthetic root commit, with no ancestry traversal or
server checkout/export writes. Bounded optimistic downloads reject intervening
workspace changes; they introduce neither a result database nor a retained
export store.

## Stores: one owns each concept

Everything references across a store boundary **by value** — a uuid — never by
Datahike entity id. Branches share an eid counter after a fork, so a cross-store
ref by eid is a bug that shows up as data loss.

| Store | Owns | Referenced elsewhere by |
|---|---|---|
| **system DB** (`.dvergr/system-db`, shared with dvergr) | identity (`:actor/*`, `:party/*`), teams (`:room/*`), grants, the system registry, the KB registry (`:kb/*`), proposals (`:proposal/*`), mail accounts | its uuids |
| **room store** (one per team) | chat messages, the team's book (`:kontor.*`), schedules, and the category-S projection of those | `:room/content-db-scope` |
| **KB store** (one per wiki) | pages, blocks, typed-page properties, and the property declarations for them | `:kb/db-scope` |
| **geschichte repo** (one per team) | the code agents work in | the room's repo system |
| **drive store** | files | `:drive/*` in the system DB |
| **app store** (`data/simmis-v2`) | the application's own furniture | `simmis-scope` |

A room store is provisioned by dvergr and resolved on **that room's execution
context**, not the server's — yggdrasil's registry is context-backed, so looking
a room store up from the server context finds nothing and returns `nil` with no
error. Use `model.room-databases/room-store-conn`, or `(room-conn "slug")` at
the REPL.

Reading chat does not initialize legacy entities or require write permission.
Native Dvergr history is read from the authorized per-room replica even when
there is no `S/ChatRoom` projection. Legacy messages and KB events are included
only when that projection exists; actual posting prepares the legacy room and
author rows before dispatch, rather than issuing writes from the render path.
Chat/thread source scopes come from the current authenticated room roster,
not restored tab hints. Before that roster arrives, or after a room disappears,
the chat does not open or query a cached replica under an unverified label.

## Server and client

The client is not a view onto an API. It runs a **real Datahike replica in the
browser**, backed by IndexedDB and kept current by konserve-sync over a kabel
websocket. Queries are local; the server is the authorization boundary, not a
query API.

That gives two distinct channels, and they are authorized separately:

- **Control plane** — RPC. `defn-spin-remote` in `.cljc` declares a function
  that exists on both sides; the client call travels to the server handler.
  Every inbound call passes `access/authorize-remote` first. A refused or
  failed call reaches the client as a short message, a type and a correlation
  id (`runtimes/rpc_errors.clj`); the exception stays in the server log.
- **Data plane** — replication. konserve-sync subscribes the client to store
  scopes it may read. Subscription is gated; publication from a client is
  refused outright, because sync here is one-directional.

See [authority.md](authority.md) for how each is gated.

Authentication, subscription readiness and replica readiness are separate
boundaries. Each authenticated socket gets an epoch. The shell reestablishes
owned invalidation subscriptions, reads an authoritative room/KB roster, and
reconciles already-open replicas; it does not eagerly open every wiki. Lazy
opens wait for that roster. Revoked scopes are withdrawn immediately, including
historical/proposal signals held by mounted views. Subscription replacement
joins an unsubscribe guarded by the exact old strategy before installing a
replacement. Old epoch callbacks cannot publish into the current views.

On same-peer reconnect, `datahike.kabel.connector/resubscribe!` confirms a fresh
server branch head and completed handshake on the existing connection. Its
optimistic overlay is retained; neither writes nor predictions are replayed.
The shared replica's initial bootstrap also follows epoch changes while still
loading. Previously requested aggregate reports catch up with their latest
options, including requests whose replies were lost with the socket.

`replica-io` owns one native lifecycle slot per Datahike store UUID across peers
and epochs; a different backend configuration cannot reuse that UUID. Server
preparation RPCs run outside this physical lease, so a lost RPC cannot block a
new epoch before construction starts. After construction starts, logical
cancellation does not release physical ownership: replacement waits for the
old constructor and confirmed subscription cleanup. Failed cleanup quarantines
that store with explicit cleanup-only recovery, rather than wiping IndexedDB.
Joined disposal is supported here for the pinned memory/IndexedDB tiered stores,
not arbitrary plugin backends. A peer/account replacement retires the old slots
and clears account projections; same-peer reconnect does not clear editor drafts.

## Three rules that shape everything else

**Writes go through the server.** The client proposes; the server resolves the
store, checks `can?`, and transacts. A client never declares schema —
`:db/ident` is append-only, so a browser that could declare an attribute could
permanently fix its type.

The replica's own writer is never used for this. A write the user should see
at once is a datahike.optimistic PREDICTION over the RPC that makes it durable
(`uis/web/desktop/optimistic_write.cljc`): the RPC answers with the store's
head after the write, the prediction is dropped once the replica reaches that
head, and a refused or failed RPC rejects it and tells the user. `opt/transact!`
would hand the transaction to the replica's KabelWriter — a raw
`datahike.kabel/dispatch`, past the operation-level `can?`. The shared RPC
runtime now routes that function, but Simmis explicitly denies it for every
principal. Neither `d/transact!` on a replica nor konserve writes into a synced
store are authorized application writes;
`browser-write-guard-test` reads every ClojureScript file (and the `:cljs`
branch of every `.cljc`) and fails on any of them.

A wiki widget (`widget_sandbox.cljs`) is browser code too. Its `kb/*` writes
are `block-remote/widget-transact` (data, predicted on the KB's overlay) and
`block-remote/widget-install-attr` (an attribute declaration, sent in order
with the data writes but not predicted, since the replica cannot apply data of
a type it does not know). Widgets write only on a KB tab's trunk. A widget
block carries the db's `:max-tx` in its row and is keyed by it, so it
re-evaluates when what it shows changes.

A write that is not shown at once is still never silent. Every call site routes
its outcome through `uis/web/desktop/remote.cljc`: `rem/invoke!` and `rem/spin!`
report a failure by default, `rem/on-ok`/`rem/on-error` are the two
continuations of a spin-remote invoked from a DOM callback, and
`rem/result-failure` treats a refusal a handler ANSWERS — `{:status :error …}`,
`{:success false …}` — as a failure rather than a result. A setting shown
before the server took it (notification level, code view) is put back when
the server refuses it.

**Aggregates are views over sources.** No persisted aggregate, no denormalised
index. If an aggregate is slow, cache it on a key derived from the sources' own
heads, so a stale entry is impossible rather than unlikely.

Client aggregate RPC gates are owned by the authenticated socket epoch. Feed,
Tasks, Accounting, Proposals and each room's leaderboard coalesce invalidations
while retaining the latest requested parameters. A replacement epoch can start
even if its predecessor never completed. Result writes and completion callbacks
are separately fenced: an old completion cannot release the replacement's gate.
Proposal diff/check requests retain independent epoch fences alongside their row
request IDs, since they normally finish after the parent list request completes.

The internal native-resource slot separates logical cancellation from physical
cleanup. One worker serializes construction, publication and retirement for its
resource. A superseded constructor must return and be disposed before its
replacement opens; retirement barriers do not wait for later replacements to
retire. Failed cleanup quarantines that slot while other stores can progress.
Explicit recovery retries the retained cleanup only. Unknown constructor errors
are not proof of cleanup and do not authorize another native open or cache wipe.

**A failure is a value.** `nil` means never attempted, `{:error …}` means it
broke, a list means it worked. An empty list must never be able to mean
"denied" or "the schema is missing".

## Three seams

Adding a feature touches these and not much else.

**Write** — `block_remote`'s `resolve-conn`. Server-side, scope- and
branch-aware. Authorization happens here because it is the only place that can
see both the actor and the target store.

**Read** — `db_signal/ensure-view-db-signal! [scope ref]`. The single place a
`(scope, reference)` pair becomes a db value; it handles `nil`/`:now` and
`{:as-of T}`. No read path may pick a store by falling back to a default.

**Navigate** — `refs/open!`. The single place a reference becomes a tab.
Aggregate rows carry a `:ref`, and wiki and chat `[[dh://…]]` links produce the
same shape, so a new target kind is added once.

## The UI is not React

The client renders with [spindel](https://github.com/replikativ/spindel) — FRP
signals and spins producing incremental DOM. It has a handful of non-obvious
rules whose violations do not fail loudly; they are listed under "spindel sharp
edges" in [`CLAUDE.md`](../CLAUDE.md), and most UI bugs shipped here came from
breaking one.

Remote calls from the client go through
`uis/web/desktop/remote.cljc` — `invoke!`, `spin!`, `report-error!` — which
surfaces failures to the user by default.

An open wiki page follows its replica. A block's text lives in a TipTap
editor, a foreign node spindel renders the container of and never the
children, so the block's content also rides on the container as
`data-content`, an attribute spindel reconciles on every commit; a
MutationObserver on it hands the committed content to the editor
(`block_editor.cljs`, `follow-replica!`). The content comes from the
effective db — replica plus this session's predictions — so a pending write
of one's own is never overwritten by an older replica value. Every block takes
the change except the one being edited (focused, or holding unsaved typing):
that one keeps its text and shows a notice offering the other version
(`block_sync.cljc`). Blocks added, deleted or moved elsewhere arrive through
the page's `ifor-each`, whose items carry everything a block renders from —
depth, children, the conflict flag.

When a block's editor leaves the page, typed text that was not saved is never
dropped (`block-sync/unmounted`): if the block still exists (the page closed,
the block moved) the text is saved, as leaving the block would; if another
session deleted it, the text is kept in a notice on the page ("This block was
deleted in another session") that restores it as a new block where the old
one was, or discards it (`sig/orphaned-blocks`). Existence is read from the
effective db, and a block this session deleted itself is not kept.

The editor itself — TipTap, which listens on the document, plus the replica
observer — is then destroyed. spindel calls a foreign node's `:on-unmount`
with nil and fires the unmount of the LAST committed vnode, a closure from a
later render than the mount, so a handle closed over in the render body is a
different, empty one by then. `uis/web/desktop/foreign_lifecycle.cljc`
(`owned`) keys what a mount acquired by the node's place in the tree (parent
address, slot, `:key`) and releases it on unmount; the block editor, the
chat input and the Vega chart use it.

The navigation lists the wikis a user owns, those shared with them, and those
reached through a grant to a room they are in (`kbs/sidebar-kbs`), each marked
read-only when `can?` refuses the user `:write` — a room's `:read` grant — and
a read-only wiki's pages open non-editable.

## Where things live

```
dev/user.clj                 lifecycle; dev/repl.clj is the read surface
src/is/simm/
├── model/                   system DB, rooms, parties, KBs + grants, room
│                            content DBs, access (can?), billing, schema
├── runtimes/
│   ├── web.clj              server boot and the HTTP routes
│   ├── web.cljs             client boot
│   ├── http_auth.clj        route auth: the gate and its boot-time validator
│   ├── rpc_errors.clj       what a failed RPC tells the client (no traces)
│   ├── auth_config.clj      kabel-auth store, JWT, accounts from config
│   ├── branching.clj        yggdrasil branching and branch GC
│   ├── storage_gc.clj       periodic orphan GC of every store — OFF unless
│   │                        enabled (see below)
│   ├── screen_intake.clj    screen-share frames → descriptions, per-user
│   │                        store; `screen/*` for agents, through grants
│   ├── transcripts.clj      jigasi watcher + "Transcribe" → drive file and
│   │                        room message; `transcript/*` for agents
│   └── telegram.clj         adapter over dvergr's telegram channel
├── agents/                  personas, prompts, tools, summaries, merges
└── uis/web/desktop/         the spindel UI
    ├── signals.cljc         all UI signals and mutators
    ├── db_signal.cljc       client replicas, tiered stores, konserve-sync
    ├── optimistic_write.cljc a write shown at once, made durable by an RPC
    ├── tab_heal.cljc        open tabs reconciled against the room roster
    ├── backlink_target.cljc what a backlink row opens, or why it opens nothing
    ├── *_remote.cljc        RPC endpoints
    └── views/               columns, chat, nav, proposals, files, …
```

## The stack underneath

| Layer | Project |
|---|---|
| UI | [spindel](https://github.com/replikativ/spindel) |
| Agents | [dvergr](https://github.com/replikativ/dvergr) |
| Database | [datahike](https://github.com/replikativ/datahike) |
| Storage | [konserve](https://github.com/replikativ/konserve) |
| Branching | [yggdrasil](https://github.com/replikativ/yggdrasil) |
| Replication | [konserve-sync](https://github.com/replikativ/konserve-sync) over [kabel](https://github.com/replikativ/kabel), [kabel-auth](https://github.com/replikativ/kabel-auth) JWT |
| Ledger | [kontor](https://github.com/replikativ/kontor) |
| Filesystem | [muschel](https://github.com/replikativ/muschel) |
| Versioned repos | [geschichte](https://github.com/replikativ/geschichte) |
