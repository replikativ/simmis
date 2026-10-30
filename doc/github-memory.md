# GitHub Memory

The GitHub source imports a public repository into a normal file-backed Memory.
It preserves source file bytes and imported Git ancestry. The initializer adds
only the repository's schema and governance foundation: no starter files,
workflow installation, local Git commit, agent registration or source-code
execution. Ordinary Markdown needs no Simmis identity. Malformed explicitly
authored Simmis metadata remains a source repair diagnostic.

**Add Memory → GitHub repository** opens the fixed import form. Geschichte
0.1.41 includes the reviewed finite import budget, native HTTP deadlines and
inspected-target checkpoint precondition, plus fixes for whitespace-only text,
invalid UTF-8 and overlapping content-merge regions.
Its bounded merge planner is a prerequisite for the forthcoming Proposal path;
the current public sync still uses fast-forward-only native publication.

New imports publish their exact source Git OID, native head, physical connection
identity and retained creation receipt in the same control transaction as
Memory membership. Failure before that transaction publishes neither membership
nor an integration row; an uncertain acknowledgment is observed using the same
creation request, never by cloning again. Existing Memories are not assigned
inferred upstream metadata. This metadata does not itself pin native snapshots
against GC or enable merging local edits, pushing, or automatic sync.

A positively acknowledged fast-forward whose reviewed base matches the
observed integration record advances that record in the
same control transaction as its `:applied` sync outcome, retaining the initial
import anchor and linking the new head/OID to that exact outcome receipt. The
writer compares the whole observed integration record and checks its base
against the reviewed upstream head before native publication. Unchanged heads
leave the record unchanged. A lost native ACK followed by observation of the
desired files does **not** advance it: that observation proves no historical
integration. Previously stale metadata remains unchanged rather than being repaired from file
equality or ancestry. Memories with absent or already stale metadata keep their
existing checkpoint-guarded fast-forward behavior, leaving that metadata
untouched. This path does not invent an integration base for them; future
three-way integration must reconcile positive receipts or explicitly establish
a new base before using such metadata.

## Import limits

Only HTTPS GitHub owner/repository sources are accepted. Credentials, alternate
hosts, ports, redirects, source-selected transport options and imported shell
helpers are not used. The selected default branch becomes the fixed tracked
branch; subsequent changes to upstream's default branch do not retarget sync.
Source provenance lives in the control DB, outside imported repository files.

The current alpha supports regular and executable files, up to 1,024 files,
4 MiB per Git object and 32 MiB checked out, with additional finite pack,
decompression, object-count, depth and operation-time limits. Complete visible
metadata must also fit 256 KiB, so a later retained review fits its artifact
bound. Symlinks and
submodules are refused before publication. Files are not rewritten to fit the
limits. One HOST Git operation is admitted at a time without a waiting queue.

The pending native Room has no Simmis membership, content scope, roster entry or
registered agent access until import, foundation checks and the final current
human authorization pass. Unknown native cleanup remains owned; observing an
uncertain creation request never fetches or creates again.

## Review and apply

**Sync from GitHub** makes a private exact-checkpoint workspace, then performs a
bounded full fetch and fast-forward-only pull there. It records the original
target, branch, source checkpoint and native allocation ACK before fetching.
The review is ready only after the private connection has positively released.
The retained branch keeps the prepared snapshot available between user actions.

The review lists changed paths and links to the exact upstream Git comparison.
**Apply reviewed update** uses the existing native `workspace/advance!` with the
reviewed physical destination checkpoint. Its serialized writer checks the same
destination worktree, staging index, configuration and refs. Local canonical
edits or divergent history refuse publication; there is no reset, push,
automatic merge or second fetch. Unsaved editor drafts retain the usual Memory
conflict behavior. Native commit hints refresh the existing Memory observation.

Editor autosaves create local Geschichte commits. These currently lack the Git
identity needed by this import-only path, so even restoring the previous file
bytes does not make that history eligible for sync again. Sync refuses rather
than resetting those commits; local commit projection and merge are follow-ups.

An ambiguous Apply ACK is never resent. **Check this sync** only observes the
same journal. **Verify current outcome** may report `satisfied-observed` when
the current exact target and complete visible state match the retained review;
that is not a historical claim about who applied it. Later changes, unavailable
history or mismatched physical identity hold for inspection.

While preparing an update, Simmis attempts to retain a verified integration
cursor's upstream base on its original physical store. A durable control intent precedes the
native root write; only its positively acknowledged fresh winner creates the
root. Retries verify the original registry entry and complete immutable root
record. An uncertain acknowledgement never authorizes recreating a missing or
partial root. This is storage retention, not proof that changes were reviewed
or accepted, and it does not advance the integration cursor. Preparation records
whether retention succeeded, is unavailable, or is held. Retention failures do
not block the existing independent fast-forward protocol; future three-way
Proposal integration must require a verified native root.

Simmis installs a permanent managed-GC prohibition in a fresh JVM before server
admission. A configuration enabling storage GC fails startup. Direct operator
collectors or a separately collecting process sharing these stores are outside
this alpha operating profile. Durable roots remain necessary; default-off GC
alone does not retain a base. These Git roots are kept permanently for now, so
their count grows with distinct retained bases. Coordinated GC and root
retirement remain follow-ups to the upstream lifetime/GC work.

Imports without verified integration metadata retain their existing
fast-forward behavior. They do not acquire an invented three-way merge base.

Discard removes only the positively owned private branch after physical exit.
Its exact checkpoint, or bounded native ancestry from the acknowledged origin,
must match. A new Room owner can explicitly retire a previous owner's known
preparation without impersonating them or publishing their changes. Unknown
allocation, control handoff, connect or release outcomes retain the original
native cleanup owner.
Those uncertain native phases require operator reconciliation; normal owner
retirement cannot bypass their retained exclusive physical slot.
A moved default repository or unavailable original store requires operator
reconciliation; it never authorizes cleanup of a replacement repository.

Selective native advancement transfers committed files and Git identities, not
the complete imported pack store. Later sync therefore uses a full fetch with
empty `haves`; thin negotiation and push are not supported. Rewritten upstream
history or missing ancestry may refuse rather than prove divergence.
