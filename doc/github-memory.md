# GitHub Memory

The GitHub source imports a public repository into a normal file-backed Memory.
It preserves source file bytes and imported Git ancestry. The initializer adds
only the repository's schema and governance foundation: no starter files,
workflow installation, local Git commit, agent registration or source-code
execution. Ordinary Markdown needs no Simmis identity. Malformed explicitly
authored Simmis metadata remains a source repair diagnostic.

**Add Memory → GitHub repository** opens the fixed import form. New imports
publish their exact source Git OID, native head, physical connection identity
and retained creation receipt in the same control transaction as Memory
membership. An uncertain acknowledgment is observed using the same creation
request, never by cloning again. Existing Memories are not assigned inferred
upstream metadata.

The file browser and editor use this ordinary Memory. Editor autosaves create
local Geschichte commits. **Sync from GitHub** now integrates incoming changes
with that saved history through a frozen Proposal; review and acceptance are
separate from fetching. Pushing and automatic fetching are separate follow-ups.

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

## Incoming changes and Proposals

**Sync from GitHub** captures the original authorized Room, source and physical
checkpoint, then creates a private exact-checkpoint workspace. A bounded full
fetch and three-way merge combine the retained upstream base, saved local
history and fetched upstream revision. The same finite operation covers fetch,
content decoding and merge planning. Historical content is read from its own
immutable commit snapshot, including when its representation is absent from the
current mutable catalog.

A clean integration files an ordinary frozen Proposal and opens its change set.
Filing does not change the live Memory or its integrated-upstream cursor.
Base, local, incoming and candidate snapshots have durable retention evidence;
the private native connection must positively release before review is ready.
Imported code is not executed and imported type definitions do not grant
validation authority. The candidate must satisfy the Memory's current document
types and properties.

**Accept Git changes** obtains a fresh semantic review bound to the exact
physical target checkpoint. Native advancement refuses intervening target
changes; recheck before accepting a changed Memory. A positively returned
advance records Proposal acceptance and the fetched upstream cursor together in
the control writer. The cursor names the upstream commit, not the merged local
candidate. Existing Memory observation refreshes files reactively.

A lost acceptance acknowledgment is never inferred from matching file bytes or
resent. An unresolved publication reservation also never permits a duplicate
Proposal. Only a positively known pre-effect refusal can release that
reservation. Repeated requests for an open incoming revision return its existing
Proposal. After acceptance, it remains available as a closed audit.

Conflicting changes are held without overwriting saved files. The current alpha
also holds Markdown deletions and moves to preserve document identity. Unsupported
history rewrites, missing ancestry, invalid documents and resource limits are
reported as held attempts. Close a positively released preparation before making
another attempt; unknown native outcomes require reconciliation.

## Retention and operation ownership

Simmis retains a verified integration cursor's upstream base on its original
physical store before three-way preparation. A durable control intent precedes
the native root write; only its positively acknowledged fresh winner creates
that root. Recovery verifies the same original root rather than recreating an
absent or partial result. The candidate's additional roots follow the same
intent-before-creation discipline. Retention is not review, acceptance or a
permission grant.

Simmis installs a permanent managed-GC prohibition in a fresh JVM before server
admission. A configuration enabling storage GC fails startup. Direct operator
collectors or a separately collecting process sharing these stores are outside
this alpha operating profile. Durable roots remain necessary; default-off GC
alone does not retain a base. Git roots are kept permanently for now. Coordinated
GC and root retirement remain follow-ups to the upstream lifetime/GC work.

Discard removes only the positively owned private branch after physical exit.
Its exact checkpoint, or bounded native ancestry from the acknowledged origin,
must match. A new Room owner can explicitly retire a previous owner's known
preparation without impersonating them or publishing their changes. Unknown
allocation, handoff, connection or release outcomes retain their original native
cleanup owner and exclusive physical slot. A moved repository or unavailable
original store never authorizes cleanup of a replacement.

## Earlier fast-forward attempts

Previously created fast-forward preparations retain their original protocol;
they are not reinterpreted as incoming Proposals. Their reviewed update advances
only the tracked branch under its captured destination checkpoint. A positively
acknowledged update advances verified upstream metadata in the same outcome
transaction. A later observation of matching files is only `satisfied-observed`
and does not establish who performed the update. Missing or stale integration
metadata is never repaired by guessing from file equality.

Selective native advancement transfers committed files and Git identities, not
the complete imported pack store. Fetch therefore currently uses empty `haves`;
this feature does not enable thin negotiation, force push or automatic sync.
