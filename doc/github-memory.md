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
local Geschichte commits. **Pull** integrates incoming changes
with that saved history through a frozen Proposal; review and acceptance are
separate from fetching. **Push** prepares outbound publication separately;
it requires this human's GitHub access and an explicit reviewed send. Automatic
fetching is not enabled by linking a repository.

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
The same operation also bounds pack visits, pack metadata parts and cumulative
index reads; object-size limits alone do not bound catalog discovery.

The pending native Room has no Simmis membership, content scope, roster entry or
registered agent access until import, foundation checks and the final current
human authorization pass. Unknown native cleanup remains owned; observing an
uncertain creation request never fetches or creates again.

## Incoming changes and Proposals

**Pull** captures the original authorized Room, source and physical
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

## Outbound publication

`is.simm.ops.github-push` implements outbound preparation and settlement inside
an already admitted original HOST operation. Browser RPCs derive the human from
the authenticated connection; no agent or MCP push API is installed. It uses a
separate typed journal protocol sharing the Room's
pending Git slot. The retained connection must match both the physical target
and current source repository/ref; changing repository configuration does not
retarget an existing connection or its cursor.

Preparation verifies the exact remote head, projects a finite incremental pack,
and records generated Git identities under the originally inspected native
checkpoint. Concurrent edits refuse preparation rather than being incorporated
implicitly. The sent commit snapshot and post-mapping source checkpoint have
separate retained roots. Binary packs use typed content-addressed store refs,
with size and identity verified before dispatch; they are not EDN strings or
byte arrays embedded in artifact metadata.

Only the original positive Ready-to-Sending transaction acknowledgement permits
one remote compare-and-swap update. A positive result records the outcome and
upstream cursor atomically. Already admitted completion facts remain recordable
if permissions are revoked or the App is fenced while the remote call returns;
the original operation still owns its physical ticket. This completion path
cannot admit another send.

An uncertain send stays fenced. Explicit recovery can observe the exact tracked
remote ref and record a **new upstream observation**, while preserving that the
historical send outcome is unknown. Observing the prepared new OID can advance
the cursor; observing the old OID leaves it unchanged. A different remote OID
holds for reconciliation. None of these paths resends. A Room administrator can
close known unsent or terminal attempts while preserving original authorship;
an unknown Sending attempt cannot be discarded or administratively retired.

**Memory settings**, available in the file/workspace actions menu, contains the
remote identity, tracked branch and this human's provider access. The page shows
only Pull, Push and sync status; review details open in separate dialogs.
Last-sync time comes from the durable integration cursor transaction, not a
timer or local refresh. The interface describes a linked remote; GitHub is the
currently supported transport/authentication adapter. Arbitrary Git hosts and
direct Geschichte remotes are not enabled by this UI consolidation.

**Your GitHub access** stores a credential for this human and this exact physical Git
connection. Room membership never shares a token. Replacement and disconnect
require the inspected connection and credential revision. A stale settings form
cannot overwrite rotated access or retarget its token to another connection.
The encrypted envelope uses the existing server credential key, with a
domain-separated identity and authenticated human/connection body. Only access
status and identity/revision metadata return to the browser; neither ciphertext
nor token enters Room files, push artifacts or receipts. The password input is
uncontrolled and clears on submission; tokens are never stored in UI signals.

Authenticated transport permits only the canonical repository's four Git
advertisement/RPC destinations, without redirects. Current credential revisions
are checked before requests and in the Sending admission writer. A rotated or
revoked credential invalidates an older push review. Provider exceptions are
sanitized before they leave the transport; cancellation retains interruption.
Configured access is used for explicit unknown-send reconciliation. Because
imports currently support public sources only, a human without configured
access can still observe public refs; a selected credential failure never
retries anonymously. Closing a known terminal attempt requires no token.
