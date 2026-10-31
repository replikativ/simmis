# Preserving knowledge upgrades for existing Memories

A Memory remains the same Geschichte repository. The knowledge profile adds the
semantic schema and definition vocabulary needed for typed Markdown properties
and canonical document views. It does not require replacing its repository,
rewriting Markdown, importing its Git history into a new store, or recreating a
Room. Daily summaries and ordinary file workflows also work on plain Geschichte
Memories without this upgrade.

New knowledge Rooms already receive the profile during pristine provisioning.
Older Rooms are deliberately preserved on reopen. Their explicit upgrade must
not call a pristine initializer, `store/install!`, or the content-seeding path.

## Scope and invariants

The first upgrade supports ordinary Geschichte stores, not partial installations
or mixed legacy wiki databases. It installs missing compatible declarations and
the shared categorical definition transactions. It seeds no example pages,
users, agents, chat rooms, workflow files, accounting accounts or allowances.
Canonical page-title uniqueness takes precedence over generic declarations.
Definition timestamps are frozen by the upgrade intent for restart recovery.
The initial operation also refuses a repository used as another Room's default:
that case needs coordinated receipts for every affected personal-room bootstrap.

Before installation, an immutable prospective DB must pass the existing profile
and bounded canonical document-view checks. Ordinary Markdown without an
explicit identity is accepted provisionally without source edits. Invalid IDs,
conflicting typed declarations, incompatible schema and capacity limits hold the
upgrade. A failed preview writes nothing.

The repository phase is one additive transaction. It uses Dvergr's established
commit-ID/max-tx guard on the actual writer snapshot; prospective `d/with` and
Markdown validation run outside the writer. Caller-supplied transaction data is
ignored. The repository identity, Git refs/head, worktree and staging descriptors
are retained exactly. Immutable content IDs protect source bytes without
reading entire videos or Git packs merely to upgrade schema.

An operator must own the Room, retain current admin and merge authority, and
retain the existing owner grant from the Room to its exact default repository.
Open or unknown proposals, unfinished forks, enabled schedules/Git polling,
active occurrences and unresolved Git operations hold the operation.

## Maintenance boundary

There is no live upgrade RPC or automatic page-open migration. The initial
operator path requires a coordinated stopped-service maintenance interval:

The compatible application must install the trusted profile's legacy Room
registration attestation in both eager registry hydration and later `live-room`
lookups. A Dvergr build that only accepts the original ready/version-1 flags is
insufficient: an upgraded legacy Room deliberately retains its absent flags.
Verification must exercise full cold native Room registration, not just opening
the Datahike stores or reusing an already registered Room.

1. Gate ingress, quiesce work, stop the service and verify its recursive cgroup
   is empty. Runtime-mask `simmis-dev` so the service cannot restart mid-upgrade.
2. Retain a verified coordinated backup of control and repository data, code,
   configuration and file/identity inventory. Exclude independent writer
   processes for the whole interval.
3. Run the CLI in a fresh process with exclusively owned existing connections.
4. Close both connections and join Kabel retirement before cold-starting the
   compatible application. Verify source/history, Room bootstrap, semantic
   views and existing grants before restoring ingress.

An App/Room lifetime lease is not an exclusion lock. The CLI checks the masked,
inactive service, empty service cgroup and closed application/admin listeners.
Without a backend-enforced exclusive lock, exclusion of independently launched
writers remains an explicit operator/harness assumption.

The CLI accepts bounded private EDN, refuses symlink/physical path aliases and
missing databases, and never creates a database. It pins the control store ID
from protected live inventory and the canonical `.dvergr/system-db` path.
The repository must be the exact journaled registry scope's `datahike`
subdirectory; a copied store retaining the same UUIDs is insufficient. Recovery
matches the full frozen manifest request to the journal before opening that
repository. This initial CLI intentionally targets the standard `simmis-dev`
installation layout.

## Journal and recovery

`inspect!` returns a closed inspected intent. `upgrade!` records `:preparing`
before repository mutation. The repository records the upgrade ID and source
basis in its atomic foundation transaction. After observing that receipt,
unchanged file/Git state and a valid canonical view, recovery reattaches the
synchronous accounting/Attempt governors and advances to `:repository-ready`.

Registry publication is last: one current-authority control transaction changes
`:system/repository-profile` and the journal to `:published`. Legacy personal
Rooms remain historically legacy; they retain their original creator and
provisioning metadata. Account bootstrap accepts the canonical profile only
with the completed receipt bound to that exact Room and default system.

A lost acknowledgement is not permission to reinstall. `recover!` inspects the
actual repository receipt and stored intent. It does not infer installation
from a phase label. A mismatched target, changed source, changed implementation
or absent receipt at a later phase holds the operation. Recovery after a
successful publication returns its verified outcome without replaying source
installation.

Ordinary registration subsequently matches the completed control journal to the
native upgrade ID/original basis, repository/store/system/scope, profile and
mandatory foundation. It never requires current files to equal the historical
upgrade source hash. Later edits, including malformed Markdown needing raw
repair, must not make the Room itself unloadable.

The fixed entrypoints live in `is.simm.ops.offline-memory-upgrade`. The private
CLI is invoked from the staged compatible checkout, for example:

```sh
clojure -J-Xmx1024m -M -m is.simm.ops.offline-memory-upgrade-cli /protected/upgrade-manifest.edn
```

The manifest has exactly `:version`, `:operation` (`:inspect`, `:upgrade`, or
`:recover`), `:installation-root`, `:evidence-root`, `:control-config`,
`:repository-config`, `:expected-control-store`, `:request`, `:inspection-file`
and `:result-name`. The frozen request contains `:id`, `:actor`, `:room`, `:at`
and `:target` (`:system`, `:scope`, `:repository`, `:store`). Inventory must come
from the original authorized live binding before stopping the application.
Results are create-new private, flushed evidence files; never replace an
uncertain result or silently change the request ID to retry.

Before ingress reopens, failed-state evidence and the coordinated stopped
backup permit deliberate restoration. After users resume work, that old backup
must never be restored blindly over their new changes.
