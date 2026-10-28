# Product knowledge targets and proposal settlement

Room conversations, Runs, execution worlds, and Proposals have different roles.
A Proposal records reviewable contributions; it is not itself an interpreter or
an atomic transaction spanning every attached storage system.

## Current product-KB boundary

Simmis attaches product KBs through room grants. Those KB systems are registered
on the server execution context. The ordinary conversational room context may
not contain a corresponding system. This legacy explicit proposal surface is
separate from Dvergr's retained Run-world settlement bridge.

`resolve-kb-conn!` is the shared boundary for `kb/*` operations and
`knowledge_add`. It checks current room/agent authority and resolves the active
proposal on each operation. It never uses the installation-time connection as a
fallback. Explicit unknown KB names are refused.

Only the server context or the owning live room context may use this product-KB
surface. A different execution world must not borrow root authority through the
closure; it must use its own world-aware facilities. This resolver is not an
implementation of generic child-world KB access.

- Reads use an existing proposal branch, otherwise the authorized product trunk.
  Reads do not mint branches.
- Governed writes create or reuse the proposal branch. A missing branch refuses
  the operation. A filed proposal refuses writes until explicitly reopened or
  released.
- Direct writes select the authorized product trunk when no proposal is active.
- Tool construction, campaign membership changes, and descriptor recovery do not
  change these rules. Runtime handles must be reconstructed separately from
  durable branch/base/identity descriptors.

The recovery regression rebuilds a tool against a round-tripped descriptor. It
proves routing after descriptor restoration, not whole-process restart recovery.

## Identity and settlement

Two page UUIDs claiming one unique title are an identity conflict. The merge
preflight reports this before any proposal contribution lands, including when a
caller asks to force acceptance. Actual KB landing checks it again. Equal titles
are not sufficient authority to silently combine pages, references, and content.

Each storage contribution settles independently. Already accepted contributions
are durably recorded and skipped on retry. Preflight reduces partial acceptance;
it does not establish a distributed atomic transaction or eliminate races. The
backend still enforces its constraints at write time.

The frontend refreshes canonical contribution statuses while retaining an action
failure until the next action clears it. A reactive refresh must not turn a
partial acceptance into an unexplained, apparently fresh Accept button.

## Regression coverage

The targeted tests cover SCI/tool read-your-own-writes, saved-descriptor routing,
closed proposals, explicit trunk work, campaign changes, current authority,
missing targets, unrelated execution contexts, owning-room access to server KBs,
reads without branch creation, full/per-contribution identity blocking, and
failure feedback across canonical refreshes.

## Independent checks

Proposal filing and its subsequent code check are different outcomes. A known
clean SCI affine-value refusal during check-fork construction is an explicit
`:error` check result, not a failed filing and not a passing test. Exceptions
carrying construction recovery ownership, a cause, suppressed cleanup failures,
or unknown data propagate intact; the released native constructor distinguishes
incomplete cleanup with its own wrapper. Successful fork cleanup retains its
existing owner and failure behavior. Infrastructure check errors are retryable
without changing the branch head; successful branch evidence remains cached.
