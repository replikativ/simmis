# simmis

A self-hosted workspace where people and AI agents share one memory: chat
rooms, a wiki, knowledge bases, an accounting book, and a code repository —
all of it versioned, all of it queryable, all of it the same substrate.

simmis is the reference application for the
[replikativ](https://replikativ.io) stack. If you want to build an agentic
harness or a local-first SaaS on Datahike, this is a worked example of the
whole path: durable storage, client↔server replication, authorization,
branching and review, and a sandboxed agent runtime.

> **Status: alpha.** Simmis is actively developed and has rough edges. Read
> [Known gaps](#known-gaps) before using it with sensitive or irreplaceable data.

## What is in the box

**Rooms.** A room is a conversation with a durable memory. Humans and agents
are the same kind of participant (a *party*), so `@mention` works across both.
Rooms can be mirrored to Telegram.

**Memory.** New canonical workspaces present one hierarchical file tree.
Markdown files are wiki knowledge, with stable page/block identities and typed
properties. Supported source, reading, rich-text and structure views share the
same revision-checked files. Code files offer source/Superficie views and an
explicit room SCI REPL. Word documents have simplified reading, selective text
corrections and advanced XML editing; Word page layout is not reproduced.
Legacy knowledge bases remain available separately and are not automatically
migrated. See [the document model](doc/documents.md) for syntax and properties.

**Proposals.** Agents can work in isolated worlds and submit their changes for
review. A proposal collects forks across KBs, the
book and the room repository, shows a semantic diff against the merge base,
and merges only when someone with `:merge` authority accepts it. This is the
control that lets you be generous with what agents may write. Direct room
operations remain subject to current permissions; not every operation is
forced into a fork.

**Time travel.** Stores keep history, so any KB can be read `as-of` a point in
time, and the history subway shows how a page got to where it is.

**The book.** Each room has a governed double-entry ledger
([kontor](https://github.com/replikativ/kontor)). Entries are validated in the
writer — an unbalanced entry is rejected — so an agent cannot corrupt the
ledger by getting the arithmetic wrong.

**Agents.** Agents run in an SCI sandbox with a curated vocabulary
(`wiki/`, `kb/`, `kontor/`, `proposal/`, `workflow/`, a shell over a
[muschel](https://github.com/replikativ/muschel) mount, and the ordinary
`datahike.api`). They can schedule recurring workflows, write wiki pages,
post to the book, and open proposals against the room's code repository.

**Workflows and Schedules.** Versioned room files define reusable work that can
run manually, be benchmarked on retained cases, or be activated on a schedule.
New canonical workspaces include an editable wiki-maintenance workflow,
installed inactive. Background execution requires a configured model, a finite
allowance and explicit activation. Results and file proposals remain
inspectable; setup does not fund or merge them automatically. Existing rooms
can install the starter without overwriting customized source.

**Intake.** Web-clipper browser extension, mail accounts, screen capture, and
voice notes all land in the same substrate.

## The stack

| Layer | Project |
|---|---|
| UI | [spindel](https://github.com/replikativ/spindel) — FRP signals/spins with incremental DOM. **Not React.** |
| Agents | [dvergr](https://github.com/replikativ/dvergr) — discourse rooms, SCI sandbox, turn factory, schedulers |
| Database | [datahike](https://github.com/replikativ/datahike) — Datalog, durable, history-preserving |
| Storage | [konserve](https://github.com/replikativ/konserve) — pluggable KV (file, S3, IndexedDB) |
| Branching | [yggdrasil](https://github.com/replikativ/yggdrasil) — copy-on-write branches over Datahike |
| Replication | [konserve-sync](https://github.com/replikativ/konserve-sync) over [kabel](https://github.com/replikativ/kabel) websockets with integrated JWT authentication |
| Ledger | [kontor](https://github.com/replikativ/kontor) — double-entry accounting on Datahike |

The browser maintains IndexedDB-backed Datahike replicas for authorized shared
and room data. Replica queries run locally. Canonical Memory files also use
bounded, authorized server operations, reactive change notifications and
on-demand file loading. The browser does not replicate the entire owned file
repository. The server checks authorization at these boundaries.

## Quick start

### Prerequisites

- JDK 21+
- Node.js 18+

### Run it

```bash
git clone <this-repo>
cd simmis
npm install
clj -X:deps prep   # compile Java sources in pinned Git dependencies
clj -M:dev
```

One command starts everything:

| | |
|---|---|
| Web UI | http://localhost:8080 |
| Websocket server | ws://localhost:47295 |
| nREPL (server, Clojure) | 47888 |
| nREPL (browser, ClojureScript) | 9631 |

Data lives in `data/simmis-v2` (app store) and `.dvergr/` (system DB, room
stores, JWT secret). To start over, delete both.

A second instance on the same machine moves all four ports with
`SIMMIS_PORT_OFFSET` and its state with `DVERGR_HOME` and `SIMMIS_DATA_DIR`;
`config.example.edn` documents them.

### Log in

There is no self-registration — accounts come from config:

```bash
cp config.example.edn config.local.edn
$EDITOR config.local.edn        # set your email, handle and password
```

`config.local.edn` is gitignored. Restart, then log in at
http://localhost:8080. The example file also documents the Telegram mirror,
external identity providers, and every environment variable simmis reads.

### LLM providers

Agents need a model. Provider keys come from the environment —
`FIREWORKS_API_KEY`, `OPENAI_API_KEY`, `ANTHROPIC_API_KEY`; a provider is
registered only when its key is present. Configure the default using
`SIMMIS_DEFAULT_MODEL` or `:models {:default "<id or family>"}` in config.
Without either, the compiled-in preference is the Fireworks GLM family,
`accounts/fireworks/models/glm-*`. An unavailable selection
never falls through to that default or another family/provider. Latest stores a
family and follows its newest usable version. An explicit version is preserved
as the preferred version; if withdrawn, it may use only a newer usable version
in the same family and provider, and automatically returns to the preferred
version if it reappears. `OPENAI_API_KEY`
authenticates the OpenAI provider; add `OPENAI_BASE_URL` to point that key at another OpenAI-compatible
endpoint, including a local one. Fireworks always uses its own base and
`FIREWORKS_API_KEY`; credentials are never shared between providers. Each
agent's model can also be set per room in its settings. The picker keeps every
curated family visible and marks rows as available, credential-required, not
supported, unavailable to the account, or temporarily unreachable; only
available rows can be newly saved, and only an available resolved target can
run. New agents inherit their owner's validated preference (then the product
default) without storing a model; the per-agent picker identifies inherited
state, and clearing an explicit override returns to that inheritance chain.

Resolution and activation are separate. A participant resolves the current
configuration when it joins or rejoins, captures the resulting provider/model
spec, and reuses that spec for subsequent turns. Owner-preference and catalog
changes update configured preference, desired resolution, and availability;
they do not rewrite an already joined participant. An explicit agent edit
resets that agent's joined participants, so the next dispatch rejoins and
resolves again. The configuration UI says “Resolves to” or “when next joined”
and reports active runtime state as uninspected.

### Production build

```bash
npm run release        # optimized JS + CSS
clojure -T:build uber  # uberjar
```

A server needs, beyond `SIMMIS_JWT_SECRET`: `SIMMIS_JITSI_URL` (calls point at
`localhost:9100` without it, and boot warns), `SIMMIS_JITSI_JWT_SECRET`, and a
persistent `SIMMIS_CREDENTIAL_KEY` (stored mail passwords are undecryptable
after a redeploy that loses the generated key). Storage GC is off unless
enabled. `config.example.edn` documents each.

## Dependency sets

Three, selected by alias:

```bash
clj -X:deps prep   # once after installing/updating pinned Git dependencies
clj -M:dev         # published/pinned dependencies; no sibling checkouts needed
clj -M:stack:dev   # + a local ../dvergr checkout, for co-developing the agent substrate
clj -M:local:dev   # + every replikativ sibling as a local checkout
```

`:stack` and `:local` expect SIBLING CHECKOUTS next to this repo (`../dvergr`,
`../spindel`, …) and fail to build a classpath without them, so they are for
people working on the stack itself. `:local` additionally inherits whatever
branch each sibling happens to sit on.

## Tests

```bash
clojure -X:test          # released Maven dependency baseline
clojure -X:stack:test    # optional sibling Dvergr development
npm run test:browser    # execute CLJS tests in isolated headless Chrome
```

Note the `-X`: the `:test` alias uses `:exec-fn`, and `-M:test` silently drops
into a REPL instead. Engine-level reactive tests live in the spindel and
dvergr repositories.

The browser test command needs Node.js 22 or newer and Chrome/Chromium
(`CHROME_BIN` can select the executable). It serves only compiled test assets
on a temporary loopback port and uses a fresh, disposable browser profile; no
Simmis server, account or model credentials are needed. An explicit completion
event determines success, and zero tests, assertion failures, browser errors or
a total deadline failure produce a nonzero exit. Both CI configurations run
this gate in addition to the production frontend build.

## Documentation

Five documents, each about code that exists.

- [`doc/architecture.md`](doc/architecture.md) — **start here.** The stores,
  what owns what, server vs client, and the three seams everything goes through
- [`doc/data-model.md`](doc/data-model.md) — objects, morphisms, instances, and
  where a property's values actually live
- [`doc/authority.md`](doc/authority.md) — one predicate, three enforcement
  planes; what is an ACL, what is fork-and-review, what is a capability;
  [bounded MCP task/workflow recipe](doc/authority.md#a-bounded-mcp-workflow-recipe)
- [`doc/proposals-and-time-travel.md`](doc/proposals-and-time-travel.md) — how
  an agent's write reaches trunk, and reading any store as of a past point
- [`doc/agents.md`](doc/agents.md) — what an agent is, what it may do, and
  where its writes land
- [`CLAUDE.md`](CLAUDE.md) — working in this repository: the REPL workflow,
  spindel's sharp edges, the logging policy

## Known gaps

Being explicit, because the list selects who should use this today:

- **Alpha security.** Authorization has integration coverage over real grants,
  inbound RPCs, data-plane gates and guarded native publication. This is not a
  security audit; some surfaces have the limitations listed below.
- **Blob reads are authenticated, but not authorized.** Any signed-in user can
  fetch any blob by hash — drive files, mail attachments, pasted images,
  screen-recording chunks. Scoping a blob to a room would need a record of
  which rooms referenced it, and blobs are content-addressed and deliberately
  shared, so the same bytes in two rooms are one blob. Revoking someone's
  access to a room therefore does not revoke their ability to fetch its blobs
  if they kept the hash.
- **The agent sandbox is a soft boundary.** It is an SCI context with a
  curated vocabulary, hardened against the escapes we found, but it is not a
  VM. Do not run untrusted agents against secrets you cannot rotate.
- **No migration tooling.** Create-time-fixed store flags mean some upgrades
  require re-creating stores. Existing wikis are not automatically converted
  to canonical Memory, and file export does not recreate every governed type
  definition.
- **Fulltext installation.** Late fulltext installation on a populated
  shared-writer store may be unavailable; the supported installation path is
  tracked in [Datahike #1136](https://github.com/replikativ/datahike/issues/1136).
- **Set `SIMMIS_JWT_SECRET` in production.** Without it a signing secret is
  generated once and persisted to `.dvergr/jwt-secret`, i.e. in the same
  directory as the data it protects. Fine for development, not for a server.
- **Room apps are private.** `/apps/<slug>/` requires membership of the room
  the app belongs to. They were reachable by anyone who knew the slug, which
  was not a decision anyone made; making them publishable again should be a
  per-room flag rather than a return to open-by-default.
- **Single-node.** Replication between peers works; multi-server deployment
  has not been exercised.

## Contributing

Issues and pull requests welcome — see [`CONTRIBUTING.md`](CONTRIBUTING.md).

Two things worth knowing before you start: the UI is spindel, not React, and
the "spindel sharp edges" section of [`CLAUDE.md`](CLAUDE.md) lists the
non-obvious rules that most of our shipped UI bugs came from breaking. And
authorization is declared, not implied — an HTTP route without an `:auth` key
stops the server from starting, on purpose.

## License

MIT — see [LICENSE.md](LICENSE.md). Deliberately permissive: building a
commercial product on this should not require a licensing conversation.

Note that the libraries underneath (datahike, konserve, kabel and the rest of
the replikativ stack) carry their own licenses; MIT here covers this
repository's own code.
