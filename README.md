# simmis

A self-hosted workspace where people and AI agents share one memory: chat
rooms, a wiki, knowledge bases, an accounting book, and a code repository —
all of it versioned, all of it queryable, all of it the same substrate.

simmis is the reference application for the
[replikativ](https://replikativ.io) stack. If you want to build an agentic
harness or a local-first SaaS on Datahike, this is a worked example of the
whole path: durable storage, client↔server replication, authorization,
branching and review, and a sandboxed agent runtime.

> **Status: early.** This is a "release early" first public cut. It runs, it
> is used daily, and it has rough edges — see [Known gaps](#known-gaps) before
> deploying it anywhere that matters.

## What is in the box

**Rooms.** A room is a conversation with a durable memory. Humans and agents
are the same kind of participant (a *party*), so `@mention` works across both.
Rooms can be mirrored to Telegram.

**Knowledge bases.** A KB is its own Datahike database holding wiki pages,
blocks, links, and typed entities (customers, invoices, contacts). Pages link
with `[[Title]]`; links are stored as datoms, so backlinks and neighborhood
queries are ordinary queries. Cross-database links use an explicit
`[[dh://…][Display]]` form.

**Proposals.** Every governed write from an agent lands on a *fork* — an
yggdrasil branch — not on trunk. A proposal collects forks across KBs, the
book and the room repository, shows a semantic diff against the merge base,
and merges only when someone with `:merge` authority accepts it. This is the
control that lets you be generous with what agents may write.

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
| Replication | [konserve-sync](https://github.com/replikativ/konserve-sync) over [kabel](https://github.com/replikativ/kabel) websockets, [kabel-auth](https://github.com/replikativ/kabel-auth) JWT |
| Ledger | [kontor](https://github.com/replikativ/kontor) — double-entry accounting on Datahike |

The client runs a **real Datahike replica in the browser** (IndexedDB-backed),
kept in sync by konserve-sync. Queries are local; the server is the
authorization boundary, not a query API.

## Quick start

### Prerequisites

- JDK 21+
- Node.js 18+

### Run it

```bash
git clone <this-repo>
cd simmis
npm install
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
registered only when its key is present. The explicit default, used only when
nobody selected a model, is `accounts/fireworks/models/glm-5p2`, so
`FIREWORKS_API_KEY` is the path of least resistance. An unavailable selection
never falls through to that default or another family/provider. Latest stores a
family and follows its newest usable version. An explicit version is preserved
as the preferred version; if withdrawn, it may use only a newer usable version
in the same family and provider, and automatically returns to the preferred
version if it reappears. `OPENAI_API_KEY`
on its own reaches OpenAI (the GPT-5.6 and GPT-5.5
models); add `OPENAI_BASE_URL` to point that key at any other OpenAI-compatible
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

## Dependency sets

Three, selected by alias:

```bash
clj -M:dev         # Maven only — START HERE. Needs nothing but this repo.
clj -M:stack:dev   # + a local ../dvergr checkout, for co-developing the agent substrate
clj -M:local:dev   # + every replikativ sibling as a local checkout
```

`:stack` and `:local` expect SIBLING CHECKOUTS next to this repo (`../dvergr`,
`../spindel`, …) and fail to build a classpath without them, so they are for
people working on the stack itself. `:local` additionally inherits whatever
branch each sibling happens to sit on.

## Tests

```bash
clojure -X:stack:test
```

Note the `-X`: the `:test` alias uses `:exec-fn`, and `-M:test` silently drops
into a REPL instead. Engine-level reactive tests live in the spindel and
dvergr repositories.

## Documentation

Five documents, each about code that exists.

- [`doc/architecture.md`](doc/architecture.md) — **start here.** The stores,
  what owns what, server vs client, and the three seams everything goes through
- [`doc/data-model.md`](doc/data-model.md) — objects, morphisms, instances, and
  where a property's values actually live
- [`doc/authority.md`](doc/authority.md) — one predicate, three enforcement
  planes; what is an ACL, what is fork-and-review, what is a capability
- [`doc/proposals-and-time-travel.md`](doc/proposals-and-time-travel.md) — how
  an agent's write reaches trunk, and reading any store as of a past point
- [`doc/agents.md`](doc/agents.md) — what an agent is, what it may do, and
  where its writes land
- [`CLAUDE.md`](CLAUDE.md) — working in this repository: the REPL workflow,
  spindel's sharp edges, the logging policy

## Known gaps

Being explicit, because the list selects who should use this today:

- **Authorization has no integration-level test.** The `can?` predicate, the
  control plane and the data-plane gates all have unit coverage, but nothing
  exercises them together against a real system DB and real grants.
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
  require re-creating stores.
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
