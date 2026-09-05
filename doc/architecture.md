# Architecture

How simmis is shaped, and where to look for each thing. Read this before the
code; the rest of `doc/` assumes it.

## The one-sentence version

A Clojure server owns a set of Datahike databases and is the only writer; a
ClojureScript client holds a real Datahike replica of the ones it may see and
queries it locally; agents run in a sandbox on the server and write to
branches rather than to trunk.

## Processes and ports

One JVM runs everything in development (`clj -M:dev`):

| | |
|---|---|
| Web UI | http://localhost:8080 |
| WebSocket (kabel) | ws://localhost:47295 |
| nREPL — server, Clojure | 47888 |
| nREPL — browser, ClojureScript | 9631 |

`dev/user.clj` is the lifecycle; `src/is/simm/runtimes/web.clj` is the server
boot — system DB, room hydration, kabel + auth + sync, telegram, then the HTTP
routes.

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

## Server and client

The client is not a view onto an API. It runs a **real Datahike replica in the
browser**, backed by IndexedDB and kept current by konserve-sync over a kabel
websocket. Queries are local; the server is the authorization boundary, not a
query API.

That gives two distinct channels, and they are authorized separately:

- **Control plane** — RPC. `defn-spin-remote` in `.cljc` declares a function
  that exists on both sides; the client call travels to the server handler.
  Every inbound call passes `access/authorize-remote` first.
- **Data plane** — replication. konserve-sync subscribes the client to store
  scopes it may read. Subscription is gated; publication from a client is
  refused outright, because sync here is one-directional.

See [authority.md](authority.md) for how each is gated.

## Three rules that shape everything else

**Writes go through the server.** The client proposes; the server resolves the
store, checks `can?`, and transacts. A client never declares schema —
`:db/ident` is append-only, so a browser that could declare an attribute could
permanently fix its type.

**Aggregates are views over sources.** No persisted aggregate, no denormalised
index. If an aggregate is slow, cache it on a key derived from the sources' own
heads, so a stale entry is impossible rather than unlikely.

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
│   ├── auth_config.clj      kabel-auth store, JWT, accounts from config
│   ├── branching.clj        yggdrasil branching and branch GC
│   └── telegram.clj         adapter over dvergr's telegram channel
├── agents/                  personas, prompts, tools, summaries, merges
└── uis/web/desktop/         the spindel UI
    ├── signals.cljc         all UI signals and mutators
    ├── db_signal.cljc       client replicas, tiered stores, konserve-sync
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
