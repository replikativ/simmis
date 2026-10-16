# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working
with code in this repository. Refreshed 2026-07-03 — the previous
version documented the retired UIx/React architecture.

## What simmis is

A business-oriented, value-building, self-programming environment on the
datahike.io stack: chat + wiki + knowledge bases + AI staff, one shared
memory substrate. The UI is **spindel** (FRP signals/spins rendering
DOM incrementally — NOT React); the agent substrate is **dvergr**
(discourse rooms, SCI sandbox, per-room scheduler, telegram channel);
persistence is **datahike** over **konserve** with **yggdrasil** CoW
branching, synced client↔server via **konserve-sync** over **kabel**
websockets with **kabel-auth** JWT.

simmis is the prime showcase for spindel — hold UI code to idiomatic
reactive decomposition (see "spindel sharp edges" below).

Documentation: `doc/architecture.md` first, then `doc/data-model.md`,
`doc/authority.md`, `doc/proposals-and-time-travel.md`, `doc/agents.md`.
Each describes code that exists; there is no roadmap or backlog in the
repository, and open work is not tracked in documentation.

## Instructions

- Use `clj-nrepl-eval` to iterate and validate before large changes.
- *Avoid fallback solutions* except for experimentation; ask when you
  can't solve something without one.
- Git commit after significant milestones; not while stuck or debugging.
- Circle back to `doc/` after an implementation and before committing. Those
  five documents describe code that EXISTS — if a change makes one of them
  wrong, fix it in the same commit rather than adding a note about it.
- When isolating a problem is hard, add log statements / REPL probes.
  For reactive-correctness bugs, trace ENGINE state via REPL probes —
  not app-level workarounds.

### NEVER Use Delays as Patches

Never use `setTimeout`/`Thread/sleep`/timeouts to "fix" timing or
synchronization issues. Use real coordination: await events, channels,
promises, signals, or pass data explicitly. (One legitimate exception:
a `requestAnimationFrame` to defer DOM measurements or scroll positions
until after layout — a documented DOM lifecycle idiom, not a race patch.)

## Development environment

```bash
npm i               # once
clj -M:dev          # START HERE: Maven only, needs nothing but this repo
clj -M:stack:dev    # + a local ../dvergr, for co-developing the agent substrate
clj -M:local:dev    # + every replikativ sibling as a git checkout (see below)
```

One command starts: nREPL **47888** (CLJ server), shadow-cljs watcher
with nREPL **9631** (CLJS), websocket server **ws://localhost:47295**,
static files **http://localhost:8080**.

### Running a second instance

The ports and the home above are the DEFAULT instance's. To run another one
beside it (e.g. an agent's end-to-end test while the demo runs), use a
separate worktree — `public/js` and `.shadow-cljs` belong to the checkout, so
two instances in ONE checkout overwrite each other's bundle — and from there:

```bash
env -u TELEGRAM_BOT_TOKEN SIMMIS_PORT_OFFSET=1000 \
    DVERGR_HOME=/tmp/simmis-b/dvergr SIMMIS_DATA_DIR=/tmp/simmis-b/data \
    SIMMIS_MAIL_SYNC=false SIMMIS_DEV_AUTH=true \
    CODEX_HOME=/tmp/simmis-b/codex CLAUDE_CONFIG_DIR=/tmp/simmis-b/claude \
    clj -J-Xmx3g -J-Duser.home=/tmp/simmis-b/home -M:dev
clj-nrepl-eval -p 48888 "(status)"   # page http://localhost:9080, ws 48295
```

Use offset 1000 or 3000, not 2000: it puts the page on 10080, which browsers
refuse to open (`ERR_UNSAFE_PORT`, the Amanda backup port), so the instance
boots and nothing can look at it.

`SIMMIS_PORT_OFFSET` moves every port (47888, 9631, shadow's 9630, 8080,
47295) by one number, page and websocket together; `(status)` probes the
offset ports and shows what the instance resolved. `DVERGR_HOME` (dvergr's own
variable) holds the system DB, room stores and generated secrets;
`SIMMIS_DATA_DIR` holds `data/`; `DVERGR_CONFIG` replaces
`./config.local.edn`. An offset instance never starts Telegram — the token in
your shell (`TELEGRAM_BOT_TOKEN`) or config is the machine's, and a second
poller knocks out the first; `env -u` is belt and braces. It names its blob
cookie `simmis_blob_<offset>`, because cookies are scoped by host, not port.
Use a FRESH home, never a copy of the demo's (a copy carries the same store
ids, so it is a replica, not a second home). `SIMMIS_DEV_AUTH`
only on a throwaway instance like this one; delete its home afterwards.

`SIMMIS_MAIL_SYNC=false` stops boot from polling the IMAP accounts the system
DB lists (default on; `:mail {:sync? false}` in a config does the same). The
machine's LLM CREDENTIALS are the other thing a test instance picks up without
asking: dvergr reads the Codex login from `$CODEX_HOME`, else
`<user.home>/.codex`, user-level definitions from `<user.home>/.dvergr`, and
the `claude` CLI it spawns for `claude-code-*` models inherits the process
environment, so it uses your interactive login unless `CLAUDE_CONFIG_DIR`
points elsewhere. `-J-Duser.home=<throwaway>` covers the JVM-side lookups (it
is not `$HOME`, so `~/.m2` and the Clojure CLI are unaffected), and
`CODEX_HOME`/`CLAUDE_CONFIG_DIR` cover the env-var ones. Provider API keys
(`FIREWORKS_API_KEY`, `OPENAI_API_KEY`, `ANTHROPIC_API_KEY`) come from the
environment: `env -u` them too if the instance must not spend on them.

Storage GC never runs on such an instance unless you turn it on — it is off
by default everywhere (see `is.simm.runtimes.storage-gc`).

Dependencies come in three sets (deps.edn): Maven releases by default;
`:stack` overlays ONLY `../dvergr` (the sibling simmis co-develops with); `:local`
overlays every replikativ sibling (`../spindel`, `../datahike`, `../konserve`,
…).

**Default to `:stack`.** `:local` inherits whatever branch each sibling
checkout happens to sit on, so parallel work in another repo breaks simmis's
boot with an error that points at simmis. Two real instances: a `../datahike`
checkout parked on a query-planner branch fails the konserve version check
(`:now nil` — a source checkout reports no version; datahike#902 makes the
check tolerate that, still open), and a sibling mid-edit fails compilation.
Reach for `:local` deliberately, when the change you are testing IS in a
sibling — and expect to check what branch it is on.

### REPL usage

```bash
# CLJ (server) — port 47888
clj-nrepl-eval -p 47888 "(status)"
clj-nrepl-eval -p 47888 "(help)"                  # the read surface, listed
clj-nrepl-eval -p 47888 "(user/restart-web!)"     # web server only, no JVM restart

# CLJS (browser) — port 9631; jack into the build FIRST (session persists):
clj-nrepl-eval -p 9631 "(shadow/repl :app)"
clj-nrepl-eval -p 9631 "@is.simm.uis.web.desktop.signals/layout-columns"
```

**Start with `(status)`.** It probes the ports rather than reporting the
dev namespace's own bookkeeping (which stays `true` after a component
has died), reports whether the shadow watcher is up, and — the one that
saves the most time — whether the bundle is STALE:

```clojure
{:instance {:port-offset 0 :ports {...} :home "…/.dvergr" :data-dir "data" …}
 :ports {:nrepl-clj true :nrepl-cljs true :shadow-http true :http true :websocket true}
 :shadow {:app-watching? true}
 :bundle {:built? true :size-mb 34 :age-min 0 :stale? false}
 :system-db {:rooms 10 :parties 12 :kbs 8}}
```

`:stale?` compares `public/js/app.js` against the newest `.cljs`/`.cljc`
source. A FAILED build leaves the previous bundle in place, so "Build
completed" in the log and a served page can both predate your edit —
which is indistinguishable from success until you check this.

**Read the running system without writing queries or requires.** `dev/repl.clj`
is referred into `user`, so these work as bare one-liners:

```clojure
(rooms)                  ;; every room, newest first
(parties) (knowledge-bases)
(room "verification")    ;; by slug, uuid, or a substring of the name
(room-conn "verification");; that room's OWN store conn — nil = not hydrated here
(kb-conn db-scope)
(conn) (db)              ;; the system DB
(q '[:find ?n :where [_ :room/name ?n]])
```

`repl` only reads; lifecycle stays in `user`, where it is harder to call
by accident. Note `room-conn` goes through the room's execution context —
yggdrasil's registry is context-backed, so resolving a room store from the
server context finds nothing and returns nil with no error.

**Reloading is the dangerous part.** `(user/restart-shadow!)` over nREPL
takes the whole JVM with it — all four ports — leaving nothing to
reconnect to; restart from a shell instead. And `refresh!` / `reload-ns!`
on anything touching datahike RECORDS gives you two classes with the same
name from different classloaders: nothing throws, and what you see
afterwards is every authenticated RPC answering `:not-authorized`. If
that happens, restart the JVM rather than debugging it as an auth
problem. The docstrings say so at the call sites too.

**Fork-safe state lies on bare deref**: spindel signals and mailbox
state resolve through the bound execution context. On the CLIENT, wrap
access: `(binding [rtc/*execution-context* runtime] @some-signal)` — a
bare deref can return nil/stale without erroring. On the server the
default context is registered, so a bare deref usually resolves; the
exception is anything owned by a room, which lives on that room's fork —
use `room-conn` rather than reaching for the registry yourself.

Long/expensive evaluations: write output to a tmp file and grep it,
instead of re-running.

### Hot reload

Shadow-cljs recompiles on save; the app remounts via `reload!` hooks.
**If a build FAILS and then succeeds, hard-reload the page** — hot
reloading through a failed build leaves the app half-mounted (writes
work, renders don't) and the corruption survives the next successful
hot-swap.

**After REMOVING a file or a require, fully restart the shadow worker**
(stop-worker + watch, or restart the dev process) — the incremental
build state can go inconsistent ("no output for id: …") and silently
serve a stale/inconsistent bundle while logging "Build completed"
(2026-07-05 episode: half-mounted boots with zero console errors).
And when scripting edits: verify each build by checking for a NEW
"Build completed" line AND the artifact mtime — a failed build keeps
serving the previous bundle, so green-looking verifications can run
on stale code.

### Watching the dev log

The dev process tees to a log file (see the background task). Grep it
for `Build completed|Build failure`, `::server-started`,
`::pump-wedged` (bus watchdog — see below).

## Architecture

```
dev/user.clj                 - dev lifecycle (nREPL, shadow, web server)
src/is/simm/
├── model/                   - system DB (shared with dvergr), rooms,
│                              parties, KBs (+grants), room content DBs,
│                              access (JWT principal), billing, schema
├── runtimes/
│   ├── web.clj              - server boot: system-db → dvergr room
│   │                          hydration → kabel/auth/sync → telegram →
│   │                          pump watchdog
│   ├── web.cljs             - client boot (kabel peer, auth, origin-derived URLs)
│   ├── telegram.clj         - thin adapter over dvergr's telegram channel
│   ├── branching.clj        - yggdrasil KB branching + internal-branch GC
│   ├── auth_config.clj      - kabel-auth store/JWT; alpha users from
│   │                          config.local.edn :alpha-users
│   ├── storage_gc.clj       - periodic store GC; OFF unless
│   │                          :storage-gc {:enabled? true} /
│   │                          SIMMIS_STORAGE_GC=true (datahike#1117)
│   └── pump_watchdog.clj    - loud detector for the room-bus wedge
├── agents/
│   ├── room_agents.clj      - thin policy layer over dvergr's turn
│   │                          factory (rooms, ctxs, prompts, tools)
│   ├── templates.clj        - factory personas + the sandbox tool manual
│   ├── summarizer.clj       - chat-window summary pages
│   ├── merger.clj           - LLM diff summaries for proposals
│   └── vocab.clj            - :doc/:arglists on the SCI vocabulary
└── uis/web/desktop/         - the spindel UI
    ├── main.cljs            - app spin (tracks top-level signals)
    ├── signals.cljc         - ALL UI signals + mutators
    ├── db_signal.cljc       - client datahike replicas (tiered
    │                          memory+IndexedDB stores, konserve-sync)
    ├── chat_remote.cljc     - defn-spin-remote endpoints (RPC)
    ├── views/               - columns, chat, nav, schedules, …
    └── block_editor.cljs    - wiki page editor
```

Identity: a party (human or agent) is an actor row in the SHARED
dvergr system DB (`.dvergr/system-db`); participant keyword =
`(keyword "party" (str uuid))`. Rooms are dvergr rooms (live Room +
store via registry); simmis adds a per-room content DB (client render
path, dies in Stage 5). KBs attach to rooms via `:grant/*` rows.

## Distributed functions (RPC)

`defn-spin-remote` in `.cljc` (see `chat_remote.cljc`):

```clojure
(defn-spin-remote my-fn!          ;; NO docstring — args vector must
  [server-id arg1]                ;; follow the name
  (spin-remote server-id [arg1]   ;; captured vars (may be empty [])
    #?(:clj (let [party-id (access/authenticated-party-id)] ...)
       :cljs nil)))
```

- The JWT principal is bound only during the invoke — capture it at the
  top, synchronously. It requires an AUTHENTICATED ws connection; the
  reconnect path does not refresh expired tokens (roadmap), so
  `:authentication-required` errors usually mean a stale client token.
- Client-side invocation from DOM/load callbacks needs
  `(binding [rtc/*execution-context* runtime] ...)` or it silently
  no-ops. From render bodies, run remote spins inside a `go` block
  (root spin) — see `load-room-details-into-signal!` for the pattern.

## spindel sharp edges

Each of these bit us. None of them fails loudly.

1. **Track placement**: a mid-body `track` resume re-executes the spin
   FROM THE TRACK POINT, and delta reads (`iv/get-new`) are not
   idempotent — track signals at the TOP of a spin (or in the parent,
   passing values down) and never downstream of interval consumers.
2. **ifor-each memoizes on item equality** — closure variables are
   invisible to the diff. Anything that must re-render lives IN the
   item (`(assoc tab :active? ...)`).
3. **Refs / foreign-node :on-mount fire on MOUNT, after linkage** —
   since spindel 0.1.122 (rule R5) the element is attached when the
   callback runs, so `.closest`/parent traversal works directly; no
   `requestAnimationFrame`. A rAF is only for post-layout measurement
   (sizes, scroll positions). A ref does NOT re-fire on reconcile: work
   that must follow a later change to an already-mounted element is not
   a ref's job.
4. **Signals/mailboxes are ctx-backed** — see "fork-safe state" above.

## Known instrumentation

- `is.simm.runtimes.pump-watchdog` probes every live room's bus each
  minute; `::pump-wedged` in the log = the source pump lost its mailbox
  waiter (dossier in the dvergr repository).
- Client replica connects self-heal once (wipe IndexedDB + resync) —
  a workaround for the konserve `-blob-exists?` bug (root fix pending).

## Logging policy

Server-side logging is Telemere only:

```clojure
(require '[taoensso.telemere :as log])
(log/log! {:level :info :id ::my-op :msg "..." :data {...}})
```

No `println`/`prn` in committed code. `js/console.log` is fine during
CLJS development and for error reporting; strip debug noise before
committing. Change level at runtime: `(tel/set-min-level! :debug)`.

## CSS

Single stylesheet `src/is/simm/core.css`, bundled by Lightning CSS:

```bash
npm run build:css      # after editing core.css (watcher does it in dev)
```

## Browser automation (Chrome DevTools MCP)

```bash
/snap/bin/chromium --remote-debugging-port=9222 http://localhost:8080
```

Then use `mcp__chrome-devtools__*` tools: `take_snapshot` (a11y tree),
`evaluate_script` (preferred for assertions/interactions — snapshot
uids go STALE after re-renders), `list_console_messages`,
`take_screenshot`. Log in via the form (accounts in config.local.edn
`:alpha-users`).

## Testing

`clj -X:stack:test` (kaocha). NOTE: the `:test` alias uses `:exec-fn`, so it
must be run with `-X`, not `-M` (`-M:test` silently drops into a REPL). A bare
`clj -X:test` runs too (everything is on Maven now); prefer `:stack` when the
change you are testing touches dvergr. Tests live under `test/is/simm/` (model
CRUD, katzen projection). The engine-level reactive tests live in the
spindel/dvergr repos.

Authorization: `model/access.clj` (the `can?` predicate) and the control
plane both have coverage — `access_test.clj`, `access_control_plane_test.clj`.
The data plane has both halves covered too (`data_plane_test.clj`):
`data-plane-authorized?` gates SUBSCRIBES, `data-plane-publish-authorized?`
refuses every inbound PUBLISH — sync is one-directional here, so no client is
ever a legitimate publisher.

`test/is/simm/authority/` runs the predicate and the gates TOGETHER against a
real (in-memory) system DB and real grants: a matrix of every `rpc-policy` row
× {owner, member, non-member, admin, anonymous} (a new row without a matrix
entry fails), the data plane, and executed attacks on the raw paths, runs and
handlers. Attacks that still succeed are `known-hole`s: they PASS while open
and FAIL once fixed, demanding to be flipped into a plain refusal assertion.
When you close one, flip it in the same PR.

## Secrets

`config.local.edn` (gitignored): telegram bot token, alpha users. Its
committed template is `config.example.edn` — when you add a config key,
document it there, since that file IS the setup instructions.
JWT dev secret persists at `.dvergr/jwt-secret` (gitignored). Never
print tokens/secrets into chat, logs, or commits.

`POST /auth/dev` mints a token for ANY email and creates the account if
missing. It is opt-in via `SIMMIS_DEV_AUTH=true` and warns on every boot
while set. It used to be enabled whenever `SIMMIS_JWT_SECRET` was unset —
i.e. on by default, in exactly the unconfigured state a fresh clone is in.
Do not re-derive one from the other: "do I have a signing secret" and "may
anyone mint a token" are different questions.
