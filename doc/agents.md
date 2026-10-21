# Agents

Agents are participants, not a feature bolted to the side. This describes what
one is, what it may do, and where its writes land.

## An agent is a party

Humans and agents are the same kind of principal: a party row in the shared
system DB, identified by `(keyword "party" (str uuid))`. That is why
`@mention` works across both, why authorization needed no separate agent model,
and why an agent can be a member of a room in the ordinary sense.

Rooms come from [dvergr](https://github.com/replikativ/dvergr) — it owns the
discourse model, the turn factory, the per-room scheduler and the telegram
channel. simmis adds policy on top: which personas exist, what prompt they get,
which tools they are handed. That policy lives in `agents/room_agents.clj`, and
the personas and the tool manual in `agents/templates.clj`.

A human send is one durable message, even when several agents are selected to
respond. Multiple recipients use Dvergr's explicit recipient set on that same
envelope, not copies with the same id (durable deduplication would suppress
all but the first). Mentions stay separate from delivery selection. A failed
join removes only that recipient; unrelated participants are not broadcast to.
The client-supplied id and thread parent remain on the canonical message.

## The sandbox

An agent's code runs in an [SCI](https://github.com/babashka/sci) context with
a curated vocabulary, injected as namespaces:

| Namespace | For |
|---|---|
| `wiki` / `kb` | pages, blocks, attributes, typed properties |
| `memory` | bounded source-selected Markdown descriptors, reads and literal search in the current file world |
| `kontor` | the room's double-entry book — `entry!`, `balances`, `accounts` |
| `proposal` | opening, filing and releasing proposals |
| `sheet` / `office` | spreadsheet and document intake |
| `screen` | screen captures the room has been granted — live, and from shares that have ended |
| `transcript` | the room's meeting transcripts (`/drive/transcripts/`), and `transcribe!` for audio on the drive |
| a shell | over a [muschel](https://github.com/replikativ/muschel) mount, so file tools and the shell see one filesystem |

Plus ordinary `datahike.api`, so an agent can query rather than being restricted
to a hand-built accessor for every question.

`memory/documents`, `memory/read` and `memory/search` address Markdown in the
room file tree, not legacy KB pages. They return explicit ready/invalid/pending
status and selection receipts; a source hash is not a promise of archival
retention. A saved function follows a descendant world's repository or the
ordinary participant's existing proposal file workspace, never opening a fork
just to read. Membership, exact repository attachment and the world's read
effect policy are rechecked on use. The isolated workflow worker does not gain
live room access through this namespace.

`wiki` and `kb` reach the KBs attached to the agent's room, and only those the
agent itself may reach through the room's grants (`access/sandbox-kb-allows?`,
doc/authority.md). A KB the room holds `:read` is listed as read-only in the
agent's room context: `kb/db` and the `wiki` readers work on it, `kb/conn` and
every `kb/*` write refuse with `:not-authorized`. Declaring an attribute
(`kb/install-attr!`, by codomain object — the type is derived) needs a `:merge`
grant of the KB to the room; a `:read-write` grant (what attaching records by
default) writes data under
existing attributes only. Each use is checked again, so
detaching a KB takes it away from a running agent. A room with no KB attached
has no KB in its sandbox; nothing falls back to its creator's KBs.

The wiki query/formatting vocabulary is shared in `agents/wiki_read.clj`.
Each call resolves its authorized targets once as immutable DB values, so page
blocks and both halves of a link neighborhood cannot mix database revisions
within that call. Installed and saved functions resolve again on their next
use; the ordinary participant adapter rechecks current grants before reading
its known connections. Missing/refused resolution raises an error, not an
empty wiki. `summarize!` stays a separate authorized, effectful operation rather
than part of that read vocabulary.
If any KB in an ordinary participant's cached selection is revoked, the read
refuses until that participant context is rebuilt, even if another cached KB
remains authorized; it does not silently present a partial selection as complete.

A `kontor/*` posting records no author: kontor's posting API takes no
transaction metadata. A team's Book (Accounting → the team's heading;
`ops.book-report`) recovers it from dvergr's call record instead — the
`clojure_eval` call that invoked a `kontor/` verb and was running when the
entry was sealed names the run, and the journal links to it. When no single
call contains that instant, the entry reads "Posted outside an agent run"
rather than guessing.

The vocabulary carries `:doc` and `:arglists` (`agents/vocab.clj`) so `doc`
works inside the sandbox. This matters more than it sounds: a capability an
agent cannot discover is a capability it does not have, and the failure looks
like the agent being unable to do something rather than being unable to find
it.

### Meetings: transcripts, screens, the scribe

A meeting reaches a room two ways. What was SAID is a transcript
(`runtimes/transcripts.clj`): jigasi's transcriber saves a file per finished
call into `SIMMIS_TRANSCRIPTS_DIR`, which a filesystem watcher started at boot
posts into the room whose id is the meeting name; or someone presses
"Transcribe" on an audio file in Files (or an agent calls
`transcript/transcribe!`), and `dvergr.audio.stt` turns it into text — Groq or
OpenAI whisper with a key, else local faster-whisper — after cutting audio over
20 MB into ten-minute ffmpeg pieces. Transcription is never automatic on upload.
If any audio chunk produces no text, the request fails visibly rather than
presenting a partial transcript as complete; the original audio stays on the drive.
Either way a successful transcript becomes a uniquely named Markdown file under
`transcripts/` on the room drive — the document wiki pages cite — and one `:kind :transcript`
message posted to the room log, which wakes no agent: an hour of call is
nobody's turn until someone asks for it.

What was SHOWN is the screen-share stream (`runtimes/screen_intake.clj`).
`screen/frames` reads the live shares; `screen/shares` lists every share into
the room, ended ones too, and `screen/history {:since :until}` reads their
stored frames, so an agent asked after the meeting sees what was on screen
during it. A frame the vision model could not describe is stored with
`:failed` and the reason, and shows as failed in the owner's gallery and to
agents; it is never dropped.

The **Meeting scribe** persona (`templates/meeting-scribe-template`) turns the
two into wiki pages in the layout the tool manual teaches: it opens a proposal,
reads the transcript and the screens, writes `:wiki/atom` pages (one claim per
top-level `<h2>` block, a verbatim `<blockquote>` quote citing
`[[transcripts/<file>.md]]`, a `<strong>Sources:</strong>` block,
`:S.Page/source` and `:S.Page/entry-point`) and one `:wiki/overview`, then
files the proposal for review. `test/is/simm/agents/meeting_scribe_test.clj`
checks every verb the persona names exists in its sandbox and plays a scripted
run back through it.

**The sandbox is a soft boundary.** It is a curated SCI context hardened
against the escapes we have found, not a VM. Do not run untrusted agents
against secrets you cannot rotate.

### Packaged local SCI endpoints

`clojure -T:build worker` builds a dedicated `target/simmis-*-worker.jar`,
without the development classpath, frontend, server entrypoint or tests.
The build rejects ambient `user.clj`/compiled user startup and manifest
classpaths. The fixed entrypoint is `clojure.main -m
is.simm.ops.worker-process`, with `guest`, `broker` or credential-free `check`
mode. Check loads the real endpoint dependencies and reports the executor and
inference policy identities; it does not construct a provider or run an agent.
The trusted Linux installation must select a compatible Java21-or-newer JDK
at `/usr/bin/java`; changing shell `PATH` or `JAVA_HOME` alone does not change
this fixed bootstrap. CI explicitly aligns that path with its configured JDK.

`scripts/worker_runtime.py` validates an operator-selected absolute artifact
path and SHA256, fixes JVM arguments, and constructs a clean child environment.
The guest gets private login locations; only an explicitly configured broker
gets the native Codex login directory. It inherits no API keys, Java injection
options or controller credentials. The native provider refuses login refresh
and persistence during work. A digest pins selected bytes, not their provenance
or safety. Runtime installation remains trusted local administration.

The local controller accepts either its existing native/bwrap adapter or an
explicit `dvergr-sci` profile. The SCI profile binds the runtime digest and
endpoint policy identities into a versioned launch journal. Its fixed trusted
supervisor starts both JVMs only after physical capture, durable admission and
GO, with separate private homes and pipes inside the same retained systemd
execution unit. The envelope is 2.5GiB memory, no swap,128 tasks, one CPU and a
finite deadline; each JVM has a512MiB heap limit. Unsupported adapters refuse
before claiming work. A completed result still requires whole-unit shutdown,
a durable stopped journal and fresh lease authority before publication.
Recovery may stop an admitted unit or retry stopped output, never relaunch it;
it does not require the runtime jar or native login to remain installed.

The application registers six separately authenticated worker transport
operations and seven human local-worker controls: enrollment, revocation,
own-worker listing, explicit admission allowance, selected-file task submission,
pinned saved workflow submission and bounded benchmark submission. Workflow
submission shares native workflow source/parameter/input
preparation but uses the enrolled local model, not a server provider credential.
An optional versioned `input-selection.edn` bundle file declares incoming
evidence and supporting knowledge folders. Ordinary manual server/local-worker
admission captures these as normal frozen files under `/in/knowledge/`; no live
room knowledge capability is injected into the isolated worker. Run provenance
records trigger, execution and manifest identities. Benchmarks must supply
retained evidence in their case fixtures and never collect from the live room.
Scheduled server workflows use the same collector with a durable per-occurrence
snapshot. Empty initial inputs and unchanged evidence skip without inference;
supporting wiki edits alone do not trigger a maintenance loop. Failed or
uncertified attempts pause rather than consume the processed-input cursor.
Successful processing does not automatically adopt generated files or knowledge.
Over MCP, human controls
require a separately requested `worker-control` token. Neither an existing
read token nor a work token gains them; this operator-helper session should not
be attached as an ordinary model tool session. Worker/lease bearer prefixes
authenticate exclusively as workers, never by falling through to human/admin
authentication, and cannot invoke the human controls.

New human enrollment, allowance and submission default to disabled. Set
`SIMMIS_LOCAL_WORKER_ADMISSION=true` or `:mcp {:local-worker-admission? true}`
to permit them. This flag is not cancellation: already queued assignments may
still be claimed, and existing lease heartbeat/stop acknowledgement remains
available. Explicit enrollment revocation removes further delegated execution.
Enrollment validates native Codex registry metadata without requiring the
server to have a subscription login. Admission units are not measured dollars
or tokens, and no control automatically funds an allowance.

Startup fences orphaned assignments after Dvergr hydration and before MCP
claims are accepted, retaining unresolved world cleanup rather than faking it.
An application-owned single-thread recovery service checks pending requests
with a30-second fixed delay. It never executes work, imports late output or
certifies an Attempt. Receipt handlers never wait for its cleanup joins.
Initial fencing failure or an unhealthy service holds new human admission and
worker claims, but never removes existing lease heartbeat/stop. Shutdown joins
the service before releasing its handle; a failed join retains ownership.
Protocol readiness or a guest result does not prove shutdown. JVM flags alone
are not containment; SCI is not filesystem isolation against a JVM escape.

### Local SCI worker driver

Use `scripts/worker_driver.py` from the trusted local installation, not as an
agent tool. It requires Linux user systemd/cgroup-v2 and the system Python's
`dbus`/`gi` packages. Build the worker jar above and inspect its SHA256. Keep a
dedicated private state directory per enrollment; its parent must already
exist. Never copy it to another machine, delete unresolved Run journals or
start independent controller roots for the same worker identity.

First log in with a separate operator-helper session and explicitly bind the
MCP SSH-tunnel destination. This binding is set during login, not accepted from
tasks. Changing hosts or ports requires another explicit login; old sessions
without a binding may only use their own origin's `/mcp` endpoint.

```bash
/usr/bin/python3 scripts/mcp_client.py login --origin https://dev.simm.is \
  --session /private/operator-session.json --access worker-control \
  --mcp-endpoint http://127.0.0.1:47990/mcp

/usr/bin/python3 scripts/worker_driver.py --state /private/room-worker \
  --session /private/operator-session.json enroll \
  --endpoint http://127.0.0.1:47990/mcp --room ROOM_HANDLE \
  --jar /absolute/simmis-worker.jar --sha256 REVIEWED_SHA256 \
  --model REGISTERED_NATIVE_CODEX_MODEL
```

Enrollment runs a credential-free packaged preflight, durably records intent
before the network call, and privately saves only the room-attenuated worker
credential. An exact retry recovers the same enrollment. Local package paths
and the subscription login are not uploaded. Subsequent human controls use the
canonical room UUID in the receipt, never a potentially renamed/reused slug.

Funding is a separate explicit action. Choose a fresh request UUID for each
new intent and reuse it on a retry:

```bash
/usr/bin/python3 scripts/worker_driver.py --state /private/room-worker \
  --session /private/operator-session.json allowance --request-id REQUEST_UUID \
  --admissions 1

/usr/bin/python3 scripts/worker_driver.py --state /private/room-worker \
  --session /private/operator-session.json task --request-id ANOTHER_REQUEST_UUID \
  --task 'Read the selected input and write a report under /out/' \
  --path docs/input.md --timeout-ms 300000 --settlement review

/usr/bin/python3 scripts/worker_driver.py --state /private/room-worker run \
  --codex-home /absolute/local/codex-login-directory
```

For a saved workflow, inspect its current bundle with the workflow read/check
tools, then submit the exact UUID instead of a free-form task. This is an
alternative submission, not an extra free admission:

```bash
/usr/bin/python3 scripts/worker_driver.py --state /private/room-worker \
  --session /private/operator-session.json workflow --request-id REQUEST_UUID \
  --name WORKFLOW_NAME --expected-bundle INSPECTED_BUNDLE_UUID \
  --expected-owned-source INSPECTED_SOURCE_REVISION_UUID \
  --params '{"subject":"literal parameter"}' --timeout-ms 300000 --settlement review
```

Case-pack workflows additionally require `--case CASE_NAME`. `--inputs` accepts
a JSON map of canonical absolute workspace paths to text. The worker sees only
the pinned workflow source and fixture/input overlays, not all room files.
Authored checkers are not executed by this ordinary workflow control.

To measure that workflow, an installation operator with current room read/merge
authority can use the separate bounded benchmark control.

A workflow may declare `benchmark.edn` with a versioned case partition, for
example `{:version 1 :splits {:development ["example"] :held-out ["acceptance"]}}`.
Every named case must occur exactly once; development is required. Such bundles
require an explicit `split` in benchmark requests (`--split` in the local driver).
Omitted cases expand only within that split; cross-split selections fail before
provider preparation. The editor defaults to development, clears the case when
switching splits, and identifies held-out submissions in confirmation. Legacy
bundles without this file keep their original behavior. This is an experiment
protocol, not an access-control barrier against the workflow's human author.
Experiment metadata records the split, selected cases and a dataset identity
covering the partition, fixtures and gold. Workflow and evaluator identities
remain separately pinned. The example in `examples/workflows/market-discovery-eval`
includes real vendor-documentation excerpts, a source-disjoint held-out group,
negative calibration examples and a human review rubric. Its automated score
does not measure market viability; correlated cases are not independent samples.
The completed leaderboard shows the frozen split and dataset identity alongside
the workflow bundle; its tooltip lists the selected cases. No split is invented
for older experiments.

Claude Code text benchmarks use the separate `simmis_cli-allowance` and
`simmis_cli-benchmark` controls. They retain Dvergr's SCI tool loop and the same
clean fixture/checker/result machinery. An operator explicitly provisions
`max-cli-runs` (1–8), then submits an inspected bundle with models, repetitions,
split/cases and limits `{:max-cli-runs N :max-model-steps S :timeout-ms T
:settlement "discard"}`. Each cell consumes one conserved CLI Run admission,
with at most 32 model steps and 600000 ms per cell. Automatic compaction,
Run resume, delegation and automatic resubmission are disabled. These are not
HTTP-dispatch or dollar-spend caps. The ordinary native benchmark contract
continues to enforce its existing HTTP-dispatch accounting unchanged.

The workflow editor exposes this as “This server · Claude Code text benchmark”.
It uses the server's trusted, existing Claude login—not an uploaded credential.

The hosted execution backend also supports pinned recurring deployments through
the workflow editor's “Recurring deployment” controls. Save and inspect source,
choose the case/model/limits, then explicitly confirm an interval and timezone.
The activation retains that revision even when authoring files are later edited.
Schedules shows activation status and recent occurrences with links to their
canonical Run/results. Pause withdraws work still queued for admission; already
admitted work has its normal Run lifecycle. Resume rechecks the original
delegator's current authority and the pinned host/model policies. A changed
policy requires a new activation. Interrupted admissions remain held for
reconciliation, not replayed. Each occurrence uses the room's existing finite
allowance; exhaustion pauses the activation without provisioning more.
Ordinary workflow completion remains distinct from checker-backed benchmarking.
Recurring benchmarks require installation-operator authority as well as room
merge authority. CLI and local-worker executions are not scheduled by this path.

The experimental Claude Code text backend is disabled by default. A trusted operator enables it
with JVM properties `simmis.cli-benchmarks.enabled=true`,
`simmis.cli-benchmarks.unit=YOUR.service` and (optionally)
`simmis.cli-benchmarks.manager=user` (`system` for a system service). Admission
requires Linux cgroup v2 and verifies that the JVM is the service's main process, belongs to its cgroup,
and has bounded whole-cgroup termination enabled. One service must exclusively
own its runtime data directory. This is trusted CLI inference, not a sandbox
for hostile native code. Abrupt crashes leave CLI requests pending for manual
reconciliation: no automatic resumption, allowance return or cross-invocation
recovery is claimed. Ordinary native benchmarks are unaffected.

Initial text CLI execution also requires Linux process-session ownership. The results
retain CLI/model/mode provenance and distinguish unknown usage from measured
zero; subscription incremental and notional list-price costs remain separate.

```bash
/usr/bin/python3 scripts/worker_driver.py --state /private/room-worker \
  --session /private/operator-session.json benchmark --request-id REQUEST_UUID \
  --name WORKFLOW_NAME --expected-bundle INSPECTED_BUNDLE_UUID \
  --expected-owned-source INSPECTED_SOURCE_REVISION_UUID \
  --case CASE_NAME --repetitions 1 --timeout-ms 300000
```

Omit `--case` for all cases in the selected split (at most four); repeat it to select cases. One or
two repetitions allow at most eight serial cells. Fund the required admissions
explicitly before submission; this command never funds or retries work. Run the
local driver once per queued cell. Each cell is discarded after evaluation.
An empty queue between serial cells is not completion: check the Job's settled
status through `job_status` before treating the benchmark as finished.
Only clean fixtures reach the guest: checkers, gold and calibration remain on
the server. Results use the existing room experiment/Scorecard views; authored
checks remain ad-hoc, not independently trusted, and subscription cost/usage
is unreported rather than measured as zero. Existing operator tokens do not gain
the new tool automatically: request a new private `worker-control` session.

The private `simmis_local-worker-list` MCP control discovers your own non-revoked
enrollments in one room, ten per page (`after` takes the previous `next-after`).
It requires a human with current room read/merge authority, including when new
admission is disabled. Results contain only worker identity, generation and
allowlisted configuration hints—not credentials, verifier hashes or artifact
references. They do not establish liveness or authorize execution; submission
rechecks current authority and enrollment. Pagination bounds response size,
not query work: the service sorts the matching enrollment IDs before paging.

The room's Workflows source editor offers the same local execution controls
under **Execution backend → My enrolled local SCI worker**. Refresh your workers
and select one explicitly. Funding stays a separate confirmation in admission
units; benchmarking reports its cell count and discards each candidate world.
The browser uses its current authenticated human session, not a worker bearer.
It does not enroll workers, upload local logins or start the private driver.
Submission/funding retries preserve their original backend and immutable payload
even if the form changes. A funded indicator applies only to its own backend.

Attempt boards and experiment leaderboards preserve Dvergr's measurement
completeness. Billed spend and notional inference cost are independent: a
subscription may have known zero billed spend but unknown notional cost. A total
or cost-per-pass is unknown unless every contributing Attempt has that dimension
explicitly priced. Unknown values display as a dash, not zero or a partial sum;
legacy numeric placeholders without completeness flags remain unknown. These
read models do not rewrite certified Scorecards or infer reported token usage.

`run` handles one queued assignment, or accepts `--run RUN_UUID`. The task's
total deadline includes queue time and cold JVM startup;
`task` and `workflow` default to five minutes, with a ten-minute maximum. Start the local
runner promptly after submitting. Fresh interpreter/broker JVMs may spend a
substantial part of that bound loading the stack before inference begins.
The broker's portable configuration lives in `worker-contract`; decoding wire
messages must not initialize guest workspaces or database implementations. The
packaged-runtime suite checks that boundary in a fresh JVM. Guest frozen-input
decoding still invokes the full canonical file validator before allocation.
Use the documented `/usr/bin/python3`: it needs the host D-Bus/GLib bindings.
The controller initializes its physical observer under its permanent ownership
lock before creating a Run journal or claiming work. Missing host bindings or
an unavailable observer therefore cannot strand a new lease. Unresolved prior
Run journals still block admission before even initializing that observer.
The controller receives no human credential; the guest receives neither credential family nor
the native login. This uses Dvergr's native subscription provider and SCI loop,
not the Codex CLI's tool harness. The native login is read only by the broker;
an expired login fails without refresh or persistence. Renew it separately.
Job/Run state and captured results appear in the existing room inspectors and
artifact MCP reads, with subscription usage explicitly unreported.

Room chat also offers a collapsible **Recorded results** shelf for the twelve
most recent certified Attempts with explicit Run relations. Expanding the shelf
does not fetch file contents: **Show recorded files** loads the authorized
manifest, then **Preview** or **Download** reads a selected captured file using
the same bounded reader as the Run inspector. **Open Run / review work** opens
that Run's existing review/proposal controls; it does not merge anything.
Certificates can contain no files or failing checks. These immutable results
are not current room files or wiki pages. Older results remain in **Attempts**;
the shelf is hidden during historical chat browsing rather than showing current
results as historical evidence.

Screen descriptions retain failed frames as failed archive entries when the
vision provider throws or is unconfigured, rather than silently dropping the
frame. Provider exception text is not copied into the searchable archive;
trusted task-model resolution errors can report the configured model problem.

After interruption or a lost completion reply, use
`worker_driver.py --state /private/room-worker reconcile --run RUN_UUID`.
This never relaunches or sends GO, and does not need the artifact or login to
remain installed. Missing physical ownership metadata stays unresolved rather
than being treated as stopped. `revoke` uses the private human session and
remains available with new admission disabled; it still requires the worker
to acknowledge actual physical stop.

## Writes land on forks

A governed write from an agent does not go to trunk. It goes to a **fork** — a
[yggdrasil](https://github.com/replikativ/yggdrasil) branch — and a *proposal*
collects forks across KBs, the book and the room's code repository. Someone
with `:merge` authority accepts it, and only then does it land.

Dvergr Runs use the same rule at a wider boundary. A Run may execute in an
isolated subworld containing several forked systems. The Run records the
execution; it does not automatically become a Proposal. When the agent or user
explicitly files substantive retained work, Simmis adopts the world, partitions
its systems into independently governed Proposal components, and links the
Proposal back to the exact Run. This keeps execution isolation, review and
governance distinct while preserving their causal relationship.

This is the control that lets you be generous about what agents may write. The
axis is not read versus write; it is where the write lands. See
[proposals-and-time-travel.md](proposals-and-time-travel.md) for the mechanism
and [authority.md](authority.md) for who may accept.

Two consequences worth knowing when reading the code:

- A **read** must not mint a fork. Tools are split so that pure readers get an
  existing fork if one is open and never create one; only write-capable tools
  mint.
- Once a proposal is **filed**, the agent's tools are pointed at a filesystem
  that refuses writes — otherwise a write after filing lands on trunk, outside
  the review the proposal exists to provide.

## Egress is a capability, not a review

Review cannot cover irreversible actions: by the time you are reviewing, the
mail has been sent. Egress — email, telegram, HTTP, money, tokens — is
authorized in advance with a budget rather than after the fact.

## Models

Agents call an LLM through a provider key from the environment —
`FIREWORKS_API_KEY`, `OPENAI_API_KEY`, `ANTHROPIC_API_KEY`. A provider is
registered only when its key is present. A key is what lets simmis ASK a
provider what the account may run; it is never by itself the answer.
`OPENAI_API_KEY` on its own talks to OpenAI; `OPENAI_BASE_URL` re-points that
key at any other OpenAI-compatible endpoint, including a local one. It does not
re-point Anthropic, which speaks its own protocol. Each agent's model can be set per room in its
settings. Fireworks always uses its own base and `FIREWORKS_API_KEY`: provider
credentials are never borrowed, even when two provider records have the same
URL. Supplying `OPENAI_BASE_URL` also marks that record OpenAI-compatible rather
than native OpenAI, so provider-specific request behavior remains explicit.

The Codex subscription is the one provider without a key. Its credential is the
ChatGPT login `codex login` writes to `~/.codex/auth.json` (or `$CODEX_HOME`),
which dvergr's `:codex-subscription` provider reads and refreshes itself; simmis
reports its rows available exactly when dvergr would register that provider.
There is no account model list to ask: the picker offers dvergr's registered
`codex-subscription-*` ids as fixed rows, and a turn on one is billed to the
subscription rather than per token.

New agents store no model selection. Persona/template selection supplies the
role and system prompt, not a hidden model. An agent follows its owner's model
preference from Settings, then the product fallback when the owner has no
preference. The product fallback is per deployment: `SIMMIS_DEFAULT_MODEL`, else
`:models {:default "<id or family>"}` in config.local.edn, else the built-in
FAMILY `accounts/fireworks/models/glm-*`, resolved at turn time to the newest
GLM the account serves and dvergr registers
(`model-selection/default-model-choice`). It was the exact id glm-5p2 until
Fireworks withdrew it and every inheriting agent stopped joining.

simmis's own one-shot calls read the same `:models` map: `:vision` (screen
frame descriptions, `dvergr.media.vision/describe`) and `:cheap` (the chat
summarizer and the wiki merger, `cheap-llm-call`), an id or a family each
(`model/task_models.clj`). Unset, dvergr's default runs. A `:vision` family
resolves to its newest available version REGISTERED as vision-capable, so
`kimi-*` skips the text-only kimi-k3. Configured but unresolvable, the call
fails with the reason — a frame is stored as failed and says why — and boot
logs one `::task-model-unusable` warning; it never runs dvergr's default
instead. Dvergr 0.1.298 uses MiniMax M3 for the cheap call. Vision needs an
explicit image-capable model in Simmis, `DVERGR_VISION_MODEL`, or Dvergr's
registry defaults, with usable provider credentials; it has no bundled default.
It is read at every resolution and never written onto an agent, so changing it
and restarting moves every inheriting agent. A configured default is checked
like any other choice: e.g. `codex-subscription-luna-6` on a machine without a
`codex login` reports "Credential required" with the login instruction and the
agent does not join — it never runs the built-in default instead. Boot logs one
`::default-model-unusable` warning in that case. Only an explicit per-agent override stores a model FAMILY
(`gpt-*-luna`, `accounts/fireworks/models/glm-*`) with `:auto`, or a preferred
model version. Clearing that override physically removes the agent's model keys
and returns it to inheritance; existing explicit choices are not migrated or
rewritten. The agent picker presents inheritance as its own first row and names
whether the current state is inherited or an explicit override.

“Latest” stores the family, not the concrete model id. At resolution time it
chooses the newest version in that provider/family that is simultaneously
served to the account, registered in dvergr, and implemented by the provider
adapter. An explicit version is a **preferred version**. It remains selected
and is used while usable. If it is withdrawn, resolution may
fall forward to the newest usable version that is strictly newer in the same
family and provider. It never considers an older version, another family or
provider, or the product fallback. With no valid forward candidate, the model
is unavailable.

The configured preferred id and its resolved fallback are separate fields in
the resolution result, and only the preference remains in storage. If the
preferred version later becomes usable again, the next join/rejoin resolves
back to it automatically; the fallback is never persisted over it. The
inspector keeps the preferred row selected, labels it “preferred,” names the
resolved version separately, and explains whether a newer family version is in
use or no valid forward candidate exists.

The concrete id is resolved when the participant is joined, rejoined, or when a
committed owner preference is activated on an already-live inheriting
participant. For every provider, a model list from that provider's own endpoint
answers which ids the credential can currently reach
(`is.simm.model.model-selection`). It does not supply pricing,
context limits or capability metadata. Each returned id retains its provider,
base URL, credential source, reachability, and the CONTRACT its evidence came
from; identical URLs therefore remain two records.

That contract is per endpoint, not per protocol, and `catalog-contracts` says
whose promise each one is:

| Contract | Request | Authority |
|---|---|---|
| `:openai-models-api` | `GET https://api.openai.com/v1/models` | documented by OpenAI |
| `:fireworks-inference-models-list` | `GET https://api.fireworks.ai/inference/v1/models` | OBSERVED on Fireworks' documented inference base; Fireworks documents completions and chat completions there, not a models list |
| `:openai-compatible-models-list` | `GET <OPENAI_BASE_URL>/models` | asserted by whoever set the variable |
| `:anthropic-models-api` | `GET https://api.anthropic.com/v1/models` | documented by Anthropic |

The first three answer `{"object": "list", "data": [{"id": ...}]}`, present the
credential as `Authorization: Bearer`, and do not page. Fireworks' documented
NATIVE list operation is a different API answering a different question —
`GET /v1/accounts/{account_id}/models` returns
`{"models": [{"name": ..., "supportsServerless": ...}], "nextPageToken": ...}`,
pages at 200 rows, and enumerates a vendor account's whole collection rather
than what this key can call. simmis does not read it, and a body in that shape
arriving at the compatible path is refused rather than read as an account that
serves nothing.

Anthropic is **not** an OpenAI-compatible provider and does not pass through
that parser. Its List models operation authenticates with `x-api-key`, requires
the `anthropic-version: 2023-06-01` header on every request, and PAGES: it
answers `{"data": [{"id": ..., "type": "model", ...}], "has_more": bool,
"first_id": ..., "last_id": ...}`, defaulting to 20 rows and accepting up to
1000, walked forward by passing `last_id` back as `after_id`. simmis walks it to
the end; a failure part way through discards the whole list rather than
reporting a short one, because a truncated account is indistinguishable from a
small one and the difference between them is `:unavailable-to-account`. Its
mandatory `has_more` is also how the parser refuses a foreign body: an
OpenAI-shaped answer carries none, and reading one here would accept a single
page as a whole account.

Dated ids get opposite treatment per vendor, which is why that too is a contract
property. OpenAI's `gpt-5.5-2026-04-23` is DROPPED — the date's digit groups
parse as a version token, and `:auto` would chase release dates. Anthropic's
`claude-haiku-4-5-20251001` is KEPT and its alias `claude-haiku-4-5` added
beside it: Anthropic answers with pinned ids while the registry spells
pre-4.6-generation models dateless, and matching those raw reported an account
that plainly serves Haiku 4.5 as not having it.

A fetch counts only when it succeeds, parses, and is complete. A missing `data`
list, a foreign schema, an unwalkable cursor, or a page marker on a contract
that does not page leaves the last-known ids in place and reports a failure,
because incomplete evidence must never harden into a verdict about the account. If one fetch fails, only that provider's last-known ids
survive and the curated rows remain visible as `:temporarily-unreachable`. A
provider that ANSWERS and refuses the key is separated out: waiting fixes an
outage, and only a new key fixes a rejection. The picker and resolver share six
states: `:available`, `:needs-credential`, `:not-implemented` (including a
registry gap), `:unavailable-to-account`, `:temporarily-unreachable`, and
`:credential-rejected`. Only an `:available` picker row may be newly saved,
and only an available resolved target may execute. Saving an owner preference
or agent override recomputes the row's availability on the server. Clearing an
override validates the inherited preference through the same soft resolver, so
a valid same-family forward fallback may execute without replacing the stored
preference. An unavailable explicit or inherited choice never falls through to
the default, a different family, or another provider.

A model must also exist in dvergr's registry, which is where its context window,
capabilities and price per token come from. A model list never adds to that
registry: a listed but unregistered id is shown as “Not supported,” while a
registered id absent from a successful provider response is unavailable to that
account. `:auto` selects the newest usable version inside its exact family;
preferred-version fallback uses the same candidate set but accepts only
strictly newer versions. No third-party metadata refresh runs in the first
agent turn; dvergr's registry is the metadata source from process start.

The PROVIDER is derived from the model, never stored beside it. A stored
provider is a second thing to keep in sync, and it used to win: every agent
created in the UI was stamped `:fireworks` at creation, so preferring an
OpenAI model still posted the request to Fireworks.

Four model facts have distinct lifecycles:

1. **Configured preference** is the stored owner preference or explicit agent
   override. It preserves a family/Latest choice or a preferred version.
2. **Desired resolution** is what `room-agents/describe-model-resolution`
   computes from current configuration and catalog/registry facts. The agent
   inspector and room settings display this as “Resolves to” or “when next
   joined.”
3. **Availability** is the resolver's current credential, catalog, registry,
   and adapter result. It governs new saves and whether a new join may capture
   the desired model.
4. **Active runtime state** is the concrete provider/model spec captured by a
   participant when it joined and subsequently changed by targeted model
   directives. The current UI does not introspect that spec.

Participant construction calls the desired resolver once, validates the result,
and puts the concrete provider/model into dvergr's participant spec. Saving an
owner preference re-reads every live inheriting agent under its existing
lifecycle lock and posts dvergr's targeted `:directive/switch-model`; the
participant, working context, subscriptions and inbox remain intact. A switch
during generation settles the current provider call and restarts that generation
with the new spec. A delayed activation carries the committed preference only as
an expectation and is skipped if a newer owner write or an explicit agent
override has won, so stale writers cannot switch the model back.

Availability is checked before any directive is posted. If the committed target
has become unavailable between save validation and activation, the save remains
committed, the old live participant keeps its previous model, and the caller gets
a partial activation failure. Agents that are not live simply resolve the new
preference when they next join. Catalog refreshes remain observational and can
change displayed desired availability without changing a live participant.

Join is where availability is enforced, and it fails closed: an unavailable
resolution throws `:model-unavailable` rather than running the agent on some
other model. That failure is isolated to the one participant. `post-user-message!`
joins each recipient separately, drops the ones that throw, and still posts the
user's message and still joins the rest. One misconfigured agent therefore
cannot lose a message or silence the room. Each dropped agent is reported twice:
a Telemere `:warn` (`::agent-join-failed`, with the agent, model and availability
state) for the operator, and a note in the room itself — authored by the agent
that cannot run, addressed back to the sender so no participant is woken by it —
naming the agent and the reason, in the same words the settings screens use. If
every recipient fails, the message is still posted; the send does not fail, and
the client shows no send error, because the message WAS sent.

That expected state is not a catch-all. A persistence, context/namespace,
provider-client, or partial `d/join` fault is classified as `:join-error`, not
as model unavailability. The server log records it at `:error` with an incident
reference and the throwable for diagnosis; the room note contains only the
agent, consequence, and incident reference — never exception text, credentials,
URLs, or stack detail. Failed initialization removes the context and participant
created by that attempt before a retry; the next dispatch therefore makes one
fresh participant rather than a duplicate responder.

Explicit agent edits are a separate path. Updating an agent's own model, prompt,
or other mutable configuration makes its live participants leave and clears
their cached contexts. The next dispatch rejoins the agent and resolves a fresh
spec. This generic reset does not imply per-turn resolution. Owner-preference
activation uses the non-destructive targeted path above; catalog changes do not
trigger either path.

One native-provider quirk is load-bearing here: on OpenAI's configured
`/v1/chat/completions` path, the GPT-5.6 entries require `reasoning_effort`
`"none"` when tools are attached. dvergr sends that only to native OpenAI, so
tools work and server-side reasoning does not; an arbitrary compatible endpoint
does not receive an OpenAI-native field. Agents that need both belong on
`gpt-5.5` until the Responses API is spoken.
