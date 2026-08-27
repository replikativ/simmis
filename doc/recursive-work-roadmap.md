# Recursive work programming model roadmap

Simmis is a projection of a recursive work programming model, not the owner of
that model. The social UI is its human observation and control plane: it makes
actors, programs, effects, artifacts and decisions legible without reducing the
system to chat.

The initial customer is a technically capable small-business owner, especially
a founder building products on the open-source Simmis stack. The substrate may
be research-grade; the default surface must remain usable in the vocabulary of
the business: customer, offer, assumption, scenario, experiment, decision,
cost, risk and next action. Programs, queries and raw traces are progressive
disclosure, not prerequisites for ordinary work.

This roadmap takes the useful lessons from Buzz while preserving the deeper
Simmis/dvergr architecture: typed knowledge, programmable properties, governed
multi-system writes, versioned files, arbitrary programs and future
probabilistic execution.

## Ownership

| Layer | Owns |
|---|---|
| spindel | incremental evaluation and effect handling (`sample`, `observe`, cancellation, other effects) |
| raster | probabilistic/simulation semantics and result values |
| dvergr | actors, rooms, assignments, messages, work, runs, causal/effect traces and execution control |
| Datahike/Yggdrasil | queryable durable state, index space, history, branches and distributed storage |
| Geschichte | versioned file trees, workspaces, interchange and published source artifacts |
| Simmis | reactive desktop/web/mobile projections and user interaction |

Application-specific records can still live in Simmis-owned schemas. A concept
needed by another dvergr client must not be introduced only as an `S.*` UI
projection.

## Core algebra

```text
Actor --Assignment--> Room
  |                    |
  |                    +-- Message --causes--> Run --emits--> Effect
  |                    |      |                  |              |
  |                    |      +-- thread         |              +-- artifacts
  |                    |                         +-- child runs
  |                    |
  |                    +-- Work/Campaign --produces--> Proposal
  |
  +-- reusable Definition/Persona

ArtifactRef --> Knowledge page | Geschichte path | drive object | site | result
```

A `Run` is deliberately not an `AgentRun`. Its kind may be `:agent-turn`,
`:workflow`, `:simulation`, `:site-build`, `:site-deploy`, `:query`, or another
program. Tool calls, model calls, transactions, `sample`, `observe`, approvals,
publishes and external egress are typed effects within a run.

The append-only effect trace is ground truth. Mutable run state is a projection
for fast supervision. Simmis renders semantic summaries by default and raw
effects on demand.

Simulation must retain an explicit epistemic boundary. A modeled actor is not
a live actor; a predicted reply is not a received message; a recommended
commitment is not an accepted proposal. Every simulated result records its
world-state snapshot, assumptions and model versions, observations, samples,
utility/cost constraints and provenance. This permits social experiments,
counterparty-response forecasts and proposal rehearsal without manufacturing
false social history.

## Product slice used across all phases

Each phase must leave one founder workflow more complete rather than only
adding infrastructure. The first vertical slice is:

1. A founder creates a room for a product/customer decision and attaches the
   relevant wiki pages, files and typed business facts.
2. They assign agents as researcher, counterparty model and reviewer, with
   explicit response policies and budgets.
3. A discussion produces visible runs, evidence and alternative proposals.
4. A scenario run forecasts bounded-rational counterparty responses and
   business outcomes, clearly labeled as simulated.
5. The founder reviews assumptions and distributions, accepts or amends a
   proposal, then optionally builds and publishes a governed product artifact.

The early phases may use deterministic or mock scenarios. Raster and Spindel
later replace that implementation behind the same run/effect and result
projection.

## Phase 1 — canonical messages, assignments and dispatch

First remove the current model split: dvergr already persists canonical
`:message/*` entities, while Simmis dual-writes `S.Message` rows for its UI.
Move the Simmis timeline to the canonical rows before adding new social
semantics.

Extend dvergr with a first-class room assignment relation:

```clojure
{:assignment/id              uuid
 :assignment/room            room-ref
 :assignment/actor           actor-ref
 :assignment/role            :lead|:specialist|:reviewer|:observer
 :assignment/response-policy :always|:mention|:manual
 :assignment/config          {...}}
```

Extend the canonical message envelope with structured actor-valued audience,
causality and threading. Visible `@name` text is presentation; dispatch resolves
it to actor identities before posting. Unknown or ambiguous explicit mentions
are errors and never fall back to waking every agent.

Simmis deliverables:

- render the existing room timeline from dvergr messages;
- preserve wiki links, party mentions, reasoning, attachments and tool rows;
- room roster with role and response-policy controls;
- show the resolved dispatch audience on a sent message;
- retain optimistic sends and reactive Datahike updates.

## Phase 2 — generic runs, effects and control

Persist a run before evaluation begins:

```clojure
{:run/id         uuid
 :run/kind       keyword
 :run/room       room-ref
 :run/actor      actor-ref
 :run/trigger    message-or-run-ref
 :run/parent     run-ref
 :run/work       work-ref
 :run/status     :queued|:starting|:running|:waiting|:blocked|
                 :completed|:failed|:cancelled
 :run/started-at instant
 :run/ended-at   instant}
```

Effects carry stable ids, run id, type, state, timestamps, semantic input/result
summaries, optional raw payload refs, and artifact refs. One effect updates in
place from requested to running to completed/failed while its transitions remain
auditable.

Controls address a run id: cancel, steer, retry, pause/resume where meaningful,
and approve/refuse. Existing dvergr cooperative cancellation and steering should
be exposed through this stable contract rather than reimplemented in Simmis.

## Phase 3 — supervision, threads and social legibility

Build the Simmis observation/control projection:

- room roster showing offline/idle/running/waiting/blocked/failed;
- active and recent runs per actor;
- an activity rail phrased as verb, object and outcome;
- failures and requests for intervention rise; reads and raw reasoning recede;
- raw effect trace is always available;
- thread/reply views over canonical message causality;
- cancel and steer controls target one run;
- high consecutive agent-to-agent hop budget, reset by human input.

Presence is a renewable availability lease. `running` is derived from runs, not
confused with presence or process health.

## Phase 4 — recursive work, campaigns and proposals

Make work durable before its final proposal exists. A work/campaign records its
goal, participants, roles, child runs, contributions, blockers and completion
criteria. It may recursively create child work.

Simmis's existing per-agent forks remain the unit of attribution and selective
review. A campaign groups them into one intended future; a proposal is the
governed transition of that future into trunk. Schedules and event triggers
create the same generic runs as human messages.

Simulations later fit here naturally: a work item can run alternative futures,
record sample/observe effects, compare distributions and propose an action
without conflating the simulation with the accepted state transition.

Social simulation uses explicit modeled-actor definitions linked to, but never
identified with, real parties. Counterparty models expose assumptions,
information available to the modeled party, objectives, constraints and model
uncertainty. Results support proposal rehearsal, communication drafting and
experiment design; sending or committing remains a separately authorized live
effect.

## Phase 5 — communication polish and lightweight clients

Add the social mechanics that improve awareness without redefining the work
model:

- reactions, amendments/deletions and thread following;
- read state, mentions, assignments, approval and failure notifications;
- reminders, presence and typing;
- bounded recent-message/thread queries and live subscriptions;
- lazy artifact previews and search;
- the narrow mobile surface: rooms, threads, notifications, run control,
  approvals and previews.

Async Datahike is not required for Phases 1–4 semantics. It is required to make
Phase 5 clients bounded: recent messages rather than a room replica, one page
rather than a wiki, and one preview rather than a drive. Partial query results
must carry completeness/coverage semantics so absence is never mistaken for a
known falsehood.

## Knowledge and file projection

The Notion-like typed page and the Obsidian-like Markdown file should be two
projections of one knowledge artifact.

A practical interchange contract:

- stable page UUID independent of path;
- path/title and selected typed properties round-trip through YAML frontmatter;
- Markdown body round-trips to the internal block tree;
- block identity is preserved by unobtrusive anchors or a sidecar when needed,
  but ordinary Markdown remains useful without Simmis;
- links use readable relative Markdown/Obsidian syntax where unambiguous and
  stable UUID-backed resolution internally;
- custom properties that Markdown cannot express losslessly remain in
  frontmatter or a namespaced sidecar, never silently discarded;
- upload/download operates on folders or zip archives, not database dumps.

Geschichte owns the versioned tree. Datahike indexes and types its contents for
query and reactive views. The exact canonical-write boundary needs a round-trip
spec and property-based tests before the wiki editor is moved.

## Programs, custom UI and published sites

SCI widgets are user programs attached to knowledge/work artifacts. Their
evaluation should create ordinary runs/effects, with declared capabilities and
bounded execution, rather than being invisible UI magic.

Room website hosting is a publish projection from a Geschichte workspace:

1. chat/work produces source, data and assets on a governed branch;
2. a site-build run records compiler/effect output and previews an immutable
   artifact;
3. approval/capability gates deployment and domain/external egress;
4. a site-deploy run records exactly what became public;
5. the public site has a stable release ref and can be rolled forward/back.

This supports interactive product offerings without giving an agent an
unreviewed path from conversation to public production state.

## Required dvergr API changes

The current dvergr turn machinery already supports cooperative cancellation,
steering, a room-turn registry and non-triggering activity/error posts. The
missing public contract is:

1. Canonical room assignments and a dispatch planner over actor ids.
2. Canonical message author/audience/thread/caused-by fields preserved through
   `PRoomStore` and replay. Concretely, the Datahike store currently drops the
   envelope's `:to` and `:in-reply-to` values and does not round-trip general
   structured metadata; shared store-contract tests must cover those fields,
   idempotence and replay before Simmis stops its compatibility projection.
3. Stable run ids, supplied before evaluation and threaded into output messages
   and effects.
4. Lifecycle subscription emitting `{event run-id room-id actor-id status at
   cause}` rather than only `room running?`.
5. Snapshot lookup of active runs.
6. `cancel-run!` and `steer-run!` targeting one run, with idempotent outcomes.
7. Structured effect events/callbacks for tool/model/process activity; the
   existing room activity messages can remain a compatibility projection.
8. Consecutive agent-authored causal-hop accounting with a configurable high
   circuit-breaker limit and a visible diagnostic when it fires.

These changes belong in dvergr. Simmis should not maintain a second assignment,
run or effect registry.
