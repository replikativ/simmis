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

## The sandbox

An agent's code runs in an [SCI](https://github.com/babashka/sci) context with
a curated vocabulary, injected as namespaces:

| Namespace | For |
|---|---|
| `wiki` / `kb` | pages, blocks, attributes, typed properties |
| `kontor` | the room's double-entry book — `entry!`, `balances`, `accounts` |
| `proposal` | opening, filing and releasing proposals |
| `sheet` / `office` | spreadsheet and document intake |
| `screen` | screen captures the room has been granted |
| a shell | over a [muschel](https://github.com/replikativ/muschel) mount, so file tools and the shell see one filesystem |

Plus ordinary `datahike.api`, so an agent can query rather than being restricted
to a hand-built accessor for every question.

The vocabulary carries `:doc` and `:arglists` (`agents/vocab.clj`) so `doc`
works inside the sandbox. This matters more than it sounds: a capability an
agent cannot discover is a capability it does not have, and the failure looks
like the agent being unable to do something rather than being unable to find
it.

**The sandbox is a soft boundary.** It is a curated SCI context hardened
against the escapes we have found, not a VM. Do not run untrusted agents
against secrets you cannot rotate.

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

New agents store no model selection. Persona/template selection supplies the
role and system prompt, not a hidden model. An agent follows its owner's model
preference from Settings, then the product fallback when the owner has no
preference. Only an explicit per-agent override stores a model FAMILY
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

The concrete id is resolved when the participant is joined or rejoined. For
every provider, a model list from that provider's own endpoint answers which
ids the credential can currently reach (`is.simm.model.model-selection`). It does not supply pricing,
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
   participant when it joined. The current UI does not introspect that spec.

Participant construction calls the desired resolver once, validates the result,
puts the concrete provider/model into dvergr's participant spec, and reuses that
captured spec for subsequent turns. Owner-preference changes and catalog
refreshes can therefore change the displayed desired resolution and availability
without changing an already joined participant. The two may legitimately differ
until that participant leaves and rejoins.

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
spec. This reset does not imply per-turn resolution, and owner-preference or
catalog changes do not currently trigger it.

One native-provider quirk is load-bearing here: on OpenAI's configured
`/v1/chat/completions` path, the GPT-5.6 entries require `reasoning_effort`
`"none"` when tools are attached. dvergr sends that only to native OpenAI, so
tools work and server-side reasoning does not; an arbitrary compatible endpoint
does not receive an OpenAI-native field. Agents that need both belong on
`gpt-5.5` until the Responses API is spoken.
