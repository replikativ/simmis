# Tool authorization and delegation

Simmis does not prompt for routine tool calls. A call executes when every
applicable authority agrees:

1. Dvergr admitted the exact tool definition to the AgentDef/Run.
2. Simmis ReBAC admits the acting party on the target application object.
3. Kontor admits and reserves any resource vector the effect consumes.
4. The sandbox contains the implementation to its filesystem, network,
   process, secret, time and output capabilities.

These are independent checks. EACL should answer object authority; it should
not encode a tool allowlist or an account balance as artificial relationships.
`is.simm.model.access/can?` remains the application seam while its current
Datahike traversal is replaced by EACL's `IAuthorization` implementation.

## Identity

Humans and agents are both parties. Dvergr's `:party/<uuid>` actor identity is
derived from the same Simmis party UUID, so room relationships apply without an
agent-specific permission table.

A durable agent identity is not one execution. Narrow, temporary authority
should normally be granted to a Run; durable room membership belongs to the
party. This prevents one hired task from silently expanding every future
performance by that agent.

## Delegation

Delegation is explicit attenuation, not automatic inheritance. A grant records:

- stable grant id and parent grant id;
- issuer and grantee (party or Run);
- room and target object scope;
- permitted actions/tools/effects;
- validity/revocation frontier;
- Kontor allocation or reservation ids.

Creating a child grant is one governed operation. It succeeds only if:

- EACL says the issuer may delegate on the parent resource;
- the child's object/action/tool scope is a subset of the parent's;
- Kontor atomically transfers a resource vector from parent to child;
- sandbox capabilities can be attenuated to the requested scope.

EACL relationships answer who may issue/use/revoke the grant. The application
transaction checks subset containment because EACL's current schema language
does not express arbitrary scope intersection or numeric conservation. Kontor
is authoritative for the latter: child allocations debit the parent rather
than copying its budget ceiling.

Revocation blocks future calls. Already committed effects remain causal facts;
in-flight calls use the policy basis recorded when admitted and may be cancelled
according to their effect contract.

## Decisions and receipts

Every call records an authorization receipt with the decision, independent
sources, subject, action, resource and optional grant id. `:authorized` and
`:denied` are policy outcomes. `:requires-decision` is reserved for an explicit
semantic checkpoint; it is not a generic tool approval mode.

The receipt belongs to the runtime, not the tool implementation. A custom SCI
function cannot fabricate the authority under which it ran. The receipt and
Kontor entries form the evidence shown by the causal Run inspector.
