# Capability routing

Generated from `nix/data/agent-routing.toml` by `agent-routing-generate`.
Pick a role by what the work may touch; the registry resolves its tier,
model, and effort for the active runtime. Never promote a blocked route
to a more expensive tier.

## Tiers

| Tier | Use when |
|---|---|
| `economy` | Bounded, objectively checkable work. |
| `balanced` | Exploration, synthesis, or correctness review. |
| `frontier` | The lead: deep ambiguity and integration judgment. |

## Native-subagent roles

- `@spawn-subagent-explore` -> `balanced`, `low`, `read-only`: Read-only explorer for mapping code paths, inventories, and evidence gathering.
- `@spawn-subagent-implement` -> `economy`, `xhigh`, `workspace-write`: Execution-focused worker for bounded implementation and fixes after scope and ownership are clear.
- `@spawn-subagent-review` -> `balanced`, `low`, `read-only`: Read-only reviewer for merge-blocking correctness, security, and regression problems.

An unnamed spawn uses the profile default route (`economy`) and inherits
the parent sandbox; use it for a bounded batch of procedural commands.

## How many agents

- A lookup or one-step check stays in the lead.
- Independent reads fan out, one explorer per unit (service, repo, area).
- Each write set gets exactly one implementer; never run parallel writers on shared files.
- Review is one independent pass before publication.

## Naming

Spawned instances use `<key>_<tier>_<model-label>_<effort-code>_<role>_<slice>`;
visible task titles use `<state-prefix> [<key>] <tier>-<model-label>-<effort-code> <action> <object>`.
The `task-coordinator` skill defines the allowed visible-task status prefixes.

## Gates

- Reclassify a materially different follow-up instead of inheriting its route.
- Authentication, access, approval, or tool failure is blocked, not escalation.
- Never repeat a side-effectful action as a routing retry; reconstruct state and authority first.
- High consequence alone does not select a premium tier.
