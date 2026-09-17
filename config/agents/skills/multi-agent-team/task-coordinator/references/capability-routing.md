# Capability routing

Select a capability tier first, then resolve it through the active
Codex runtime in `nix/data/agent-routing.toml`. Resolve the concrete model
and model label through that runtime; never promote a blocked route to a
more expensive tier.

## Tiers

| Tier | Use when |
|---|---|
| `economy` | Bounded, objectively checkable work. |
| `balanced` | Exploration, synthesis, correctness review, or evidence work. |
| `frontier` | Deep ambiguity or exceptional judgment remaining after decomposition. |

## Runtime resolution

Resolve the selected tier/model label through the active Codex runtime;
keep requested/effective effort separate and exact IDs in runtime fields.

## Native-subagent launchers

Stable `@spawn-subagent-*` names mean native spawning, never
visible tasks, with no concrete model.

Generic tier launchers (parent outcome/constraints; inherited boundary):

- `@spawn-subagent-economy` -> `economy`, `xhigh`, inherit parent sandbox and approval.
- `@spawn-subagent-balanced` -> `balanced`, `high`, inherit parent sandbox and approval.
- `@spawn-subagent-frontier` -> `frontier`, `xhigh`, inherit parent sandbox and approval.

Specialized tier launchers (fixed role contracts):

- `@spawn-subagent-economy-check` -> `utility`, `economy`, `medium`.
- `@spawn-subagent-economy-implement` -> `worker`, `economy`, `xhigh`.
- `@spawn-subagent-balanced-explore` -> `explorer`, `balanced`, `medium`.
- `@spawn-subagent-balanced-review` -> `reviewer`, `balanced`, `high`.
- `@spawn-subagent-balanced-audit` -> `evidence-auditor`, `balanced`, `xhigh`.

Legacy aliases remain generated: `@evidence-auditor`, `@explorer`, `@reviewer`, `@utility`, `@worker`; new instructions use canonical launchers.

Generic launchers have no role contract and omit sandbox/approval fields so the native subagent inherits the parent boundary.

## Scorecard

For every delegated unit, record:

```text
Routing scorecard
- Work unit and expected output:
- Scope: local | bounded multi-component | cross-system
- Ambiguity: low | medium | high
- Judgment: procedural | synthesis | adversarial | exceptional
- Verification: objective | partial | subjective/unknown
- Adaptivity: bounded | iterative | open-ended
- Consequence if wrong: low | material | high
- Tier:
- Runtime:
- Resolved model label:
- Requested effort:
- Effective effort:
- Cheapest capable route:
- Why cheaper routes are insufficient:
- Escalate when:
```

Spawned instances use `<key>_<tier>_<model-label>_<effort-code>_<role>_<slice>`;
visible task titles use `[<key>] <tier>-<model-label>-<effort-code> <action> <object>`.

## Gates and follow-ups

- A one-step status check stays in the lead.
- Bounded inventory -> `@spawn-subagent-economy-check`; implementation -> `@spawn-subagent-economy-implement`.
- Ownership mapping -> `@spawn-subagent-balanced-explore`; review -> `@spawn-subagent-balanced-review`; lifecycle audit -> `@spawn-subagent-balanced-audit`.
- Exceptional unresolved judgment after decomposition crosses the frontier gate.
- Reclassify a materially different follow-up instead of inheriting its route.
- Authentication, access, approval, or tool failure is blocked, not escalation.
- Never repeat a side-effectful action as a routing retry; reconstruct state and authority first.
- High consequence alone does not select premium model.

Use named roles only when their fixed route matches the scorecard.
