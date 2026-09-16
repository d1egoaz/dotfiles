# Capability-aware routing

Use this policy before creating delegated work and whenever a follow-up changes
what a child must do. The lead supplies semantic judgment; the policy constrains
the result to the cheapest adequate GPT-5.6 model and effort. Do not target a
model-share percentage.

## Decide whether to delegate

Keep a one-step lookup, command, or short transformation in the lead. Delegate
a meaningful bounded unit when cost, context isolation, specialization,
independent verification, or parallelism materially helps. Decomposition comes
before model escalation: narrow a broad task when independent slices can be
verified separately.

## Print the capability card

Print every field below before creating or materially rerouting work. Use the
listed categorical values, not an additive score. Each dimension can establish
a minimum route; easy dimensions never offset a critical one.

```text
Routing scorecard
- Work unit and expected output:
- Scope: local | bounded multi-component | cross-system
- Ambiguity: low | medium | high
- Judgment: procedural | synthesis | adversarial | exceptional
- Verification: objective | partial | subjective/unknown
- Adaptivity: bounded | iterative | open-ended
- Consequence if wrong: low | material | high
- Cheapest capable route:
- Why cheaper routes are insufficient:
- Escalate when:
```

High consequence alone does not select a premium model. Couple consequence to
the unresolved ambiguity, judgment, and verification needs. Production and
publication gates remain unchanged by route selection.

## Select the cheapest adequate route

| Route | Use when |
|---|---|
| Keep in lead | One-step utility work whose delegation overhead exceeds its value. |
| Luna-medium `utility` | Read-only bounded multi-step work with a clear output, procedural judgment, and objective verification. |
| Luna-xhigh `worker` | Scoped implementation or diagnosis with exact acceptance checks, limited coupling, and no broad synthesis. |
| Terra-medium `explorer` | Broad mapping, ownership discovery, or multi-source synthesis with ongoing judgment. |
| Terra-high `reviewer` | Independent correctness, security, behavior, or design review requiring adversarial judgment. |
| Terra-xhigh `evidence-auditor` | Cross-system lifecycle or runtime reconstruction with contradictions, partial verification, or substantial unresolved judgment. |
| Sol-high or Sol-xhigh explicit route | Exceptional reliability or deep ambiguity remains after useful decomposition, especially when combined with high consequence. |

Use a configured native role only when its fixed model and effort match the
card. Otherwise use an explicit unnamed model/effort route and copy the needed
read-only or write-owning role contract into the prompt. Visible tasks always
receive the selected model and effort explicitly.

## Reclassify and escalate

Reclassify every materially different follow-up. For a visible task, override
the model and effort on that turn. Resume a native subagent only when the new
card still matches its fixed route. When the capability requirement changes,
create a distinctly scoped slice at the new route; do not duplicate unfinished
work.

Escalate Luna to Terra or Terra to Sol only when one of these occurs:

- scope expands beyond the card;
- direct evidence conflicts and requires broader synthesis;
- objective verification exposes a reasoning defect rather than an execution
  failure;
- two materially different reasoning approaches fail; or
- independent review identifies a material correctness gap requiring greater
  judgment.

Do not escalate for missing credentials, sandbox restrictions, unavailable
systems, absent evidence, approval boundaries, or ordinary tool outages. Report
those as blocked or unknown. Never repeat a side-effectful action as a routing
retry. Reconstruct current state and exact authorization before considering any
new mutation.

## Contract scenarios

- A one-step status check stays in the lead.
- A bounded read-only inventory selects Luna-medium `utility`.
- Scoped implementation with objective tests selects Luna-xhigh `worker`.
- Broad ownership mapping selects Terra-medium `explorer`.
- Independent correctness review selects Terra-high `reviewer`.
- Conflicting lifecycle and runtime evidence selects Terra-xhigh
  `evidence-auditor`.
- Exceptional unresolved judgment after decomposition crosses the Sol gate.
- A simple follow-up inside a Terra task is reclassified to Luna when its card
  permits it.
- Authentication or tool failure does not cause model escalation.
- A side-effectful action is never automatically retried.
