# Capability routing

Keep one-step work in the lead. Delegate when cost, isolation, specialization,
verification, or parallelism justifies it. Decompose before escalating. Do not
target model share.

Print the completed card as a fenced Markdown `text` block, never prose, a list,
or a table. Dimensions are independent gates, not an additive score.

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

## Routes

- Lead: one-step work where delegation costs more than it saves.
- Luna-medium `utility`: bounded read-only work with objective checks.
- Luna-xhigh `worker`: scoped implementation or diagnosis with exact checks.
- Terra-medium `explorer`: broad mapping or multi-source synthesis.
- Terra-high `reviewer`: independent adversarial correctness or design review.
- Terra-xhigh `evidence-auditor`: conflicting cross-system lifecycle evidence.
- Sol-high/xhigh: exceptional reliability or deep ambiguity remains after
  decomposition, especially with high consequence.

High consequence alone does not select a premium model. Use a named role only
when its route matches; otherwise use an explicit route with the needed role
contract.

Reclassify material follow-ups. Escalate only for expanded scope, conflicting
evidence, objective reasoning failure, two failed approaches, or a material
review finding. Missing access, approval, data, or tools means blocked, not
escalation. Never repeat a side effect; reconstruct state and authority first.
