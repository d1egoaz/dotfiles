# Council

Run it for substantial work, and again when execution would change an
approved interface, data model, dependency, or module boundary. During a
substantial run, a routine choice the approved plan does not cover may get
one peer pass and no further rounds. Routine work has no council.

## Protocol

1. Lead plan: write the lead's plan into the ledger before contacting the
   peer, so it cannot drift toward the peer's answer.
2. Blind pass: one `delegate_task` to the council peer, role `design`. Send
   the problem, constraints, success criteria, and repository paths, not the
   lead's plan. Ask for the approach, rejected alternatives with reasons,
   assumptions, risks, its least certain parts, and how to test the design.
   Tell it not to edit files; T3 cannot enforce read-only.
3. Compare: list agreements, disagreements, and unshared assumptions. Settle
   what evidence can settle, using code, docs, or a cheap worker probe.
4. Round 1: a new `delegate_task` with both plans and the open
   disagreements. Ask the peer to critique the lead plan, defend or revise its
   own, and mark each disagreement resolved or open, with evidence.
5. Round 2, only if disagreements remain: a new `delegate_task` with the
   narrowed list and new evidence. There is no round 3.
6. Agreement requires the peer's latest response to accept the plan
   explicitly. Silence, a partial answer, a failure, or a timeout is not
   agreement.

## Unresolved

After round 2, record each open disagreement: both positions, evidence, cost
of being wrong, and reversibility. If the choice is reversible and low
impact, the lead decides, records why, and tells the user. Otherwise ask the
user before execution.

## Decision record

Keep it in the ledger and pass the relevant slice to each worker: decision,
rationale, rejected alternatives, open risks, and what reopens the council.
