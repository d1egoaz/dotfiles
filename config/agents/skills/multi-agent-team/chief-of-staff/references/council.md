# Council

Run it for substantial work, and again when execution would change an
approved interface, data model, dependency, or module boundary. During a
substantial run, a routine choice the approved plan does not cover may get
one pass from one peer and no further rounds. Routine work has no council.

The council has two peers, Codex and Grok (Seats table). Name their seats
`peer-codex` and `peer-grok` in titles and request IDs.

For a new interface, every plan starts from usage: two or three real call
sites first, then types derived from them. When the types and the call
sites disagree, change the types, not the usage.

## Protocol

1. Lead plan: write the lead's plan into the ledger before contacting the
   peers, so it cannot drift toward their answers.
2. Blind pass: one `delegate_task` per peer, role `design`, both in one
   message with the same brief. Send the problem, constraints, success
   criteria, and repository paths, not the lead's plan or the other peer's.
   Ask for the approach, rejected alternatives with reasons, assumptions,
   risks, its least certain parts, and how to test the design. Tell it not
   to edit files; T3 cannot enforce read-only.
3. Compare the three plans: list agreements, disagreements, and unshared
   assumptions, noting who holds each position. Settle what evidence can
   settle, using code, docs, or a cheap worker probe.
4. Round 1: a new `delegate_task` per peer with all three plans and the open
   disagreements. Ask each peer to critique the lead plan and the other
   peer's, defend or revise its own, and mark each disagreement resolved or
   open, with evidence.
5. Round 2, only if round 1 left a material disagreement (a conceded point
   or a peer's acceptance is not one): a new `delegate_task` to each peer
   that still holds one, with the narrowed list and new evidence. There is
   no round 3.
6. Agreement requires each peer's latest response to accept the plan
   explicitly. Silence, a partial answer, a failure, or a timeout from either
   peer is not agreement, and one peer's acceptance does not outvote the
   other's objection.

## Unresolved

After round 2, record each open disagreement: every position and who holds
it, evidence, cost of being wrong, and reversibility. If the choice is reversible and low
impact, the lead decides, records why, and tells the user. Otherwise ask the
user before execution.

## Decision record

Keep it in the ledger's Decisions section and pass the relevant slice to each
worker: decision, rationale, rejected alternatives, open risks, and what
reopens the council.
