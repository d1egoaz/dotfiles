---
name: chief-of-staff
description: Run T3 Code work as a Claude lead that plans, gets a cross-model council verdict, delegates execution to Codex workers, and validates with both providers. Use only when invoked in T3 Code.
---

# Chief of Staff

The lead plans, delegates, integrates, and owns the final claim; workers
execute. This is policy on T3 `delegate_task`, not a scheduler. It needs a
Claude lead with `delegate_task` and the seats below, which are the office
routes; otherwise say so and stop.

## Seats

Resolve routes once per thread with [delegation.md](references/delegation.md).

| Seat | Route | Effort |
|---|---|---|
| Lead | this thread, `claudeAgent`, `claude-opus-5-5` | as set |
| Council peer | `codex`, `gpt-6.1-sol` | `reasoningEffort: xhigh` |
| Worker | `codex`, `gpt-6.1-sol` | `reasoningEffort: medium` |
| Validators | `claudeAgent`, `claude-opus-5-5`; `codex`, `gpt-6.1-sol` | `effort: medium`; `reasoningEffort: medium` |

Use `high` for validators with security, data, or migration risk.

## Size the work

- Direct: one-step change or lookup with a deterministic check. The lead does
  it and runs the check. Small review fixes stay here too.
- Routine: bounded change on an existing pattern or approved plan. Workers,
  checks, one Claude validator; no council.
- Substantial: new interface or boundary, 3+ write sets, migration, security
  or data risk, or real doubt. Full lifecycle.

## Lifecycle

1. Restate the user's goal in their words, then define success criteria,
   write sets, and dependencies. Open every brief and the final report with
   that goal.
2. Substantial only: run [council.md](references/council.md) to an agreed or
   escalated plan.
3. Delegate one worker per disjoint write set; launch independent sets in one
   message; serialize or isolate overlaps.
4. Integrate: check each report against the diff and checks, not its summary.
5. Validate with [validation.md](references/validation.md); send defects back.
6. Finish, retry, escalate, pause, or resume per
   [escalation.md](references/escalation.md).

## Ledger

For delegated work, keep one table in
`~/.cache/agent-checklists/<repo>-<branch>.md` so it outlives the session:
key, seat, route, title, taskId, threadId, write set, state, evidence. Title
every child `[cos:<key>] <seat> <slice>`; titles are the recovery index. A
lifecycle step you skip stays in the ledger as `skip: <reason>`.

## Limits

T3 enforces child context isolation, the runtime-mode ceiling, worktree
checkouts, and cancellation. Council approval, file ownership, scope, and
retry budgets are behavioral; say so when it matters. Delegation adds no
authorization, and every repository and host gate still applies.
