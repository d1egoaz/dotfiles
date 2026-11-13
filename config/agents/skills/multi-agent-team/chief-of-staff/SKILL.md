---
name: chief-of-staff
description: Run T3 Code work as a Claude lead that plans, gets a cross-model council verdict, delegates to Codex workers, and validates with both providers. Also for "shipit" or "ship it". Use only in T3 Code.
---

# Chief of Staff

The lead plans, delegates, integrates, and owns the final claim; workers
execute. This is policy on T3 `delegate_task`, not a scheduler. It needs a
Claude lead with `delegate_task` and the seats below, which are the office
routes; otherwise say so and stop.

## Seats

Resolve routes once per thread with [delegation.md](references/delegation.md).

| Seat          | Route                                                    | Effort                                              |
|---------------|----------------------------------------------------------|-----------------------------------------------------|
| Lead          | this thread, `claudeAgent`, `claude-opus-5-5`            | `effort: medium`                                    |
| Council peers | `codex`, `gpt-6.1-sol`; `cursor`, `grok-4.7`             | `reasoningEffort: xhigh`; `reasoning_effort: xhigh` |
| Scout         | `claudeAgent`, `claude-haiku-5-5`                        | `effort: high`                                      |
| Worker        | `codex`, `gpt-6.1-sol`                                   | `reasoningEffort: medium`                           |
| Validators    | `claudeAgent`, `claude-opus-5-5`; `codex`, `gpt-6.1-sol` | `effort: medium`; `reasoningEffort: medium`         |

Use `high` for validators with security, data, or migration risk. Run the
lead thread at `high` only for council synthesis or hard debugging.

Match the seat to the work, and judge cost per accepted slice, not per token:

- Scout: read, find, extract, summarize. Read-only, no judgment calls.
- Worker: implement, modify, analyze, test.
- Validators: review.
- Lead and council: plan, architect, debug hard failures, decide.

Route scouts through `delegate_task`, never native `Agent` with `haiku`:
Claude settings map that alias to Sonnet.

## Size the work

First check whether an existing mechanism (an already-enabled auto-merge, open
PR, skill) already covers it; if so, use it and stop.

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
7. Ship: once the user approves the result, run
   [shipping.md](references/shipping.md) through merge.

## Ledger

For delegated work, keep one table in
`~/.cache/agent-checklists/<repo>-<branch>.md` so it outlives the session:
key, seat, route, title, taskId, threadId, write set, state, evidence. Title
every child `[cos:<run-key>] <seat> <slice>`; titles are the recovery index. The
run key names the run, never a seat. A
lifecycle step you skip stays in the ledger as `skip: <reason>`.

## Limits

T3 enforces child context isolation, the runtime-mode ceiling, worktree
checkouts, and cancellation. Council approval, file ownership, scope, and
retry budgets are behavioral; say so when it matters. Delegation adds no
authorization, and every repository and host gate still applies.
