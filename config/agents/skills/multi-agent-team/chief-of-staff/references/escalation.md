# Escalation

## Budgets

| Loop | Limit | Then |
|---|---|---|
| Council | blind pass and 2 rounds | lead decides if reversible, else ask |
| Transient child failure | 1 retry of the same contract | escalate |
| Repair per finding | 2 fix-and-revalidate cycles | escalate |

Never raise effort or switch model as a retry.

## Failures

- Transient (provider error, failed turn, setup timeout): read the child's
  thread and the checkout first; reconstruct what it changed before any
  retry, and never repeat a side-effectful step blind.
- Blocked (auth, access, approval, missing tool or model, or a provider that
  still cannot start turns after the retry): not retryable. Report exactly
  what is missing; continue other authorized work. Never move a blocked seat
  to another route without the user's approval.
- Codex fails at start with "provider could not start this turn": confirm
  the child ran with `runtimeMode: "auto"` (see delegation.md). Managed Codex
  requirements reject `full-access` without T3 showing why. If `auto` also
  fails, the seat is blocked.
- Stalled (a child waits on a question or approval, or the user reports it
  stuck): inspect once with `t3_thread_read` (`view: "activity"`, small
  `limit`). Answer within scope or relay to the user; on that evidence only,
  `task_cancel` with a reason, then retry or escalate. A wait timeout or a
  quiet async child is not a stall: follow `next` and end the turn.
- Failed acceptance: a repair cycle, not a retry.

## Recovery

T3 keeps child threads and results after the lead stops. On resume:

1. Read the ledger file. Then `t3_thread_list` with `includeSubagents: true`
   and `titleContains: "[cos:<key>]"`, or `"[cos:"` when the key is lost.
2. `t3_thread_read` each child's final result, or `task_status` for a known
   finished `taskId`.
3. Rebuild the ledger; check `git status` and branches before trusting any
   writer's claim.
4. `task_cancel` duplicates or orphans so one owner remains per write set. A
   child with no recorded `taskId` is reported to the user by thread link,
   not guessed at.

## Finish

Done means every acceptance criterion has evidence, deterministic checks
pass, no blocker or unwaived major remains, and every child reported or was
cancelled. Stop and report when a budget runs out, a blocker persists, or the
user must decide. Never claim success without evidence.

Track seat, model, and effort per child in the ledger; these tools expose no
token or cost data. End with the ledger, then **Blocked on me**, **Changed**,
and **Found**.
