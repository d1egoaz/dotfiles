---
name: task-coordinator
description: Coordinate requested multi-step work with visible tasks, native subagents, follow-ups, or monitoring. Use for 2+ independent outcomes/repos/tasks or ongoing follow-up; skip one coupled task and one-time status checks.
---

# Task Coordinator

Coordinate a workstream through visible, isolated, independently reviewable
outcomes. Use this skill when a request has at least two independent outcomes,
repositories, or visible tasks, or asks for ongoing follow-up, PR babysitting,
monitoring, or a user-stated stopping condition. Do not use it for one tightly
coupled implementation outcome or a one-time status check.

## Establish the target

1. Resolve the exact goal, coordination key, scope, requested outcome, and
   stopping condition. Never treat a placeholder identifier as real.
2. For repository work, locate the Git root and use the host-supplied instruction
   chain. Read only missing repository instructions and skip aliases or
   duplicates. Keep each checkout boundary clear.
3. Choose one durable visible task per independent outcome or repository. The
   lead owns planning, delegation, sequencing, follow-ups, integration,
   verification, and final status; it must not reach into an execution task's
   isolated tree.

Read [`references/codex-task-creation.md`](references/codex-task-creation.md)
only when resolving saved projects or creating visible tasks. It covers
worktree/local choices and prompt mechanics.

## Route and name work

- Explicitly pass a model and reasoning effort for every visible task. Sol,
  Terra, and Luna are all valid task routes; select the route from the judgment
  that remains, not from a default or the lead's route.
- After scoping an execution task, reassess whether native subagents materially
  improve speed, specialization, context isolation, or independent verification.
  When useful, proactively spawn the minimum useful named roles and integrate
  their findings in the owning task.
- Keep naming tied to the route actually selected:
  - lead: `[<key>] 🤖 <goal>`
  - visible execution task: `[<key>] <model>-<effort> <scope>: <outcome>`
  - native subagent: `<key>_<model>_<effort>_<role>_<slice>`
  - native prompt label: `[<key>] <model>-<effort> <role>: <slice>`
- Include `Coordination key: <key>`, `Selected route: <model>-<effort>`, and
  the parent title in each child prompt. Keep the exact key across retries,
  forks, and follow-ups.

Every execution prompt must state the outcome, constraints, verification,
publication boundary, stopping condition, and a delegation contract requiring
lead-mediated communication and the naming schema above. A child owns its
subagent tree, implementation, integration, and verification. Subagents return
results to their parent; they do not peer-coordinate, discover peer IDs, or
change scope. Use configured native roles by default. Do not create a separate
review task solely to review another task unless the user asks for one.

## Follow through

- Wait with the runtime's bounded task control. Do not repeatedly poll unchanged
  state. Read a task when its result, blocker, or decision needs inspection.
- Send follow-ups only when new evidence or a changed user decision affects the
  next action. For explicit monitoring or babysitting, use the available
  heartbeat and stay quiet while state is unchanged.
- Treat the user's stopping condition as authoritative. Creation or
  coordination never authorizes commit, push, PR publication, merge, apply,
  deployment, production mutation, or communication outside the agent app.

Read [`references/codex-controls.md`](references/codex-controls.md) only when
selecting a native, visible-task, or App Server control. It contains exact
runtime mappings without loading them for ordinary coordination decisions.
