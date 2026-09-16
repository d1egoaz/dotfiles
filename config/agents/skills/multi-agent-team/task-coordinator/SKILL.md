---
name: task-coordinator
description: Coordinate independent outcomes and ongoing external-state follow-up with one lead, visible child tasks, and bounded subagents. Use when invoked or those needs arise; keep simple or tightly coupled work in the lead.
---

# Task Coordinator

One lead owns the goal, sequencing, integration, verification, and completion.
Keep simple or tightly coupled work in the lead. Multiple files, steps, or
reference repositories alone do not justify child tasks. Explicit invocation
does not require delegation.

## Split and route

1. Define the goal, verifiable outcomes, dependencies, and stopping condition.
2. Use visible child tasks for independent outcomes the user can inspect and
   approve. Use native subagents for bounded slices or independent review.
   Delegate when the benefit exceeds its cost.
3. Before delegation or a material reroute, read
   [capability-routing.md](references/capability-routing.md). Select model and
   effort explicitly and give a brief user-facing reason.
4. Give every child the handoff below. Before creating visible tasks, read
   [codex-task-creation.md](references/codex-task-creation.md).
5. Use only exposed host controls. If visible tasks are unavailable, say so;
   keep outcomes visible in lead updates and use native subagents where useful.
   Read [codex-controls.md](references/codex-controls.md) when operating controls.

## Handoff

- Outcome and acceptance criteria.
- Owned files/resources, constraints, and dependencies. No concurrent writers
  to the same files or resources.
- Exact model and effort, relevant skills, and required context.
- Verification, existing authorization, approval boundary, and stopping condition.

Visible tasks own their outcome through verification and report evidence to the
lead. They ask the user directly for missing approval, then wait on that action
while other authorized work continues. Existing authorization remains
valid; task creation adds none. The lead never proxies approval.

Native subagents return results or blockers only to their parent. They do not
peer-coordinate, spawn descendants, or declare overall completion.

## Finish

Track each outcome's owner, route, dependencies, state, and evidence.
On new evidence, resolve blockers, retry or reroute when justified, and integrate
completed work. Verify acceptance criteria and cross-task behavior before
reporting completion. Report blockers and unverified outcomes. Respect the
user's stopping condition.
