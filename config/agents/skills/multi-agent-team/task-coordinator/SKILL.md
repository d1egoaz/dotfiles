---
name: task-coordinator
description: Coordinate independent outcomes and ongoing external-state follow-up with one lead, parallel read subagents, single-owner writers, and evidence checks. Use when invoked; keep one-step judgment in the lead.
---

# Task Coordinator

One lead owns the goal, sequence, integration, and verification. Delegate only
for parallel independent reads, context isolation, a bounded write set, or
independent review; multiple files alone do not justify a child. Roles, tiers,
and naming live in [capability-routing.md](references/capability-routing.md).

## Pattern

1. Define the goal, the independent outcomes, and what done means for each.
2. Fan out independent reads, one `@spawn-subagent-explore` per unit. Give
   each write set one `@spawn-subagent-implement`. Run one
   `@spawn-subagent-review` before publication. Use an unnamed spawn for a
   bounded batch of procedural commands, never for one trivial command.
3. Use a visible task only for an outcome with its own repository, PR, or
   long-running follow-up. Read
   [codex-task-creation.md](references/codex-task-creation.md) first and
   [codex-controls.md](references/codex-controls.md) when steering.
4. Check each child's evidence before accepting it. A success claim is not
   completion.
5. Reassess the route when diagnosis becomes authorized execution or a blocker
   changes the work.

## Handoff

Send one message with:

- Task and context, including what the lead already knows.
- Done means: a checkable finish line.
- Stop and ask if: the conditions that need the parent.
- Owned files or resources (no concurrent writers) and authorization.

Native subagents return results or blockers only to their parent; they do not
peer-coordinate, spawn descendants, or declare overall completion. Visible
tasks own their outcome through verification and ask the user directly for
missing approval. Creating a task adds no authorization.

## Track and finish

For more than one outcome, keep one table: outcome, owner, evidence, and state
(verified, blocked, unverified). On long runs keep it in the host task list or
an untracked checklist file and update it as children report. End with
**Blocked on me**, **Changed**, and **Found**.
