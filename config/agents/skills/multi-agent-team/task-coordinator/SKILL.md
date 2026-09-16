---
name: task-coordinator
description: Coordinate visible tasks and native subagents. Use when explicitly invoked, for 2+ independent deliverables or write-owning repos, or ongoing external-state follow-up; skip one coupled outcome and one-time status.
---

# Task Coordinator

Use when invoked, for 2+ shippable outcomes or write-owning repositories, or
ongoing follow-up. Skip one coupled outcome, one-time status, and reference-only
repositories.

## Required flow

1. Resolve the key, goal, outcomes, repositories, constraints, verification,
   publication boundary, and stopping condition.
2. Assign visible tasks and native slices. If no visible task fits, say why.
3. Rename the lead `🤖 [<key>] <goal>`. One lead owns each key.
4. Before delegation or material reroute, read
   [`references/capability-routing.md`](references/capability-routing.md), print
   its completed scorecard in a fenced Markdown `text` block, and choose the
   cheapest adequate model and effort.
5. Before creating a visible task, read
   [`references/codex-task-creation.md`](references/codex-task-creation.md).
6. Read [`references/codex-controls.md`](references/codex-controls.md) only to
   choose or operate a runtime control.

Explicit repository write work needs a visible implementation task unless the
user keeps it in the lead. Without visible controls, report that and use native
subagents.

## Naming and prompts

- Lead: `🤖 [<key>] <goal>`
- Visible task: `[<key>] <model-label>-<effort-code> <action> <object>[: <outcome>]`
- Native subagent: `<key>_<model-label>_<effort-code>_<role>_<slice>`
- Native prompt label: `[<key>] <model-label>-<effort-code> <role>: <slice>`

Titles use `Sol`, `Terra`, or `Luna`, never raw IDs. Visible titles are unique,
at most 72 characters, with action/object before context. Prompts keep exact
model/full effort and routing, outcome, verification, publication,
stop, and delegation fields. Read the task-creation reference for codes.

Visible tasks are user-owned and own their outcome. They ask the user once for
needed approval; the lead never proxies it. Native subagents return only to the
parent, do not peer-coordinate or spawn, and never declare overall completion.

## Follow through

Reclassify material follow-ups. Override visible routes per turn; resume native
agents only while their route fits. Wait without polling unchanged state.
Creation never authorizes commit, push, PR, merge, apply, deployment,
production mutation, or external communication.
