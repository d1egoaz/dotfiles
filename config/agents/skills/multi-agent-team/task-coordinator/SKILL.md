---
name: task-coordinator
description: Coordinate independent outcomes and ongoing external-state follow-up with one lead, visible child tasks, and bounded subagents. Use when invoked; keep one-step judgment in the lead.
---

# Task Coordinator

One lead owns goal, sequence, integration, and verification. Apply
[capability-routing.md](references/capability-routing.md). Keep one-step or
tightly coupled judgment stays in lead; multiple files do not justify
children.

Choose `economy`, `balanced`, or `frontier` from capability, then resolve it
through the active Codex runtime. Record runtime, tier, model label, and
requested/effective effort.

Use `@spawn-subagent-economy`, `@spawn-subagent-balanced`, or
`@spawn-subagent-frontier` for a generic native subagent; the parent handoff
supplies outcome and constraints. Use canonical `@spawn-subagent-*`
specialized launchers for fixed roles (see `capability-routing.md`).
These names invoke native spawning, never visible child-task creation. Old role
names remain compatibility aliases; new instructions use canonical names.

## Split and route

1. Define goal, outcomes, dependencies, and stopping condition.
2. Use visible tasks for independent inspectable outcomes; use canonical
   `@spawn-subagent-*` launchers for bounded slices or review. Never use a
   native launcher as a visible task type.
3. Before delegation/rerouting, read the routing reference, select tier and
   effort, resolve runtime/model label, and explain both.
4. Give every child the handoff below. Before visible tasks read
   [codex-task-creation.md](references/codex-task-creation.md); read
   [codex-controls.md](references/codex-controls.md) when operating controls.

## Handoff

- Outcome, acceptance criteria, owned files/resources, constraints, and
  dependencies with no concurrent writers.
- Exact tier/model-label/effort, skills/context, verification, authorization,
  approval boundary, and stopping condition.

Visible tasks own outcomes through verification and report evidence to the lead;
they ask the user directly for missing approval. Creation adds no authorization.

Native subagents return results or blockers to their parent. They do not
peer-coordinate, spawn descendants, or declare overall completion.

## Visible-task status titles

Use the visible task title as a compact coordination overlay. It helps people
scan the task list, but does not replace the host's lifecycle or runtime state.
Apply it only to visible child tasks: native subagents have no equivalent
user-facing title surface, so the lead tracks their state and evidence.

| Prefix | State | Use when |
| --- | --- | --- |
| `🆕` | spawned | Created or still completing setup; no meaningful work evidence yet. |
| `🔄` | in progress | The owner is doing authorized work or safely retrying after recovery. |
| `⛔` | blocked | No authorized runnable action remains because of access, approval, or an external dependency. |
| `❌` | error | An unexpected tool, runtime, or task failure has no safe in-scope recovery yet. |
| `❓` | needs info | A specific user decision, input, or clarification is required; the visible task asks the user directly. |
| `✅` | done | The task's acceptance criteria have been verified, not merely attempted or reported. |

Create a visible task with the `🆕` prefix. Once it is addressable, the owner
updates its own title with the host title control at each semantic transition;
the lead reconciles a title when the evidence says it is stale. Do not rename
for routine commentary, waits, or repeated observations of the same state.
Preserve the key, route, and outcome after the prefix. A retry returns to
`🔄`; it does not erase the previous error evidence from the task record.

## Finish

Track owner, route, dependencies, state, and evidence. On new evidence resolve
blockers, retry/reroute, and integrate. Verify acceptance and cross-task
behavior; report blockers/unverified outcomes and respect the stop condition.
