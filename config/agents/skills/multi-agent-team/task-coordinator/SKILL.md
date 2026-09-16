---
name: task-coordinator
description: Coordinate visible tasks and native subagents. Use when explicitly invoked, for 2+ independent deliverables or write-owning repos, or ongoing external-state follow-up; skip one coupled outcome and one-time status.
---

# Task Coordinator

Use this skill when explicitly invoked, for at least two independently
shippable outcomes or write-owning repositories, or for ongoing external-state
follow-up and a user-stated stopping condition. An ordinary continuation of one
task does not count. A reference-only repository is not a separate outcome.
Skip one tightly coupled implementation outcome and a one-time status check.

## Establish the target

1. Resolve the goal, coordination key, scope, outcome, and stopping condition.
   Never treat a placeholder identifier as real.
2. For repository work, locate the Git root, use the host instruction chain,
   read only missing instructions, and keep checkout boundaries clear.
3. Resolve the lead's saved project. Non-repository children inherit its exact
   project and local environment; use projectless only for a projectless lead.
   Read the task-creation reference for repository exceptions.
4. State which outcomes get visible tasks and which slices stay native
   subagents. If no visible task is justified, explain why in one sentence.
5. Whenever this skill applies, rename the lead `🤖 [<key>] <goal>` as soon as
   the key and goal are known. Do this before delegation for explicit use.
6. Treat explicit `$task-coordinator` invocation as a request for this workflow.
   For repository write work, create at least one visible implementation task
   unless the user keeps it in the lead. Without visible-task controls, report
   that limitation and use native subagents.
7. For implicit coordination, use one visible task per shippable outcome or
   write-owning repository. Keep reference-only repository analysis in a native
   `explorer`. The lead owns coordination, integration, and final status.

Read [`references/codex-task-creation.md`](references/codex-task-creation.md)
only when resolving projects or creating visible tasks.

## Route and name work

- Before creating or materially rerouting delegated work, read
  [`references/capability-routing.md`](references/capability-routing.md). Print
  its complete capability card in a fenced Markdown `text` code block, keep
  one-step utility work in the lead, and choose the cheapest adequate model and
  effort without a model-share quota.
- Explicitly pass a model and reasoning effort for every visible task. Sol,
  Terra, and Luna are all valid task routes; select the route from the judgment
  that remains, not from a default or the lead's route.
- Reassess whether native subagents materially improve speed, specialization,
  isolation, or verification; proactively spawn the minimum useful named roles.
- Keep naming tied to the route actually selected:
  - lead: `🤖 [<key>] <goal>`
  - visible execution task: `[<key>] <model-label>-<effort> <scope>: <outcome>`
  - native subagent: `<key>_<model-label>_<effort>_<role>_<slice>`
  - native prompt label: `[<key>] <model-label>-<effort> <role>: <slice>`
- Use `Sol`, `Terra`, or `Luna` in titles. Never put a raw model ID in a title;
  keep the exact ID in the child prompt and creation fields.
- Keep visible titles unique and at most 56 characters. Normalize a
  plan-supplied title to this schema with the scope and outcome first.
- Build the lead goal from the resolved user goal without a key, model, scope,
  or trailing punctuation. If the title exceeds 56 characters, keep the longest
  goal prefix that fits at a word boundary; hard-cut only when no boundary fits.
  Do not paraphrase or add an ellipsis. Preserve the robot emoji and key.
- Include `Coordination key: <key>`, `Selected model: <exact-model-id>`,
  `Selected effort: <effort>`, `Display route: <model-label>-<effort>`, and the
  parent title in each child prompt. Keep the exact key across retries, forks,
  and follow-ups.

Every execution prompt includes the card, selected route, rejected cheaper
routes, escalation conditions, outcome, constraints, verification, publication
boundary, stopping condition, lead-mediated delegation contract, and naming
schema. Visible tasks are user-owned, may interact directly with the user, and
own their complete scoped outcome. Native subagents return only to the parent;
they do not peer-coordinate, discover peer IDs, or change scope. Use a named
role only when its fixed route matches; otherwise use an explicit route with the
full role contract. Do not create a separate review task unless the user asks.

## Follow through

- Reassess the decomposition when a material follow-up creates or removes an
  independent outcome or write-owning repository.
- Reclassify every materially different follow-up. Override a visible task's
  model and effort per turn. Resume a native subagent only when its card still
  matches; otherwise create a distinctly scoped slice at the new route.
- Wait with bounded task control; do not repeatedly poll unchanged state.
- If a visible task needs human approval, let it ask once in that task and wait
  for the user's direct response. Do not proxy, quote, or duplicate the request;
  Codex surfaces the task as needing attention.
- Send follow-ups only for new evidence or changed decisions. For monitoring,
  use the heartbeat and stay quiet while state is unchanged.
- Treat the user's stopping condition as authoritative. Creation or
  coordination never authorizes commit, push, PR publication, merge, apply,
  deployment, production mutation, or communication outside the agent app.

Read [`references/codex-controls.md`](references/codex-controls.md) only when
selecting a native, visible-task, or App Server control. It contains exact
runtime mappings without loading them for ordinary coordination decisions.
