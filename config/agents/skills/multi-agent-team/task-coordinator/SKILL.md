---
name: task-coordinator
description: Coordinate multi-step goals through visible agent tasks and native subagents. Use when the user asks to coordinate work through completion, create and follow tasks, babysit a PR, keep workstreams posted, or monitor until a stated condition. Do not use for ordinary single-task implementation or a one-time status check.
---

# Task Coordinator

## Purpose

Coordinate an explicitly requested workstream from a lead task while keeping each durable outcome visible, isolated, and independently reviewable. This skill owns cross-task mechanics; applicable instruction files own model routing, subagent roles, domain procedures, and general execution policy.

## Establish The Target

1. Resolve the exact goal, coordination key, target scope, requested outcome, and stopping condition. Never treat a placeholder identifier as real.
2. For repository work, locate the actual Git root and read the applicable instruction files before creating work.
3. In Codex, list saved projects and prefer an exact saved Git project when available; only a project with `isGitRepository = true` can provide an app-managed worktree. In another runtime, use its native project and isolation controls when available.
4. If only a saved non-Git umbrella project is available, use it as the coordination surface and include the exact nested repository path in each task prompt. Do not require every nested repository to be registered separately.
5. Stop only when the target scope cannot be resolved or no suitable coordination surface is available.

## Create Visible Tasks

- Create a separate visible task only when the user explicitly requests coordination, delegation, or a new task and the runtime provides a native task control. Do not create tasks for a status question or an ordinary single-task request. If visible task controls are unavailable, keep ownership in the lead and use native subagents instead of imitating task creation through unmanaged shell sessions.
- Choose one stable coordination key before creating children. Prefer a shared ticket or project identifier; otherwise derive a short uppercase slug from the goal. Reuse the exact key for every child, retry, fork, and follow-up task in that workstream.
- Rename the lead task `[<key>] 🤖 <goal>` as soon as the key and goal are known. Use a concise outcome or purpose after the emoji, not `orchestrator`, `coordinator`, the key repeated as a title, or a repository name alone. Examples: `[PROJ-123] 🤖 Add retry-safe delivery`, `[RELEASE-AUTO] 🤖 Continuously promote verified releases`, or `[WEB-PROFILING] 🤖 Compare web worker profiles`. Reserve the robot emoji for the lead task. If the runtime cannot rename the current task, report that limitation instead of silently leaving an ambiguous title.
- Title each execution task `[<key>] <model>-<effort> <scope>: <outcome>`, where the model uses a compact stable label such as `Sol`, `Terra`, or `Luna`, the effort remains lowercase, and scope is the repository, service, or external system that owns the work. Examples: `[PROJ-123] Luna-xhigh payments-api: add retry policy`, `[RELEASE-AUTO] Terra-high image-updater: validate promotion`, or `[WEB-PROFILING] Terra-medium observability: inspect web profiles`. Keep the prefix, model-effort label, and scope intact when shortening the outcome.
- Derive the title's model-effort label from the route actually selected at task creation, not from a default or intended route. If work is handed to a replacement task with a different route, give that replacement an accurate label rather than relabeling the original task.
- Pass the title explicitly when creating the task. If the runtime cannot set it at creation, rename the task immediately. Include `Coordination key: <key>`, `Selected route: <model>-<effort>`, and the parent task title in the child prompt so ownership and routing survive outside the sidebar.
- Use one visible task per independent durable outcome or repository.
- In Codex, when the exact repository is a saved Git project, create an app-managed worktree task unless the user explicitly asks to use its local checkout. In another runtime, use its native isolation mechanism or the applicable worktree workflow.
- When only the umbrella project is saved, create a local task there and include the target repository's absolute path in its prompt. For write work, require that task to use `$git-worktree-flow` to create or reuse an inspected sibling worktree before editing. For read-only work, run commands from the nested repository's primary checkout without creating a worktree.
- For every visible task, explicitly pass a model and reasoning effort selected under the applicable outcome-value routing policy. In Codex, Sol, Terra, and Luna are all valid task routes; sibling tasks may differ. In another runtime, use its available equivalents. Never omit the values or copy the lead task's route by default.
- Give each task a cohesive prompt containing the outcome, constraints, verification, applicable safety gates, requested publication boundary, and stopping condition.
- Include a delegation contract in every execution-task prompt: pass the coordination key and require the task to use the subagent naming schema below. After reading applicable instructions and scoping the work, the task must reassess whether native subagents would materially improve cost, speed, context isolation, specialization, or independent verification. When the applicable policy supports delegation, the task should proactively spawn the minimum useful named subagents without waiting for another user request, then integrate and verify their results itself.
- For multi-repository work, keep worktrees, commits, and PRs separate. The lead owns cross-repository sequencing and integration.
- The execution task owns its subagent tree, implementation, integration, and verification. The lead task does not reach into that tree; it owns visible-task boundaries, model selection, sequencing, follow-ups, retries, and final status.

## Choose Tasks Or Subagents

- Use a visible task for a durable independent outcome, a separate repository or worktree, or work the user wants to inspect and continue directly from the sidebar.
- Use native subagents inside the owning task for bounded slices of the same investigation or implementation. Two `explorer` subagents are appropriate for independent service-profile analyses that the parent will synthesize. One `reviewer` subagent is appropriate after an implementation diff or PR exists when independent verification is the purpose.
- Give each native subagent a compact canonical task name when the runtime supports one: `<key>_<model>_<effort>_<role>_<slice>`, normalized to lowercase letters, digits, and underscores, for example `proj_123_terra_high_reviewer_retry_logic`. Also begin its delegation prompt with the human-readable label `[<key>] <model>-<effort> <role>: <slice>`. If the runtime supports renaming the agent thread, use that human-readable label as its title; otherwise the canonical task name and prompt label are the fallback.
- Keep all communication lead-mediated. A subagent returns findings to its parent task. A visible task does not message a peer directly; the coordinator reads its result and sends any necessary follow-up to the owning task.
- Do not create a peer task solely to review another task unless the user explicitly wants a separate user-owned review task. By default, tell the implementation task to invoke its own `reviewer`, address valid findings, rerun verification, and report the disposition.
- Use configured native roles by default. Do not invoke another agent runtime unless the user explicitly requests it and an installed integration supports the handoff. If no integration is installed, say so and use the configured native reviewer only with the user's agreement; never mislabel which agent performed a review.

## Follow Through

- Use task wait controls for normal progress. Poll only after a material state change or a bounded wait; do not repeatedly fetch unchanged status.
- Read a child task when its result, blocker, or requested decision needs inspection. Send a follow-up only when new evidence or a user decision changes the next action.
- When the user explicitly asks to babysit an external state such as PR approval, use a thread heartbeat when available. Keep it quiet while the state is unchanged and notify only on completion, failure, or required user action.
- Treat the user's stated stopping condition as authoritative. A PR being opened, checks passing, approval, merge, deployment, runtime behavior, and business outcome are separate states.

## Route Specialized Work

- Use the available worktree workflow for manual worktree creation or repair when the app-managed path is not the requested path.
- Use the available publishing workflow only when commit, push, or PR publication is authorized.
- Load repository-specific or domain-specific skills only when their own triggers apply.

## Authorization Boundaries

- Creating a task does not authorize commit, push, PR creation, marking ready, merge, apply, deployment, production mutation, or communication outside the current agent application. Follow the global and workspace authorization boundaries for every material transition.
