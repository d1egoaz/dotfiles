---
name: multi-agent-routing
description: Route Codex subagents and model choices for multi-agent work. Use when the user asks for Multi-Agent Routing, multi agent routing, multi-agent, subagent, delegation, parallel agents, agent team design, model-per-agent configuration, or explicit `$multi-agent-routing` help. Applies to coding, repo review, PR repair, incident investigation, Terraform/IAM review, Slack/Jira follow-up, and other workflows where a lead agent must decide what to keep local, what to delegate, which model/reasoning effort each agent should use, and how to integrate results safely.
---

# Multi-Agent Routing

## Overview

Act as the lead/conductor. Own scope, decomposition, model routing, risky decisions, integration, and the final answer.

Treat the phrase `Multi-Agent Routing`, `multi agent routing`, or `$multi-agent-routing` as an explicit request to consider subagents now. Use subagents only when the user explicitly asks for multi-agent, subagent, delegation, or parallel agent work, or when the active runtime instructions otherwise permit delegation. If subagents are unavailable, state that and apply the same routing judgment locally.

When the user names this skill and there is meaningful independent read-only or verification work, default to spawning at least one sidecar agent. Keep live mutations, destructive actions, and final decisions in the lead thread unless the user explicitly authorizes delegation of those writes.

## Workflow

1. Diagnose the work.
   - State the objective, repo or artifact scope, and known constraints.
   - Identify the immediate critical-path task the lead should do locally.
   - Identify independent sidecar tasks that can run in parallel without blocking the next local step.
   - Classify risk: low-risk read-only, routine write, high-risk production/security/IAM/Terraform/data-loss, or ambiguous cross-repo work.

2. Decide whether to delegate.
   - Delegate bounded, independent sidecar tasks that materially advance the work.
   - If this skill was named explicitly and a read-only scout or verifier can help, spawn one instead of keeping the whole task local.
   - Keep immediate blockers local when the lead's next step depends on the result.
   - Keep tightly coupled, tiny, or unclear tasks local.
   - Do not delegate live mutations or destructive actions unless the user explicitly authorized that risk.

3. Route by role and risk.
   - Lead/conductor: `gpt-5.5`, `xhigh`. Keeps strategy, scope, critical path, risk calls, and synthesis.
   - Evidence scout: `gpt-5.4-mini`, `low` or `medium`. Performs read-only discovery, file mapping, log collection, docs lookup, PR metadata reads, and exact evidence extraction.
   - PR steward or scribe: `gpt-5.4-mini`, `low`. Drafts PR bodies, Jira summaries, Slack wording, and changelog notes from verified facts. Use `gpt-5.4`, `medium` for incident-sensitive wording.
   - Implementation worker: `gpt-5.4`, `medium` or `high`. Makes scoped code changes in an assigned write set. Use `gpt-5.5`, `high` for cross-repo, production-sensitive, security-sensitive, or ambiguous changes.
   - Verification agent: `gpt-5.4`, `high`. Runs tests, reviews diffs, checks local behavior, and validates outputs. Use `gpt-5.5`, `high` for production, IAM, Terraform, data-loss, or root-cause claims.
   - Scope auditor: `gpt-5.4`, `high`. Checks blast radius, unrelated diffs, generated artifact ownership, permissions widening, and repo instruction compliance. Use `gpt-5.5`, `high` for prod/security/infra/cross-team risk.
   - Terraform/IAM auditor: `gpt-5.5`, `high` or `xhigh`. Reviews plan JSON, IAM action/resource/condition scope, TFE runs, state diffs, and safety claims.

4. Write precise delegation prompts.
   - State the role, objective, allowed files or read scope, and exact output needed.
   - For mutating workers, assign a disjoint write set and say they are not alone in the codebase.
   - Instruct mutating workers not to revert or overwrite edits made by others.
   - Ask agents to list changed files, commands run, evidence found, and unresolved risks.
   - Include model and reasoning effort only when the runtime supports overrides.

5. Integrate.
   - Review worker changes before trusting them.
   - Reconcile findings with local context and user instructions.
   - Run verification from the lead thread when practical.
   - Finalize with the result, validation performed, and any remaining risk.

## Delegation Prompt Template

```text
Role: <evidence scout | implementation worker | verifier | scope auditor | scribe>
Model: <model> with <reasoning effort>, because <risk/cost reason>
Task: <bounded objective>
Scope: <read scope or disjoint write set>
Constraints: You are not alone in the codebase. Do not revert edits made by others. Preserve unrelated changes.
Output: <exact evidence, changed files, tests, risks, or draft needed>
```

## Practical Patterns

Use two or three agents for broad work:
- Lead: continue local critical-path reading or editing.
- Scout: map relevant files, logs, or PR facts.
- Verifier/auditor: check blast radius or tests while implementation proceeds.

Compose this skill with focused workflow skills when they apply. For example, use `$repo-research` for discovery, `$signed-pr-publish` for publishing, `$chime-pr-followup` for Chime PR branches, and `$codex-config-maintenance` for Codex config or skill changes.

Use separate write sets for parallel implementation:
- Worker A owns one module or repo.
- Worker B owns a different module or repo.
- Lead integrates and verifies.

Use stronger models where being wrong is expensive:
- Terraform plans, IAM policy conditions, production incident scope, data mutation, migration safety, security boundaries, and cross-team blast radius.

Use faster models where the task is bounded and easily checked:
- Searching files, listing changed paths, extracting review comments, summarizing verified facts, drafting copy from provided evidence, or collecting command output.

## Guardrails

- Cheap agents collect and draft. They are not final authority for correctness.
- The lead owns the final claim and must not paste unverified agent output.
- Prefer no delegation over poor delegation when the task is small, serial, or risky.
- Do not let agents duplicate the same unresolved work unless independent confirmation is the goal.
- Stop and ask the user before live destructive changes, broad forceful git operations, or production mutation.
