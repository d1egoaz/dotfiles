---
name: multi-agent-routing
description: Route Codex subagents and model choices for multi-agent work. Use when the user asks for multi-agent routing, subagent, delegation, parallel agents, agent team design, model-per-agent configuration, or `$multi-agent-routing`. Covers deciding what to keep local, what to delegate, which model and reasoning effort each agent uses, and how to integrate results safely across coding, repo review, PR repair, incident, and Terraform/IAM work.
---

# Multi-Agent Routing

## Overview

Act as the lead/conductor. Own scope, decomposition, model routing, risky decisions, integration, and the final answer.

Treat the phrase `Multi-Agent Routing`, `multi agent routing`, or `$multi-agent-routing` as an explicit request to consider subagents now. Use subagents only when the user explicitly asks for multi-agent, subagent, delegation, or parallel agent work, or when the active runtime instructions otherwise permit delegation. If subagents are unavailable, state that and apply the same routing judgment locally.

When the user names this skill and there is meaningful independent read-only or verification work, default to spawning at least one sidecar agent. Keep live mutations, destructive actions, and final decisions in the lead thread unless the user explicitly authorizes delegation of those writes.

## Model Tiers

Route by tier, not by raw model name, so a Codex lineup change is a one-line edit here. Confirm the current IDs in the Codex model picker. Reasoning effort runs Light < Medium < High < Extra High.

| Tier | Codex model (verify in the picker) | Use for |
|------|------------------------------------|---------|
| `strong` | GPT-5.5 | strategy, cross-repo or ambiguous changes, production/security/IAM/Terraform, root-cause and safety claims |
| `mid` | GPT-5.4 | scoped implementation, verification, scope audits |
| `fast` | GPT-5.4-Mini | read-only discovery, evidence extraction, drafting from verified facts |

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

3. Route by role and risk. Reference the model tiers above; raise reasoning effort as the cost of being wrong rises.
   - Lead/conductor: `strong`, High or Extra High. Keeps strategy, scope, critical path, risk calls, and synthesis.
   - Evidence scout: `fast`, Light or Medium. Read-only discovery, file mapping, log collection, docs lookup, PR metadata reads, and exact evidence extraction.
   - PR steward or scribe: `fast`, Light. Drafts PR bodies, Jira summaries, Slack wording, and changelog notes from verified facts. Use `mid`, Medium for incident-sensitive wording.
   - Implementation worker: `mid`, Medium or High. Scoped code changes in an assigned write set. Use `strong`, High for cross-repo, production-sensitive, security-sensitive, or ambiguous changes.
   - Verification agent: `mid`, High. Runs tests, reviews diffs, checks local behavior, and validates outputs. Use `strong`, High for production, IAM, Terraform, data-loss, or root-cause claims.
   - Scope auditor: `mid`, High. Checks blast radius, unrelated diffs, generated-artifact ownership, permissions widening, and repo-instruction compliance. Use `strong`, High for prod/security/infra/cross-team risk.
   - Terraform/IAM auditor: `strong`, High or Extra High. Reviews plan JSON, IAM action/resource/condition scope, TFE runs, state diffs, and safety claims.

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

Compose this skill with focused workflow skills when they apply. For example, use `$repo-research` for discovery, `$signed-pr-publish` for publishing, work-local follow-up skills when present, and `$codex-config-maintenance` for Codex config or skill changes.

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
