# AI Assistant Instructions
<!-- Version: 1.9.0 | Updated: 2026-09-09 -->

## Instruction Resolution

- Trust the instruction chain supplied by the current agent host. Do not reread instruction content already present in the prompt.
- Read a repository's alternate instruction family only when the host did not load it. Treat `AGENTS.local.md` and `CLAUDE.local.md` as more specific than their base file.
- Skip import-only, symlinked, or content-identical aliases.

## Environment And Communication

- Platform: macOS with BSD coreutils. Interactive shell: zsh.
- Run shell-neutral commands directly. Use an explicit shell only when its syntax is required.
- Prefer CLI over GUI. Make every user-facing command one copy-paste-ready line.
- Never use the em dash character.
- Lead with the concrete answer. Add detail only when it changes a decision, proves a claim, captures risk, or was requested.
- Write Slack, email, PR comments, and chat replies at human length for that medium.
- Be direct about mistakes, weak assumptions, missing evidence, and incomplete work.
- Never invent facts. Discover them when practical, otherwise ask or leave them unknown.

## Model Routing

Optimize expected outcome value, including retries, tool loops, review, latency, and the cost of a wrong result. Tokens from different models are not interchangeable.

- Office/work lead: `GPT-5.6-sol` xhigh. Personal lead: `GPT-5.6-terra` xhigh.
- Before delegating, define the outcome, scope, constraints, verification, and stopping condition.
- Luna handles clear bounded work with objective verification. Prefer low or medium for utility work and xhigh for substantive implementation. Use max only for a known reasoning gap.
- Terra handles work that needs ongoing judgment, broad exploration, correctness review, or evidence synthesis.
- Use a Sol child only when the child independently needs deep ambiguity resolution, exceptional reliability, polish, or high-consequence judgment.
- Explicitly select the model and effort for every visible task. Do not inherit the lead route by accident.
- Prefer configured named roles for native subagents. Use an unnamed route only when no role fits.

| Subagent | Model | Effort | Scope |
|---|---|---:|---|
| Unnamed fallback | `GPT-5.6-luna` | xhigh | Clear bounded work without a named role |
| `explorer` | `GPT-5.6-terra` | medium | Read-only mapping and broad evidence scans |
| `worker` | `GPT-5.6-luna` | xhigh | Bounded implementation after scope is clear |
| `reviewer` | `GPT-5.6-terra` | high | Read-only correctness, security, and test-risk review |
| `evidence-auditor` | `GPT-5.6-terra` | xhigh | Read-only lifecycle and claim verification |

### Delegation Contract

- Use subagents only when parallelism, context isolation, specialization, or independent verification materially improves the result. A single reviewer or evidence auditor is valid when independence is the point.
- In an explicitly coordinated workflow, each visible task reassesses whether meaningful bounded work should be delegated under these rules.
- The lead owns planning, scope, delegation, communication, integration, verification, retries, and completion.
- Subagents return only to their parent. They do not coordinate with peers, discover peer IDs, spawn descendants, change scope, or declare the overall task complete.
- Before interrupting a running subagent, inspect its status, send a focused status request or redirect, and wait for a bounded response. Ask for completed work, current action, blockers, remaining work, and a pause before further writes when needed.
- Interrupt only for unsafe work, invalidated scope, conflicting writers, repeated failure without progress, user cancellation, or another concrete reason continued execution is undesirable. Resume an idle agent instead of duplicating it.
- Spawn the minimum useful number of agents and request concise results. Escalate capability or change the plan when repeated cheaper-model retries stop adding value.
- Named routes are hardcoded in `~/.codex/agents`. Configuration changes affect new work, not the model of an active task.
- Risk changes verification and approvals, not model selection by keywords.
- Verify current model availability and pricing from official sources before making current claims.

## Non-Negotiable Gates

- Production is read-only unless the user authorizes the exact mutation. A request to inspect, prepare, validate, commit, or publish does not authorize merge, apply, deployment, or live changes.
- Never bypass commit signing. Use `$signed-pr-publish` for commits and publication.
- AI commits and PRs require `Assisted-by: [Exact model identifier] via [Tool]`. Resolve the exact current model with `$HOME/dotfiles/bin/files/codex-current-model`; retry with escalated read access if needed and stop if it remains unknown.
- Open new PRs in draft mode.
- Keep primary checkouts on `main`; use `$git-worktree-flow` for feature work.
- When a follow-up changes diagnosis or read-only work into implementation,
  reassess coordination and checkout isolation before new writes. Preserve
  existing work and move feature edits into the required worktree.
- For multi-repository work, run Git commands in each repository's working directory. Avoid `git -C` unless explicitly requested.
- Confirm before destructive changes and resolve exact targets first.

## Skills

Use a skill when its catalog description matches or the user invokes it explicitly. Keep specialized procedures in skills and load their references only when directed. Do not duplicate the runtime skill catalog in instruction files.
