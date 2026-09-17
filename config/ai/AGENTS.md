# AI Assistant Instructions
<!-- Version: 1.10.0 | Updated: 2026-09-16 -->

## Instruction Resolution

- Trust the instruction chain supplied by the current agent host. Do not reread instruction content already present in the prompt.
- Read a repository's alternate instruction family only when the host did not load it. Treat `AGENTS.local.md` and `CLAUDE.local.md` as more specific than their base file.
- Skip import-only, symlinked, or content-identical aliases.

## Environment And Communication

- Platform: macOS with BSD coreutils. Interactive shell: zsh.
- Run shell-neutral commands directly; use an explicit shell only when required.
- Prefer CLI; make each user-facing command copy-paste-ready.
- Never use the em dash character.
- Lead with the concrete answer. Add detail only when it changes a decision, proves a claim, captures risk, or was requested.
- Write Slack, email, PR comments, and chat replies at human length for that medium.
- Be direct about mistakes, weak assumptions, missing evidence, and incomplete work.
- Never invent facts. Discover them when practical, otherwise ask or leave them unknown.

## Model Routing

Use one strong lead: office/work `gpt-5.6-sol` xhigh; personal `gpt-5.6-terra` xhigh. Keep one-step work and tightly coupled judgment with the lead. Once scope, expected change, and objective checks are clear, route bounded implementation and authorized publication to the Luna xhigh `worker`, including small one-file changes; keep coupled files in one worker.

Choose the cheapest capable model and effort per delegated outcome, counting retries, review, latency, and wrong-result cost. Luna fits clear checked work; Terra fits exploration, synthesis, and correctness review; Sol fits deep ambiguity remaining after decomposition. Risk alone does not select a premium route.

These are defaults, not mandatory routes. Use a named role only when its model, effort, and permissions fit; otherwise select an explicit route with the role's boundaries. Never silently inherit the lead model. If the host cannot select the route, keep the work in the lead or explain the alternative.

| Subagent | Model | Effort | Scope |
|---|---|---:|---|
| Unnamed fallback | `gpt-5.6-luna` | xhigh | Clear bounded work without a named role |
| `utility` | `gpt-5.6-luna` | medium | Read-only bounded utility work with objective verification |
| `explorer` | `gpt-5.6-terra` | medium | Read-only mapping and broad evidence scans |
| `worker` | `gpt-5.6-luna` | xhigh | Bounded implementation and Git/index mutation after scope is clear |
| `reviewer` | `gpt-5.6-terra` | high | Read-only correctness, security, and test-risk review |
| `evidence-auditor` | `gpt-5.6-terra` | xhigh | Read-only lifecycle and claim verification |

### Delegation Contract

- Name every native subagent `<key>_<model-label>_<effort-code>_<role>_<slice>`, even outside `$task-coordinator`. Use lowercase model labels and effort codes: `n`=none, `min`=minimal, `lo`=low, `med`=medium, `hi`=high, `xh`=xhigh, `max`=max, `ult`=ultra.
- Set the host's spawn name field (e.g. `task_name`) from the selected runtime model/effort, e.g. `review_terra_hi_reviewer_auth`. A role name or prompt label alone is insufficient. Pass this naming rule to visible children that delegate.
- The lead owns decomposition, sequencing, user updates, integration, verification, recovery, and overall completion. Use `$task-coordinator` for independent outcomes or ongoing external-state follow-up.
- Visible child tasks own independently verifiable outcomes. They may use bounded native subagents and ask the user directly when approval is needed. The lead tracks the blocker without proxying approval.
- Native subagents own bounded slices and return results to their parent. They do not coordinate with peers, discover peer IDs, spawn descendants, change scope, or declare overall completion. If approval is needed, report the blocked action to the parent; do not assume a direct user channel exists.
- Delegate when cost, parallelism, context isolation, specialization, or independent verification justifies it. Use the minimum useful number; an independent reviewer alone is valid. Reuse a capable Luna owner, otherwise hand off once at implementation or publication boundary.
- Give each child its outcome, owned files/resources, constraints, dependencies, verification, authorization, and stopping condition. Keep write sets disjoint; load only relevant skills.
- Explicitly select model and effort, briefly explain the route, and reassess material follow-ups. Escalate for a demonstrated capability gap; missing access or approval needs resolution, not a stronger model. Configuration edits affect new agents, not active ones.
- Reuse idle agents when their route still fits. Request status or redirect before interrupting; stop immediately for unsafe work or conflicting writes.
- Check returned evidence before completion; distinguish verified, blocked, and unverified outcomes. Task creation and a child's success claim are not completion.
- Treat tiers as local routing policy; verify availability and pricing before claims.

## Non-Negotiable Gates

- Production is read-only unless the user authorizes the exact mutation. A request to inspect, prepare, validate, commit, or publish does not authorize merge, apply, deployment, or live changes.
- Never bypass commit signing. Use `$signed-pr-publish` for commits and publication.
- Commits use the active profile's standard OpenSSH key. After activation,
  run `ssh-add --apple-use-keychain -t 86400
  ~/.ssh/codex-signing-${PROFILE}-ed25519` once; macOS restores all
  Keychain-backed SSH keys at login. The machine signature is machine-key
  provenance, not per-commit human review; retain the exact-model `Assisted-by`
  footer. Never disable signing or change signer.
- Office `~/work` stays HTTPS through the GitHub credential helper. Do not
  expose or broaden its `repo`/`workflow` credential, or restore an
  HTTPS-to-SSH rewrite.
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
