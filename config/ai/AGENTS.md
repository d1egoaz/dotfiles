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

Use one strong lead selected by the active Codex runtime in
`nix/data/agent-routing.toml`. Office Codex uses the frontier lead at xhigh;
personal Codex uses the frontier lead at high.
Keep one-step work and tightly coupled judgment with the lead. Once scope,
expected change and objective checks are clear, route bounded implementation
and authorized publication through the economy `worker` route.

Choose the cheapest capable capability tier and effort per delegated outcome,
counting retries, review, latency, and wrong-result cost. `economy` fits clear
checked work; `balanced` fits exploration, synthesis, and correctness review;
`frontier` fits deep ambiguity remaining after decomposition. Risk alone does
not select a premium route.

Resolve the tier through the active Codex runtime before every delegation. Keep
model label and requested/effective effort separate; put tier and model label
in task titles and route explanations. Host controls resolve the exact ID.

These are defaults, not mandatory routes. Use a named role when its tier,
model, effort, and capabilities fit; otherwise use its explicit boundaries.
Never silently inherit the lead model. If the host cannot select a route, keep
the work in the lead or explain the alternative.

Specialized native launchers are `@spawn-subagent-economy-check`,
`@spawn-subagent-economy-implement`, `@spawn-subagent-balanced-explore`,
`@spawn-subagent-balanced-review`, and `@spawn-subagent-balanced-audit`; their
tier, effort, and sandbox contracts are defined in the routing registry.

### Delegation Contract

- Use `@spawn-subagent-economy`, `@spawn-subagent-balanced`, or `@spawn-subagent-frontier` for a generic native subagent whose outcome and constraints come entirely from the parent handoff. Use the specialized `@spawn-subagent-*` launchers above when a fixed role contract is appropriate.
- Invoke these launchers only through native subagent spawning, never visible child-task creation. The old role names (`utility`, `worker`, `explorer`, `reviewer`, `evidence-auditor`) remain compatibility aliases; new coordinator instructions use canonical launchers.
- Name every spawned instance `<key>_<tier>_<model-label>_<effort-code>_<role>_<slice>`. Set the host spawn name from the selected tier, runtime-resolved model label, and effort; a role name alone is insufficient. Pass this rule to children that delegate.
- The lead owns decomposition, sequencing, user updates, integration, verification, recovery, and overall completion. Use `$task-coordinator` for independent outcomes or ongoing external-state follow-up.
- Visible child tasks own independently verifiable outcomes, may use bounded native subagents, and ask the user directly when approval is needed. The lead tracks blockers without proxying approval.
- Native subagents return results or blockers to their parent. They do not message peers, discover peer IDs, spawn descendants, change scope, or declare overall completion; report approval blockers to the parent.
- Delegate when cost, parallelism, isolation, specialization, or independent verification justifies it; use the minimum useful number. Reuse an economy owner, otherwise hand off at implementation/publication.
- Give each child its outcome, owned files/resources, constraints, dependencies, verification, authorization, and stopping condition. Keep write sets disjoint.
- Select tier and effort, explain route, and reassess follow-ups. Escalate only for a capability gap; missing access or approval needs resolution. Configuration edits affect new agents, not active ones.
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
