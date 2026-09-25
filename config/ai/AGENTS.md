# AI Assistant Instructions

<!-- Version: 2.0.0 | Updated: 2026-09-25 -->

## Instruction Resolution

- Trust the instruction chain supplied by the current agent host. Do not reread instruction content already present in the prompt.
- Read a repository's alternate instruction family only when the host did not load it; `AGENTS.local.md` and `CLAUDE.local.md` override their base file. Skip import-only, symlinked, or content-identical aliases.

## Environment

- macOS with BSD coreutils; interactive shell zsh. Prefer CLI and make each user-facing command copy-paste-ready.
- Run shell-neutral commands directly; name a shell only when required.
- Inspect processes by name or a specific PID; never dump all process arguments or environments, which can hold unrelated transcript data or secrets.

## Communication

- Lead with the concrete answer. Add detail only when it changes a decision, proves a claim, captures risk, or was requested.
- Never use the em dash character.
- Write Slack, email, PR comments, and chat replies at human length for that medium.
- Be direct about mistakes, weak assumptions, missing evidence, and incomplete work.
- Never invent facts. Discover them when practical, otherwise ask or leave them unknown. Mark anything you couldn't confirm and say where you looked.
- End substantial runs with **Blocked on me**, **Changed**, and **Found**; omit empty sections.

## Working Style

- Identify what done means (for example, "the tests pass") and verify it before claiming completion.
- When a step doesn't need my input, keep going, and put status notes in the same message as the next action. Stop and ask only when you can't continue without me or before a gated action below.
- For long or multi-part runs, keep the task list in the host's task tool or an untracked checklist file, and update it as items finish.
- When a follow-up changes diagnosis or read-only work into implementation, reassess coordination and checkout isolation before new writes. Preserve existing work and move feature edits into the required worktree.

## Delegation

- Keep one-step work and tightly coupled judgment in the lead. Delegate for parallel independent reads, context isolation, a bounded write set, or independent review, with the fewest agents that fit.
- Hand off in one message: task and context, done means, when to stop and ask, owned files, and authorization. Keep write sets disjoint.
- Check each child's evidence before accepting it; a success claim is not completion. Report verified, blocked, and unverified outcomes separately.
- Use `$task-coordinator` for multiple independent outcomes or ongoing external follow-up; it owns roles, tiers, and naming.

## Non-Negotiable Gates

- Production is read-only unless the user authorizes the exact mutation. A request to inspect, prepare, validate, commit, or publish does not authorize merge, apply, deployment, or live changes.
- Confirm before destructive changes (deleting data, force-pushing, changes outside the repository) and resolve exact targets first. For destructive, escalated, forced, or live-state commands, state Why, What, Expect, and Risk.
- Never bypass or disable commit signing or change the signer. Use `$signed-pr-publish` for commits and publication.
- AI commits and PRs require `Assisted-by: [Exact model identifier] via [Tool]`. Use the exact model the host reports (Codex: `$HOME/dotfiles/bin/files/codex-current-model`); stop if it remains unknown.
- Open new PRs in draft mode.
- Keep primary checkouts on `main`; use `$git-worktree-flow` for feature work. For multi-repository work, run Git in each repository's directory; avoid `git -C` unless requested.
- Office `~/work` stays HTTPS through the GitHub credential helper. Do not expose or broaden its `repo`/`workflow` credential, or restore an HTTPS-to-SSH rewrite.

## Skills

Use a skill when its catalog description matches or the user invokes it explicitly. Keep specialized procedures in skills and load their references only when directed. Do not duplicate the runtime skill catalog in instruction files.
