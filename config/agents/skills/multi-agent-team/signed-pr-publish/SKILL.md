---
name: signed-pr-publish
description: Commit signed changes, push branches, and open or update draft PRs. Use when asked to commit, push, publish, create PR, open PR, draft PR, ready for review, amend, fix attribution, signed commit, Assisted-by footer, PR body, `gh pr`, or `gh api`.
---

# Signed PR Publish

## Overview

Use this skill to publish local work without losing scope control, signing, attribution, or PR-body quality.

## Scope Check

1. Inspect `git status --short`.
2. Inspect staged and unstaged diffs.
3. Separate intended changes from unrelated local changes.
4. If the user says `commit staged files`, commit only staged files.
5. If generated artifacts are involved, identify the source file that owns them.

## Commit

- Use Conventional Commits:

```text
<type>[scope]: <description>

[body]

Assisted-by: [Exact model identifier] via [Tool]
```

- Types: `fix`, `feat`, `build`, `chore`, `ci`, `docs`, `style`, `refactor`, `perf`, `test`, `revert`. Breaking changes: append `!` after the type or add a `BREAKING CHANGE:` footer.
- Before attribution, run `$HOME/dotfiles/bin/files/codex-current-model`; it resolves `CODEX_THREAD_ID` to the latest metadata-only `turn_context` for the active task. Use its exact model output, including version and variant, without shortening or normalizing it. Examples: `gpt-5.6-sol`, `gpt-5.4`.
- Never use a family-only label such as `GPT-5`. If the resolver cannot read session metadata, rerun it with escalated permissions; stop and ask the user only if it still cannot resolve an exact identifier.
- Never bypass commit signing; never use signing-bypass flags such as `-c commit.gpgsign=false`.
- Run `git commit -S` with escalated permissions first when signing may need the 1Password SSH agent.
- If a signing command fails with socket or agent errors, rerun the same command with escalated permissions immediately.
- If signing still fails after escalation, stop and ask the user.
- Verify attribution after commit:

```fish
git log -1 --pretty=%B | rg -n "^Assisted-by: .+ via .+$"
```

The regex only confirms that the footer exists. Compare the captured model value with the exact identifier exposed by the current runtime or session metadata. Do not report the footer as verified if the value is shortened, family-only, guessed, or mismatched; amend the commit before pushing.

## Push And PR

1. Push the current branch.
   - If repo-local or parent instructions require a special network-auth path, follow that more specific guidance.
2. Open new PRs in draft mode.
3. Use the repo PR template when present.
4. PR bodies must explain the problem, chosen fix, validation, and real risk. Include rollback or follow-up only when there is an actual unresolved follow-up or meaningful rollback decision.
5. For generated artifacts, state the source file and whether generated output was reviewed or left to CI.
6. For incident or live production fixes, include the live evidence and validation command.

Use `gh api` for PR body updates because `gh pr edit --body` is deprecated:

```fish
gh api repos/OWNER/REPO/pulls/NUMBER -X PATCH -f body="..."
```

Before marking ready, verify attribution in the PR body:

```fish
gh pr view --json body --jq ".body" | rg -n "^Assisted-by: .+ via .+$"
```

Apply the same exact-model comparison to the PR body. Do not mark a PR ready if commit or PR attribution is missing, shortened, family-only, guessed, or mismatched.
