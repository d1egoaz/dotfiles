---
name: signed-pr-publish
description: Sign commits, push branches, and publish draft PRs. Use for commit, push, PR creation or updates, ready-for-review, attribution, PR bodies, `gh pr`, or `gh api`.
---

# Signed PR Publish

Use for commits or GitHub publication. Preserve unrelated work; perform only
authorized publication actions.

Read [`references/commands.md`](references/commands.md) when exact commit or
GitHub command templates are needed.

## Commit

1. Inspect status and diffs. Stage exact files; “commit staged files” adds
   nothing else. Identify generated-file ownership.
2. Run `$HOME/dotfiles/bin/files/codex-current-model`. Retry with scoped read
   access; stop if unknown. Never shorten or guess the result.
3. Use a Conventional Commit and this exact footer:

```text
Assisted-by: [Exact model identifier] via [Tool]
```

4. Use `git commit -S` with the profile OpenSSH key. Never bypass signing.
5. Verify signature, scope, and exact footer value before publication; regex
   presence alone is insufficient.

After activation, enroll once with `ssh-add --apple-use-keychain -t 86400
~/.ssh/codex-signing-${PROFILE}-ed25519`. Office repos stay on HTTPS through the
existing credential helper; do not replace or broaden it.

## Publish

- Recheck remote state and exact head before pushing.
- Push only when authorized. Open new PRs as drafts and use the repository
  template.
- Explain problem, change, validation, risk, and generated-artifact handling.
- Re-fetch and verify draft state, head, files, checks, and exact attribution.
- Use `gh api ... -X PATCH` for PR body updates; `gh pr edit --body` is
  deprecated.

Do not infer push, PR, ready-for-review, merge, deployment, or production
authority from another lifecycle step.
