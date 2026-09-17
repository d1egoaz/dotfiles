---
name: signed-pr-publish
description: Sign commits and publish GitHub changes with exact AI attribution. Use for committing, pushing, creating or updating PRs, or marking ready for review; skip read-only PR status and API queries.
---

# Signed PR Publish

Use for commits/publication; preserve unrelated work and authorization.
Once the expected change, checked diff, and objective checks are clear, assign
bounded implementation and the authorized publication workflow to Luna xhigh
`worker`, including small one-file changes. Reuse a capable owner; simple
changes still follow this route.

Read [`references/commands.md`](references/commands.md) for templates/handoff.

## Commit

1. Inspect status/diffs; stage exact files; “commit staged files” adds nothing
   else; identify generated files.
2. Run `$HOME/dotfiles/bin/files/codex-current-model`; retry with scoped read
   access; stop if unknown. Never shorten or guess. The actor uses its own
   current model for new attribution, retains prior attribution, and never
   invents switching.
3. Use a Conventional Commit with this footer:

```text
Assisted-by: [Exact model identifier] via [Tool]
```

4. Use `git commit -S` with the profile OpenSSH key. Never bypass signing.
5. Verify signature, scope, and exact footer before publication; regex alone is
   insufficient.

Office repos stay on HTTPS; keep the credential helper unchanged.

## Publish

- Recheck remote state/exact head before pushing.
- Push only when authorized. Open new PRs as drafts; use the template.
- Explain problem, change, validation, risk, and artifacts.
- Re-fetch and verify draft state, head, files, checks, and attribution.

Do not infer push, PR, ready-for-review, merge, deployment, or production
authority from another lifecycle step.
