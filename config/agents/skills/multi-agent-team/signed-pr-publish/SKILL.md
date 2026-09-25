---
name: signed-pr-publish
description: Sign commits and publish GitHub changes with exact AI attribution. Use for committing, pushing, creating or updating PRs, or marking ready for review; skip read-only PR status and API queries.
---

# Signed PR Publish

Use for authorized commits/publication; preserve unrelated work.
Assign `@spawn-subagent-implement` once scope, checked diff, and objective
checks are clear. Include `<model-label>` after the selected tier in native and
visible task names; keep the exact model ID in runtime fields.

Read [`references/commands.md`](references/commands.md) for templates/handoff.

## Commit

1. Inspect status/diffs; stage exact files; “commit staged files” adds nothing
   else; identify generated files.
2. Codex: run `$HOME/dotfiles/bin/files/codex-current-model` (retry with
   scoped read access); other hosts: use the host-reported ID. Stop if unknown. Never shorten or guess. The actor uses its own
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
