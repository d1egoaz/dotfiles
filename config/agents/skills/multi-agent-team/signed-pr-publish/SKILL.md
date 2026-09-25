---
name: signed-pr-publish
description: Sign commits and publish GitHub changes with exact AI attribution. Use for committing, pushing, creating or updating PRs, or marking ready for review; skip read-only PR status and API queries.
---

# Signed PR Publish

Use for authorized commits and publication; preserve unrelated work. Commands
and templates: [`references/commands.md`](references/commands.md).

## Commit

1. Inspect status and diffs; stage exact files only. "Commit staged files"
   adds nothing else. Note generated files.
2. Resolve the exact model: in Codex run
   `$HOME/dotfiles/bin/files/codex-current-model`; elsewhere use the ID the
   host reports. Stop if unknown; never shorten or guess.
3. Write a Conventional Commit ending with
   `Assisted-by: [Exact model identifier] via [Tool]` for the model making
   this commit. Keep earlier footers unchanged.
4. Sign with `git commit -S`; never bypass signing or change the signer. If it
   asks for a passphrase, follow "Signing fails" in the reference.
5. Before publishing, confirm `git verify-commit HEAD` reports a good
   signature, the commit holds only the intended files, and the footer names
   the exact model.

Office repos stay on HTTPS; keep the credential helper unchanged.

## Publish

- Re-fetch and confirm the base and exact head before pushing.
- Push only when authorized, naming the branch: `git push -u origin <branch>`.
- Open new PRs as drafts. Body: problem, change, validation, risk, follow-up,
  and the `Assisted-by` footer.
- Re-fetch and verify draft state, head, files, checks, and footer.

Do not infer push, PR, ready-for-review, merge, deployment, or production
authority from another lifecycle step.
