---
name: scratch-log
description: Keep a local decision log for substantial work when the user explicitly asks for audit notes or a scratch log. Do not invoke automatically.
---

# Scratch Log

Use this skill only after an explicit request for a scratch log or audit notes.
The log is a private aid for the user and future agent turns, not a published
artifact. Skip it for routine work unless the user changes that instruction.

## Location

Use the current Git worktree root. Prefer `.codex/scratch-log.md` (or
`.claude/scratch-log.md` under Claude Code) only when that path is already
ignored. Check with `git check-ignore -q <path>` before writing. If it is not
ignored, or there is no worktree, use
`/private/tmp/codex-scratch-logs/<repo-or-dir>-<date>.md`; never alter a
repository's ignore rules just to store the log. If the preferred path is not
writable, use that temporary location and report it.

Do not stage, commit, quote in a PR, or include the log in a patch unless the
user explicitly asks. Mention its path in the final report.

## Entries

Keep entries brief and factual. Record the task context, meaningful decisions,
assumptions or gaps, review fixes, and validation checkpoints. Distinguish
evidence from inference. Do not record secrets, tokens, large outputs, or
private data. Append corrections when decisions change. Before finishing, skim
the log and report unresolved gaps accurately.

Use the structure in [`references/template.md`](references/template.md) when
the user requests a new log. Add metadata once, then entries only at real
decision, assumption, review, or validation boundaries.
