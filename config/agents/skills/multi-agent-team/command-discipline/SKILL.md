---
name: command-discipline
description: Run safe, transparent, fish-compatible shell workflows. Use when the user asks to run commands, shell commands, fish, copy-paste-ready commands, command transparency, escalation, destructive-command safety, bulk refactors, shell rewrites with `sed`, JSON/YAML validation commands, or asks why commands are wrapped in `fish -lc`.
---

# Command Discipline

## Overview

Use this skill for command shape, command transparency, approval boundaries, and safe shell workflows. Keep routine updates concise, but make risky operations auditable before executing them.

## Command Rules

- Run shell-neutral commands directly. Use `fish -lc` only when the command relies on fish syntax or must be shown exactly as fish.
- Every user-facing command must be copy-paste ready as a single command line. Do not break commands across lines.
- Prefer CLI over GUI. Use GUI only when the task truly requires it.
- For multi-repo work, run commands in each repo's working directory. Avoid `git -C` unless the user explicitly requests it.
- Explain routine read-only commands with at most one short sentence.
- Use expanded risk explanation only for destructive operations, escalated permissions, broad rewrites, force pushes, live state changes, or when the user asks for it.

## Safety Gates

- Confirm before destructive changes such as removing files, resetting branches, force pushing, or changing live state.
- If a command fails because of sandboxing, rerun the same command with escalated permissions and a concise justification.
- Do not ask for broad persistent approvals. Scope approval prefix rules to the tool family actually needed.
- Never request a persistent approval rule for destructive commands.
- For manual code edits, use `apply_patch`. Use formatters or mechanical rewrites only when they are the safer, narrower tool for the job.

## Bulk Refactors

Use this sequence for broad text changes:

1. Verify scope with `rg` or `fd`.
2. Back up or make the rewrite mechanically reversible when the blast radius is broad.
3. Apply the replacement.
4. Review the diff.
5. Clean up backup files only after review.

Useful fish-compatible examples:

```fish
rg -n 'find_this' --hidden --glob '!.git/*'
rg -l 'find_this' | xargs sed -i.bak 's/find_this/replace_that/g'
git diff -- . ':!*.bak'
fd -e bak -0 | xargs -0 rm
```

## Validation Commands

Use structured validators when the format has one:

```fish
fd -e json -0 | xargs -0 -I{} sh -c 'jq . "{}" >/dev/null'
fd -e yaml -e yml -0 | xargs -0 -I{} sh -c 'yq "." "{}" >/dev/null'
```

If a command fails, report: what failed, the exact error, the likely root cause, the smallest fix, and the rerun result.
