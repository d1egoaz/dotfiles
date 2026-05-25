---
name: scratch-log
description: Maintain a local scratch log for large refactors, multi-file implementation work, ambiguous requirements, long-running debugging, review-feedback fixes, or tradeoff-heavy changes. Use when Codex should record decisions, assumptions, alternatives considered, review fixes, validation checkpoints, or user-specification gaps while working so later turns can audit what happened.
---

# Scratch Log

## Purpose

Keep a short local record of important decisions made during substantial work. The log is for the user and future agent turns, not for reviewers unless the user asks to publish it.

## Start Criteria

Start or resume a scratch log early when the task is likely to include any of:

- multi-file, multi-module, or multi-repo edits
- refactors, migrations, or architectural reshaping
- ambiguous requirements that require assumptions
- review feedback, CI fixes, or bot feedback across turns
- production or infrastructure changes where evidence and tradeoffs matter
- more than one meaningful implementation approach

Skip the log for simple questions, one-line fixes, read-only lookups, and tasks where the user explicitly says not to create files. If a task grows beyond its original scope, start the log then.

## Location

Use the current git worktree root when available; otherwise use the current directory.

Preferred path: `.codex/scratch-log.md`

Before writing, check whether the log already exists and append to it. If `.codex/` does not exist, create the directory, then use `apply_patch` to create or update the markdown file.

Do not stage, commit, quote in PR bodies, or include the scratch log in patches unless the user explicitly asks. Mention the path in the final answer when the log was created or updated.

If the preferred path is not writable, use `/private/tmp/codex-scratch-logs/<repo-or-dir>-<date>.md` and mention that fallback path in the final answer.

## Entry Rules

- Keep entries brief and factual.
- Add a new entry at task start, after each meaningful decision or tradeoff, when review feedback changes the plan, and after validation.
- Record unresolved questions and assumptions when they are made.
- Distinguish decisions, evidence, assumptions, and speculation.
- Do not paste secrets, tokens, large command output, full stack traces, or private data dumps. Use command names, paths, URLs, and concise summaries.
- If a decision changes, append a correction instead of rewriting the history.
- Before the final answer, skim the log and use it to report decisions, remaining gaps, and validation accurately.

## Template

```markdown
# Scratch Log

Task: <short user goal>
Started: <YYYY-MM-DD HH:MM local>
Worktree: <absolute path>
Branch: <branch or none>
Status: active

## Context
- <known user request, constraints, and relevant repo rules>

## Decisions
- <timestamp> Decision: <choice>. Why: <reason>. Alternatives: <options considered>. Risk: <tradeoff or unknown>.

## Assumptions And Gaps
- <timestamp> <assumption or missing requirement>. Action: <verified, asked, or left unresolved>.

## Review And CI Fixes
- <timestamp> <feedback or check>. Fix: <change>. Evidence: <command, path, or URL>.

## Validation
- <timestamp> <command or check>. Result: <pass, fail, or blocked>. Notes: <important output only>.

## Final Notes
- <what a future agent or user should know before continuing>
```

## Cadence

Use the log as a private work aid. It should not slow down routine edits:

1. Create or update metadata once at the start.
2. Add bullets only when a real decision, assumption, review fix, or validation result occurs.
3. Add final notes only when they would help resume or audit the work later.
