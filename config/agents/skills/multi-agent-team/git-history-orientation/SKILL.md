---
name: git-history-orientation
description: Orient on an unfamiliar repository using read-only git history signals before reading code. Use when asked to understand a codebase, inspect churn hotspots, identify likely ownership or bus-factor risk, compare active versus historical contributors, summarize repo momentum, or decide which files to read first from git history.
---

# Git History Orientation

## Overview

Use this skill for a quick, read-only map of a repository before deeper code inspection. Treat the output as directional evidence, not proof of quality, team health, or root cause.

This workflow is adapted from Ally Piechowski's "The Git Commands I Run Before Reading Any Code": https://piechowski.io/post/git-commands-before-reading-code/

## Scope

- Run commands from the repo or the most relevant subtree, such as `src`, `app`, `nix`, `packages/<name>`, or the service directory in a monorepo.
- Prefer path-limited history over repo-root noise when lockfiles, generated files, vendored code, or changelogs dominate.
- Keep this workflow read-only. Do not fetch, reset, clean, rebase, or rewrite history.
- Do not run bug-keyword cluster checks or firefighting grep checks by default. Those are intentionally excluded from this skill unless the user explicitly asks for them.

## Preflight

Confirm the repository and history depth:

```bash
git rev-parse --show-toplevel
git rev-parse --is-shallow-repository
git status --short
```

If the repository is shallow, say the history signals are incomplete. If the worktree is dirty, ignore local changes unless the user asked for change review.

## Churn Hotspots

Run from the most relevant subtree when possible:

```bash
git log --format=format: --name-only --since="1 year ago" -- . | sed '/^$/d' | sort | uniq -c | sort -nr | head -20
```

Interpretation:

- High churn means "read this first", not "this is bad".
- Discount generated files, lockfiles, vendored code, snapshots, changelogs, and migration dumps unless they are the actual target.
- For monorepos, rerun inside the service or package that matches the user's question.
- A high-churn file with vague ownership or broad call sites is a stronger risk signal than churn alone.

## Contributor Concentration

Compare all-time contribution concentration with recent activity:

```bash
git shortlog -sn --no-merges
git shortlog -sn --no-merges --since="6 months ago"
```

Interpretation:

- If one contributor dominates all-time history but is absent recently, flag a possible ownership or continuity gap.
- If many historical contributors collapse to a small recent set, call out that maintainers and original builders may differ.
- Ask or verify the merge strategy before making strong claims. Squash-heavy workflows can make `shortlog` reflect commit authors, PR squash authors, or merge operators more than implementation authors.

## Monthly Cadence

Show month-by-month commit volume:

```bash
git log --format='%ad' --date=format:'%Y-%m' | sort | uniq -c
```

Use path-limited history for a specific subsystem:

```bash
git log --format='%ad' --date=format:'%Y-%m' -- . | sort | uniq -c
```

Interpretation:

- Look for shape, not a single number: steady cadence, long decline, recent drop, or release batching.
- Do not equate lower commit count with lower productivity without corroborating evidence.
- Cross-check with release strategy, PR workflow, squashing, generated commits, and repository splits before drawing team conclusions.

## Synthesis

Return a compact orientation:

```text
Churn: <top files or directories, with generated/noise caveats>
Ownership: <all-time versus recent contributor concentration, with merge-strategy caveat>
Cadence: <monthly trend shape and whether it is path-limited>
Read first: <2-5 files/directories worth opening next>
Confidence: <what history can and cannot prove from this repo>
```

When the user asks for next steps, use the churn and ownership signals to choose specific files or modules to inspect with `$repo-research`.
