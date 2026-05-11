---
name: git-worktree-flow
description: Create and manage scoped git worktrees for feature work. Use when asked for a new branch, worktree, feature branch, new PR, branch from main, origin/main, multi-repo work, keeping the main checkout clean, or repairing a stale feature checkout.
---

# Git Worktree Flow

## Overview

Use this skill to keep the primary checkout on `main` and put feature work in sibling worktrees.

## New Worktree

1. Start in the primary repo checkout.
2. Confirm current branch and dirty state. Do not overwrite unrelated changes.
3. Fetch `origin/main`.
4. Create a sibling worktree using `<repo>-worktrees/<feature-name>`.
5. Run subsequent git commands from the worktree's own working directory.

Fish-compatible pattern:

```fish
git fetch origin main
git worktree add ../(basename $PWD)-worktrees/feature-branch -b feature-branch origin/main
```

## Multi-Repo Work

- Create one worktree per repo.
- Keep commits and PRs separate per repo.
- Run git commands in each repo's current working directory.
- Avoid `git -C` unless the user explicitly requests it.

Known Chime worktree roots under `/Users/diego.alvarez/work/github.com/1debit`:
- `chime-tf-worktrees`
- `chime-cd-worktrees`
- `infra-claude-skills-worktrees`
- `infra-service-worktrees`
- `server-router-worktrees`
- `archimedes-worktrees`
- `chime-atlas-worktrees`
- `mergebot-worktrees`

## Repair And Cleanup

- If an existing branch is stale, inspect status and remote state before changing it.
- If the user says to clone or repair a branch locally, prefer an isolated worktree so the main checkout remains clean.
- Do not remove worktrees, delete branches, or reset history unless explicitly requested.
