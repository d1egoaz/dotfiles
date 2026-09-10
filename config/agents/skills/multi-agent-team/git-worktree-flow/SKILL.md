---
name: git-worktree-flow
description: Create or repair scoped Git worktrees. Use for feature branches, new PR work, branching from main, multi-repo changes, or keeping the primary checkout clean.
---

# Git Worktree Flow

## Overview

Use this skill to keep the primary checkout on `main` and put feature work under
`~/.codex/worktrees`. Do not create worktrees beside the
primary repository.

## New Worktree

1. Start in the primary repo checkout.
2. Confirm current branch and dirty state. Do not overwrite unrelated changes.
3. Fetch `origin/main`.
   - If repo-local or parent instructions require a special network-auth path, follow that more specific guidance.
4. Create the worktree at
   `~/.codex/worktrees/<repo>-<feature-name>`.
   - Use the repository name and a filesystem-safe feature name so paths remain
     unique and recognizable.
   - If the destination already exists, inspect it instead of overwriting it or
     choosing a different location outside the worktree root.
5. Run subsequent git commands from the worktree's own working directory.

Pattern:

```bash
git fetch origin main
git worktree add ~/.codex/worktrees/repo-feature-branch -b feature-branch origin/main
```

## Multi-Repo Work

- Create one worktree per repo.
- Keep commits and PRs separate per repo.
- Run git commands in each repo's current working directory.
- Avoid `git -C` unless the user explicitly requests it.

## Repair And Cleanup

- If an existing branch is stale, inspect status and remote state before changing it.
- If the user says to clone or repair a branch locally, prefer an isolated worktree so the main checkout remains clean.
- Do not remove worktrees, delete branches, or reset history unless explicitly requested.
