---
name: git-worktree-flow
description: Create or repair scoped Git worktrees. Use for feature branches, new PR work, branching from main, multi-repo changes, or keeping the primary checkout clean.
---

# Git Worktree Flow

Keep the primary checkout on `main`; put feature writes in an isolated worktree.

1. Inspect the primary branch, dirt, worktrees, and target path. Preserve
   unrelated changes.
2. Fetch `origin/main` through any repository-required auth path.
3. Create a unique branch/worktree from `origin/main`.
4. Run Git there, not with `git -C`.

Use one worktree and publication lifecycle per repo. Inspect local and remote
state before repair. Never remove worktrees, delete branches, reset history, or
overwrite a path without explicit authorization.
