---
name: git-worktree-flow
description: Create or repair scoped Git worktrees. Use for feature branches, new PR work, branching from main, multi-repo changes, or keeping the primary checkout clean.
---

# Git Worktree Flow

Keep the primary checkout on `main`. Put feature writes at
`~/.codex/worktrees/<repo>-<feature>`; never beside it.

1. Inspect branch, dirt, worktrees, and target; preserve unrelated changes.
2. Fetch `origin/main` through any repository-required auth path.
3. Use a filesystem-safe recognizable name. Inspect existing targets; never
   overwrite them or escape the Codex root.
4. Create from `origin/main`; run there, not with `git -C`.

Use one worktree/lifecycle per repo. Inspect before repair. Never remove a
worktree, delete a branch, or reset without authorization.
