# Task creation

Create one task per outcome or write-owning repository/worktree. Keep reference
mapping native; without them use native agents.

## Project and environment

Use the exact lead `projectId`.

- Non-repository children inherit the saved project/local environment. Use
  projectless only for a projectless lead.
- Leave only for another saved Git/user-selected project; inspect
  `isGitRepository`.
- Saved Git uses an app worktree unless local checkout is requested. A non-Git
  umbrella stays local; nested writes use that repository's worktree flow.

Verify `projectId`; pause on mismatch.

## Prompt and ownership

Pass the title and entrypoint-required fields.

## Titles

The 72-character readability budget is not a platform limit. Put key, route,
action, and object first. Add context if it fits; trim at word boundaries.
Never remove action/object or add an ellipsis.

Effort codes: `n`=none, `min`=minimal, `lo`=low, `med`=medium, `hi`=high,
`xh`=xhigh, `max`=max, `ult`=ultra. Keep full effort in task fields/prompts.

Visible tasks are user-owned. Task-local authority may cover commit/push/draft
PR. Otherwise prepare, ask once, and wait. The lead waits with
`wait_threads`; never proxy, quote, or duplicate approval.

## Creation and retries

Check the key; one active lead owns it. Titles are unique.

- `threadId`: ready task.
- `clientThreadId`: setup pending, not failure. Wait and refresh; do not create
  another task.
- Retry only after setup failure. Increment the highest retry number; keep the
  title within 72 characters.
- If a pending task later duplicates an owner, keep the earliest `createdAt`
  task, breaking ties by lexical `threadId`; pause and mark later tasks
  `(superseded)`.

Keep repository worktrees, commits, and PRs separate. Creation never authorizes
commit, push, PR, merge, deployment, or production mutation.
