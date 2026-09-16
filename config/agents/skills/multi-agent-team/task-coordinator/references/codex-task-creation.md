# Visible task creation

Create one task per durable outcome or write-owning repository/worktree. Keep
reference mapping in a native `explorer`. Use native agents without visible
controls.

## Project and environment

Resolve the lead's exact `projectId` from task metadata.

- Non-repository children inherit the lead's saved project and local
  environment. Use projectless only for a projectless lead.
- Leave it only for a different saved Git repository or user-selected project;
  inspect `isGitRepository` first.
- Use an app worktree for a saved Git project unless the user requests its local
  checkout. A non-Git umbrella stays local; nested-repository write work uses
  that repository's worktree workflow.

Verify `projectId`; pause and report a mismatch.

## Prompt and ownership

Pass title, key, project IDs, model/effort, route, parent, card, outcome,
constraints, verification, publication boundary, stop condition, and native
naming contract.

Visible tasks are user-owned. Task-local authorization may cover commit, push,
or draft PR creation. Otherwise prepare, ask once, and wait. Codex surfaces
attention. The lead waits with `wait_threads`; never proxy, quote, or duplicate
approval.

## Creation and retries

Check for the key before creation. One active lead owns it. Titles are unique.

- `threadId`: ready task.
- `clientThreadId`: setup pending, not failure. Wait and refresh; do not create
  another task.
- Retry only after explicit setup failure. Increment the highest retry number;
  keep the title within 56 characters.
- If a pending task later duplicates an owner, keep the earliest `createdAt`
  task, breaking ties by lexical `threadId`; pause and mark later tasks
  `(superseded)`.

Keep repository worktrees, commits, and PRs separate. Creation never authorizes
commit, push, PR, merge, deployment, or production mutation.
