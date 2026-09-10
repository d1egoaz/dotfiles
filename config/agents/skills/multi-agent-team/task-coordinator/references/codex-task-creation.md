## Visible task creation

Create a visible task for a durable independent outcome, a write-owning
repository or worktree, or work the user wants to inspect in the sidebar. An
explicit `$task-coordinator` invocation requests this workflow; for repository
write work, create at least one visible implementation task unless the user
asks to keep implementation in the lead. Do not create a child for a one-time
status question. If visible-task controls are unavailable, report that and use
native subagents instead of unmanaged shell sessions.

A repository used only as read-only reference material is not a separate
visible outcome. Delegate that mapping to a native `explorer` inside the owning
implementation task.

### Coordination surface

For repository work, list saved projects and prefer an exact saved Git project;
only a project marked `isGitRepository = true` can provide an app-managed
worktree. If only a saved non-Git umbrella is available, use it as the surface
and include the exact nested repository path in each prompt. Do not require
every nested repository to be registered separately.

Use an app-managed worktree for a saved Git project unless the user explicitly
asks for its local checkout. For a non-Git umbrella, use a local task. For
write work in a nested repository, require the execution task to use the
repository's worktree workflow before editing; read-only work may use the
primary checkout.

### Prompt contract

Pass the title explicitly when supported, otherwise rename immediately. Include
the coordination key, exact selected model, effort, compact display route,
parent title, outcome, constraints, verification, publication boundary, and
stopping condition. Require the execution task to use the canonical subagent
naming schema and to reassess native delegation after it understands the scope.

One lead owns a coordination key. Before each creation, inspect existing tasks
for that key. If another active lead owns it, stop and report the collision.
Do not reuse an identical title for another outcome or attempt. This check is
best effort because the visible-task API has no atomic key reservation. After
creation:

- A returned `threadId` identifies a ready visible task.
- A returned `clientThreadId` means worktree setup is pending, not failed. Do
  not call `create_thread` again for that outcome merely because the task is not
  addressable yet. Wait for provisioning, then refresh the task list. If the
  runtime could not accept a title at creation, retain the intended title and
  apply it only after a resolved `threadId` is available.
- Retry only after an explicit create/setup error or a task state that reports
  setup failure. A timeout, missing `threadId`, or `clientThreadId` alone is not
  failure. Set `N` to one more than the highest existing retry number for the
  same key and outcome, treating the original attempt as zero, then add
  `(retry N)` after the outcome. Shorten only the outcome at a word boundary as
  needed so the complete retry title remains within 56 characters.
- If a pending attempt later appears after a replacement exists, rename it with
  `(superseded)` and preserve it unless cleanup is explicitly authorized.
- After provisioning, refresh the task list again. If duplicate tasks exist for
  the same key and outcome, keep the task with the earliest `createdAt` as owner,
  breaking a tie by lexical `threadId`. Send later duplicates a focused pause,
  shorten only their outcome as needed, rename them with `(superseded)`, and do
  not send them more work.

Keep worktrees, commits, and PRs separate across repositories. The lead owns
sequencing and cross-repository integration. Creating a task does not authorize
commit, push, PR creation, merge, deployment, or production mutation.
