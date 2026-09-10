## Visible task creation

Create a visible task only for a durable independent outcome, separate
repository or worktree, or work the user wants to inspect in the sidebar. Do
not create one for a status question or an ordinary single-task request. If
native visible-task controls are unavailable, keep ownership in the lead and
use native subagents instead of unmanaged shell sessions.

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
the coordination key, selected route, parent title, outcome, constraints,
verification, publication boundary, and stopping condition. Require the
execution task to use the canonical subagent naming schema and to reassess
native delegation after it understands the scope.

Keep worktrees, commits, and PRs separate across repositories. The lead owns
sequencing and cross-repository integration. Creating a task does not authorize
commit, push, PR creation, merge, deployment, or production mutation.
