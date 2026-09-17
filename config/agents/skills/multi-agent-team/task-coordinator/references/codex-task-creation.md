# Visible task creation

Create one task per independent outcome. Split by repository only for separate
write ownership or delivery lifecycles. Keep reference reads in the owning task.

## Project and handoff

Resolve the lead's exact `projectId` from metadata, not its label or cwd.
Non-repository children inherit that project and local environment. Use
projectless only for a projectless lead unless the user selects a project.

Select another project for its saved Git repository or at the user's request.
Inspect `isGitRepository`: saved Git uses an app worktree unless the user requests
local; non-Git uses local. For unsaved nested repositories, retain the umbrella
project and pass the exact path and checkout workflow. Follow host restrictions.
Verify the child's `projectId` after setup; pause and report a mismatch.

Pass the entrypoint's handoff, key, parent identity, project ID, and exact checkout.
Set model and effort in runtime fields. Visible tasks may use bounded native
subagents, but cannot create more visible tasks. Include the shared native naming
rule in their handoff. Each outcome/write set has one owner.

## Naming

Reuse the issue key or choose a short descriptive key. Inspect existing tasks;
one active lead owns each key. Report ownership conflicts before creation.

- Lead: `🤖 [<key>] <goal>`
- Visible: `[<key>] <model-label>-<effort-code> <action> <object>[: <outcome>]`

Display `Sol`, `Terra`, or `Luna`; use exact IDs in runtime fields. Effort codes:
`n`=none, `min`=minimal, `lo`=low, `med`=medium, `hi`=high, `xh`=xhigh,
`max`=max, `ult`=ultra. Only use supported pairs and host-valid names.

Aim for 72 characters, preserving action, object, and useful context. Example:
`[IC-563] Terra-xh Verify controls: close remaining rollout gaps`.
Pass the title at creation or rename once addressable.

## Setup and retries

- `threadId`: addressable task; check setup before writes.
- `clientThreadId`: setup pending. Wait and refresh; a timeout or missing
  `threadId` alone does not justify another attempt.
- Retry only after confirmed creation/setup failure. Add `(retry N)` using one
  more than the highest existing attempt number for that outcome.
- For duplicate owners, keep the earliest `createdAt`, breaking ties by lexical
  `threadId`. Pause later tasks and mark `(superseded)`; preserve and reconcile
  their writes before proceeding.
