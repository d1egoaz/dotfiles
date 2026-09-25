# Visible tasks

Create one task per outcome with its own repository, PR, or delivery
lifecycle. Visible tasks may use native subagents but cannot create more
visible tasks. Each outcome and write set has one owner.

## Project and runtime

- Use the lead's exact `projectId` from metadata, not its label or cwd. Pick
  another project only for its saved Git repository or at the user's request;
  saved Git uses an app worktree unless the user asks for local.
- For an unsaved nested repository, keep the umbrella project and pass the
  exact path and checkout workflow. Verify the child's `projectId` after setup.
- Set the registry-resolved model ID and effort in host runtime fields.
- Pass the handoff, key, parent identity, project ID, and exact checkout.
- A setup timeout alone does not justify another task; refresh and inspect
  existing tasks before any retry, and never leave two owners for one outcome.

## Titles

- Lead: `🤖 [<key>] <goal>`
- Visible: `<state-prefix> [<key>] <tier>-<model-label>-<effort-code> <action> <object>[: <outcome>]`

Reuse the issue key or a short descriptive key. Effort codes: `n` none,
`min` minimal, `lo` low, `med` medium, `hi` high, `xh` xhigh, `max` max,
`ult` ultra. Aim for 72 characters, for example
`🆕 [IC-563] balanced-<model-label>-lo Verify remaining rollout gaps`.

| Prefix | State | Use when |
| --- | --- | --- |
| `🆕` | spawned | Created or completing setup; no work evidence yet. |
| `🔄` | in progress | Doing authorized work or safely retrying after recovery. |
| `⛔` | blocked | Access, approval, or an external dependency stops all authorized work. |
| `❌` | error | An unexpected failure has no safe in-scope recovery yet. |
| `❓` | needs info | A specific user decision is required; the task asks the user directly. |
| `👀` | ready for review | Work is verified and a PR or deliverable waits on the user's review. |
| `✅` | done | Acceptance criteria are verified and nothing waits on the user. |

Create with `🆕`. The owner renames its own title at each state change; the
lead reconciles a stale title from evidence. Do not rename for routine
progress. A retry returns to `🔄` without erasing the earlier error evidence.
