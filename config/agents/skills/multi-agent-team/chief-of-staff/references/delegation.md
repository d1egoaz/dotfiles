# Delegation

## Resolve routes

1. Call `orchestrator_capabilities` once per thread.
2. Confirm each seat's model and effort from the Seats table are listed under
   its provider (`claudeAgent`, `codex`); child seats need
   `canRunChildTask: true`. Use `search` if the short list omits a model.
   Confirm `effort` on Claude and `reasoningEffort` on Codex.
3. If a provider, model, or effort is missing, say so and stop. Never
   substitute a model, effort, or provider.

Targets:

- Codex: `{providerInstanceId: "codex", model: <id>, options: {reasoningEffort: <effort>}}`
- Claude: `{providerInstanceId: "claudeAgent", model: <id>, options: {effort: <effort>}}`

## Launch

- One `delegate_task` per child with `title`, `role`, and `clientRequestId`
  `cos-<key>-<seat>-<slice>-<attempt>`. Record the returned `taskId` and
  `childThreadId` in the ledger. Resending the same ID and payload returns the
  existing task without a new run; bump the attempt only for a deliberate
  retry.
- Roles: council `design`; worker `implementation`, `test`, or `research`;
  validator `review`.
- Default `mode: "async"`. Do not poll `task_status`; follow each returned
  `next` and read results when completion wakes you. Use `mode: "wait"` only
  when the next step needs that result now; a wait timeout does not cancel
  the child.
- Codex children get `runtimeMode: "auto"`, because managed Codex
  requirements can reject an inherited `full-access`. Omit it when the lead
  runs a narrower mode; T3 rejects a child mode above the parent's. Claude
  children omit `runtimeMode` unless the user asks for a supervised child.
- Never steer or re-check with `t3_thread_send`. Each new round is a new
  `delegate_task` carrying the brief, prior findings, and objections.

## Isolation

- A child sees only `task`. Put every needed fact in it; never "see above".
- `shared` (default) uses the parent checkout. Use it when write sets are
  disjoint.
- `worktree` starts a new checkout from the parent commit; uncommitted parent
  edits are absent. Use it for overlapping sets that must run in parallel,
  risky experiments, or one PR per child. Commit the base first when a
  worktree child needs it. The contract grants commits on the child branch
  only, through `$signed-pr-publish`; the worker reports branch and SHA, and
  merging is a planned step with one owner.
- Overlap: split ownership by file, else serialize, else isolate. Never two
  writers on one file in `shared`.

## Task contract

Send these labels verbatim:

- Task and context: the user's goal in their words, the approved plan slice,
  decisions already made, repository paths, and what the lead already knows.
- Owned files: the exact write set; everything else is read-only.
- Depends on: inputs and their current state.
- Done means: acceptance criteria and the exact commands to run.
- Constraints: prohibited changes, repository rules, and authorization (file
  edits only unless stated). Do not spawn children, widen scope, or make
  architecture calls.
- Stop and ask if: scope must grow, the plan looks wrong, an owned file needs
  another owner's change, or something needed is unreachable. Never
  substitute, mock, or guess.
- Report: changed files, commands with exit status, open issues, and
  deviations from the plan.
