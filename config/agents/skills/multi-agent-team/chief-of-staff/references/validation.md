# Validation

## Order

1. Deterministic first: run the acceptance commands, tests, linters, and type
   checks, or have the owning worker run them. A failing check blocks
   acceptance whatever validators say.
2. Validators: routine work gets one Claude validator. Substantial work gets
   a Claude and a Codex validator launched together, each a fresh
   `delegate_task` with role `review` at `medium`, or `high` for security,
   data, or migration risk.
3. Independence: send each validator the objective, acceptance criteria,
   decision record, changed paths or branch and SHA, and the check commands.
   Withhold worker summaries and the other validator's findings. Each
   inspects the diff and reruns checks itself, and must not edit files; T3
   cannot enforce read-only.

## Brief

Ask for findings only, each with severity, file and line, evidence (command
output or code), and the smallest fix. Cover acceptance criteria,
architecture compliance, security and reliability, test coverage, regression
risk, needless complexity, and integration consistency. A clean result lists
what was checked.

- `blocker`: wrong behavior, failing check, security or data risk, or plan
  violation. Must fix.
- `major`: likely defect, or a stated criterion without coverage. Fix, or the
  user waives it.
- `minor`: quality or clarity. Fix if cheap; otherwise list it.

## Triage

Verify each finding against the code before acting. Model agreement raises
confidence but proves nothing; it never overrides a failing check, and a
passing check does not dismiss a verified finding. Small fixes stay with the
lead and are rechecked by the deterministic checks, with no new delegate.
Send other confirmed defects to the owning worker as a new contract citing the
finding, then revalidate only that slice with a fresh validator that gets the
prior finding and the fix.
