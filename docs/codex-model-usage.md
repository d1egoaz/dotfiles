# Codex model-usage audit

`bin/files/codex-model-usage` counts unique turns using only `turn_context`
metadata. It never parses conversation message records or changes sessions,
tasks, files, or automation state. Run `--json` for structured data, `--summary`
for the compact report, or omit both for the detailed text report.

## Scheduled task prompt

Use this prompt for the existing daily office audit. This file documents the
prompt; it does not install or update a live scheduled task or its schedule.
The absolute paths are for the office Mac; the script also accepts repeated
`--root` options on other machines.

```text
Run `/Users/diego.alvarez/dotfiles/bin/files/codex-model-usage --root /Users/diego.alvarez/work --root /Users/diego.alvarez/dotfiles --json` once.
Use only that command's JSON output. Do not inspect conversation message records, other files, sessions, tasks, or automation state. This audit is read-only: do not modify any of them, create tasks, or change routing.
If the command fails or the JSON lacks schema_version 2 and summary_lines, report that the audit is unavailable and include the command error or schema mismatch. Do not fall back to inspecting session files, reuse old results, or invent zero counts.
Otherwise return the strings in summary_lines verbatim, in order, separated by real line breaks. Do not add a heading, explanation, blank lines, bullets, code fences, escaped newlines, or trailing backslashes. The report must stay under 12 lines.
The report already contains the combined last-24-hour model@effort counts; current versus previous 7-day Luna, Terra, Sol, and premium shares and their denominator; premium and Sol deltas; effort buckets; each root's current 7-day model breakdown; a metadata-based review candidate; and coverage warnings.
Treat cheaper/costlier as routing direction within the stated model group, never a quality verdict or measured savings. Turn counts are a routing proxy, not billed token or dollar usage.
```

## Interpretation

- Windows are rolling 24 hours and two adjacent seven-day windows, measured in
  UTC from `generated_at`. `window_bounds` gives the precise boundaries.
- Shares retain the existing denominator: all `gpt-5.6-*` turns. Every model is
  included in total turns and per-root breakdowns. The compact report shows
  both denominators and the count of other models, so changing providers cannot
  silently look like a reduction in overall premium usage.
- “Premium” means Terra + Sol, an audit category rather than a price estimate.
  A premium-share increase can coincide with a Sol-share decrease. Such a
  report explicitly flags mixed routing signals. For example, the supplied
  92.1% versus 88.8% premium shares rose 3.3 points while Sol's 40.1% versus
  56.9% fell 16.8 points; the premium delta alone misses that shift.
- Luna “value” means high/xhigh/max and “utility” means low/medium. Sol
  “quality” means high/xhigh/max/ultra and “below-high” means low/medium.
  These historical names describe effort settings, not achieved value or
  quality. Missing or unrecognized efforts are reported separately; Luna
  ultra is also unclassified by the existing Luna bucket definitions.
- The review candidate is the root with the most Sol low/medium turns. It is
  a prompt to check routing intent, not evidence that any task should be
  downgraded. This collector cannot infer task complexity, outcomes, retries,
  latency, token usage, or dollars from its allowed metadata.

## Data handling and validation

Schema version 2 retains the existing window and trend fields and adds
`summary_lines`, definitions, window bounds, missing-directory coverage,
duplicate-context counts, other-model counts, unclassified effort counts, and
the mixed-signal flag. `sol_below_high_turns` now counts only known low/medium
efforts; it no longer includes missing or unrecognized efforts.

The collector scans both active and archived JSONL files, including old files
that contain recent resumed turns. It recognizes the Codex envelope header
with optional whitespace (`type` first, or `timestamp` then `type`), before
parsing a `turn_context`. Nested `type` fields inside messages do not qualify.
It deduplicates by `turn_id`, keeping the earliest in-window context, and
assigns overlapping roots to the longest matching path. Roots are matched
literally; a worktree outside a requested root needs its own `--root` option.

No existing session directory is an error; an existing empty directory yields
zero turns and unavailable shares. A missing optional directory or malformed
metadata is reported in the compact output. Counts can then be incomplete.
Parse-error counts describe recognized metadata records, not validation of
conversation messages. A missing root cannot be distinguished from a root
with no recorded turns. Historical files with no recent turns are still scanned
to avoid dropping resumed sessions; runtime grows with session history size.

Run `just codex-model-usage-test` for regression tests using synthetic records
only, then `just check` for the repository checks. Tests cover the report,
window boundaries, alternate models, effort classification, malformed metadata,
message exclusion, resumed sessions, archived duplicates, and root scoping.
