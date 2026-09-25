---
name: git-history-orientation
description: Orient on an unfamiliar repository with read-only Git history. Use for churn, ownership, contributor, momentum, bus-factor, or history-guided file analysis.
---

# Git History Orientation

Use for a read-only map of churn, ownership, contributors, or cadence before
deeper inspection. History is directional evidence, not proof of quality, team
health, productivity, or root cause.

Run from the relevant repository subtree. Prefer path-limited history when
generated files, lockfiles, vendored code, migrations, or changelogs dominate.
Never fetch or rewrite history.

## Commands

```bash
git rev-parse --show-toplevel
git rev-parse --is-shallow-repository
git status --short
git log --format=format: --name-only --since="1 year ago" -- . | sed '/^$/d' | sort | uniq -c | sort -nr | head -20
git shortlog -sn --no-merges
git shortlog -sn --no-merges --since="6 months ago"
git log --format='%ad' --date=format:'%Y-%m' -- . | sort | uniq -c
```

Report shallow-history and dirty-worktree limits. Discount generated noise.
Treat churn as “read first,” compare all-time with recent contributors, and
verify merge strategy before ownership claims. Describe cadence shape without
equating commit count to productivity.

Return only:

```text
Churn: <hotspots and noise caveats>
Ownership: <all-time versus recent, with merge caveat>
Cadence: <trend and path scope>
Read first: <2-5 paths>
Confidence: <what history cannot prove>
```

For requested follow-up inspection, read the "Read first" paths or fan out
explore subagents.
