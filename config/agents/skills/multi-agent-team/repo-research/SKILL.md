---
name: repo-research
description: Inspect repository artifacts only when the user explicitly asks to inspect, trace, diagnose, explain, compare, or gather evidence. Do not trigger for routine implementation reads.
---

# Repo Research

Use for explicit inspection, tracing, diagnosis, comparison, or evidence; skip
routine implementation reads.

Use supplied instructions; read only missing guidance. Inspect the named
artifact, search narrowly with `rg`, `rg --files`, or `fd`, and read only the
code, tests, schemas, callers, or history needed.

Lead with evidence, label inference, cite needed paths/commands, state
what was not verified, and ask only for undiscoverable identifiers or intent.
