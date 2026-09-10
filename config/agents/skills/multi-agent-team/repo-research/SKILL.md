---
name: repo-research
description: Inspect repository artifacts only when the user explicitly asks to inspect, trace, diagnose, explain, compare, or gather evidence. Do not trigger for routine implementation reads.
---

# Repo Research

Use this skill for an explicit inspection, tracing, diagnosis, root-cause,
comparison, explanation, or evidence-gathering request. Do not invoke it for
routine reads needed to implement a clearly scoped change.

## Workflow

1. Use the instruction chain already supplied by the host. Read only applicable
   repository instructions not already present, and skip aliases or duplicates.
2. Inspect the exact artifact named by the user. Do not answer from memory when
   the file, issue, PR, config, or command output is available.
3. Search narrowly with `rg`, `rg --files`, or `fd`; use fixed strings and
   filetype filters when useful.
4. Read surrounding functions, tests, schemas, call sites, or history needed
   to support the claim. Separate confirmed evidence from inference.

## Evidence

Lead with the concrete answer when known. Include only the paths, lines,
commands, and output needed to audit the result. Say what was not verified.
Ask only for identifiers or intent that cannot be discovered locally.
