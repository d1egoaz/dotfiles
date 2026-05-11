---
name: repo-research
description: Inspect exact repo artifacts before planning or changing code. Use when asked to inspect, investigate repo behavior, trace a code path, find references, compare files, explain why something changed, find root cause, read a specific file, answer where something is defined, or gather evidence with `rg`, `fd`, `bat`, `jq`, or `yq`.
---

# Repo Research

## Overview

Use this skill to ground answers in the real repository instead of memory or assumptions. Read enough context to make claims auditable and rerunnable.

## Workflow

1. Read instruction files first.
   - Check parent and repo-local `AGENTS.md` and `AGENTS.local.md`.
   - Treat `CLAUDE.md` and `CLAUDE.local.md` the same way when present.
   - Follow the most specific applicable instruction.

2. Inspect the exact artifact.
   - If the user names a file, PR, issue, Slack thread, config, plan, or command output, inspect that artifact directly.
   - Do not answer from memory when the artifact is available.
   - When a fact may be stale and is cheap to verify, verify it.

3. Search with the fastest narrow tool.
   - Prefer `rg` for text and `rg --files` or `fd` for files.
   - Use fixed-string search when the input is literal.
   - Use filetype filters to reduce noise.

4. Read surrounding context.
   - Open full functions, nearby tests, schema definitions, and call sites.
   - For regressions, trace history or recent changes before proposing a fix.
   - Separate confirmed evidence from inference.

## Command Patterns

```fish
rg -n -C2 --hidden --glob '!.git/*' 'pattern'
rg -n -F 'exact string'
rg -n -t go 'pattern'
rg -l 'FIXME:'
fd '.env' . -H -E node_modules -d 3
fd -e go -H -E vendor
bat -n --paging=never file.go
jq -r '.version' package.json
yq '.kind, .metadata.name' deployment.yaml
```

## Output

- Lead with the concrete answer when known.
- Include the minimum file path, line, command, or output needed to audit the claim.
- Say what was not verified when verification was skipped or blocked.
- Ask only for product intent or unavailable identifiers that cannot be discovered locally.
