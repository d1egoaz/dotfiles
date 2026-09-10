---
name: command-discipline
description: Run safe, transparent shell workflows. Use explicitly for shell commands, copy-paste syntax, command transparency, process inspection, bulk rewrites, validation commands, or escalation boundaries.
---

# Command Discipline

Use this skill when the user explicitly asks for command shape, shell
syntax, transparency, process inspection, bulk rewriting, or command safety.
Keep routine guidance short and risky operations auditable.

## Command shape

- Run shell-neutral commands directly. Use explicit `zsh -lc` or `bash -lc` only
  for required shell syntax or when the user asks for that form.
- User-facing commands must be single-line, copy-paste-ready commands. Prefer
  CLI over GUI and run multi-repo commands from each repository's directory.
- Explain routine read-only commands in one sentence. Use a
  `Why/What/Expect/Risk` block for destructive, escalated, broad, forced, or
  live-state operations.

## Process inspection

Never run broad process-argument scans in Codex sessions: arguments can contain
unrelated transcript data. Inspect command names, then a specific PID only:

```bash
ps -axo pid,ppid,comm | rg '(^|/)git$|(^|/)ssh$|(^|/)gh$'
ps -p PID -o pid,ppid,args=
```

## Safety and bulk changes

- Confirm before removing files, resetting branches, force pushing, or changing
  live state. If sandboxing blocks a command, retry with scoped escalation and
  concise justification; never request a broad standing rule.
- Use `apply_patch` for manual edits. For bulk rewrites, verify scope with
  `rg` or `fd`, make the rewrite reversible, review the diff, then clean up.
- Use structured validators for JSON and YAML.

If a command fails, report the exact error, likely cause, smallest fix, and
rerun result.
