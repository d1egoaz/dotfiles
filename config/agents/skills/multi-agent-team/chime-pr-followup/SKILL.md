---
name: chime-pr-followup
description: Handle Chime repo PR follow-ups safely. Use when working in `chime-tf`, `chime-cd`, `tf-sync`, or their worktrees; when a PR branch has CI bot commits, non-fast-forward pushes, generated artifacts, `cdk.tf.json`, `zz.vladsum`, `zz.auto-generated`, Terraform overrides, Codex review false positives, TFE plan review, IAM scope, or force-push questions.
---

# Chime PR Follow-Up

## Overview

Use this skill for Chime-specific PR follow-up rules that are too repo-specific for global AGENTS. Always read the active repo's own `AGENTS.md` or `CLAUDE.md` first.

## Instruction Discovery

- `/Users/diego.alvarez/work/AGENTS.md` is the Chime workspace overlay.
- `chime-tf/AGENTS.md` is canonical and `CLAUDE.md` points to it.
- `chime-tf/.claude/skills/snowflake-provider-bump/SKILL.md` exists but contains older workflow mechanics; use its domain logic only after reconciling with current signing, attribution, and worktree rules.
- The primary `chime-cd` checkout may not have a populated `AGENTS.md`; check active worktrees and current repo state before assuming one exists.
- `tf-sync` currently has no top-level `AGENTS.md` or `CLAUDE.md`; parent instructions apply unless a repo-local file is added later.
- Treat worktree-only AGENTS files as evidence, not authority, unless the same rule repeats or matches the canonical repo guidance.

## Generated Artifacts

- In `chime-tf`, CI owns `environments/*/cdk.tf.json` and `environments/*/zz.vladsum/**`.
- In `chime-cd`, generated outputs include `zz.auto-generated/**`, merged config vars, and Helm override aggregates.
- `chime-cd/overrides/terraform/**` is downstream of `chime-tf`; point changes at the owning source repo when needed.
- Do not hand-edit or hand-commit generated files unless the repo instructions or user explicitly require it.

## Follow-Up Commit Workflow

After a PR is open on `chime-tf` or `chime-cd`, assume the remote branch has moved because CI may have pushed generated artifacts.

1. Fetch the remote branch.
2. Make the intended source changes.
3. Commit signed with the `Assisted-by` footer.
4. Rebase on the latest remote branch.
5. Verify signature and attribution survived.
6. Push normally after the rebase.

Fish-compatible checks:

```fish
git fetch origin <branch>
git log -1 --pretty='%G? %s'
git log -1 --pretty=%B | rg '^Assisted-by: .+ via .+$'
git push
```

Do not force-push over CI bot commits unless the user explicitly authorizes it and you have verified no shared bot or reviewer state will be lost.

## Review And Plan Rules

- For IAM, policy, networking, or workspace-ownership changes in `chime-tf`, the rendered TFE plan is the source of truth.
- Compare effective `Effect`, `Action`, `Resource`, and `Condition` tuples instead of trusting local diff shape.
- For Codex reviewer comments about missing generated files or undeclared `var.ATLAS_WORKSPACE_NAME` / `var.TFE_RUN_ID`, verify generated output first, then reply with concrete evidence instead of ignoring it.
- Before answering PR metadata questions, query the live GitHub PR object.
