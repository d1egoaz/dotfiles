---
name: codex-config-maintenance
description: Maintain Codex config, hooks, approval rules, skills, and AI instruction files in dotfiles. Use when asked about Codex config, `config.toml`, `AGENTS.md`, `CLAUDE.md`, approval rules, `~/.codex/rules`, hooks, notifications, split config, config layering, dotfiles, skills, install skill, or why Codex did or did not trigger a skill.
---

# Codex Config Maintenance

## Overview

Use this skill for Codex configuration and dotfiles-managed AI instructions. Prefer official Codex mechanisms over ad hoc workarounds.

## Source Of Truth

- Edit `~/dotfiles/config/ai/AGENTS.md` for the home-level `AGENTS.md`, `~/.codex/AGENTS.md`, and `~/.claude/CLAUDE.md` content.
- Edit `~/dotfiles/config/codex/config.toml` for Codex config.
- Edit `~/dotfiles/config/codex/hooks.json` for Codex hooks.
- Edit `~/dotfiles/config/codex/rules/10-shared.rules` for tracked shared approvals.
- Keep work-only approvals local-only, for example `~/.codex/rules/90-work-local.rules`.
- Edit `~/dotfiles/config/agents/skills/multi-agent-team/<skill>/` for this grouped user-level skill set. Home Manager links that group to `~/.agents/skills/multi-agent-team`.

## Skills

- Use `$skill-creator` or its `init_skill.py` helper when creating a new skill.
- Keep `SKILL.md` focused. Put trigger words in the frontmatter `description`.
- Add `agents/openai.yaml` for display metadata and invocation policy.
- Current Codex docs use `.agents/skills` for user and repo skills. Do not copy Symphony's `.codex/skills` path unless the local build is verified to scan it.
- Codex detects skill changes automatically, but restart Codex if a new or changed skill does not appear.

## Hooks And Notifications

- Use documented Codex hook events only: `Stop`, `PermissionRequest`, `UserPromptSubmit`, and `SessionStart`.
- Do not add a `Notification` hook to `hooks.json`; use top-level `notify = [...]` in `config.toml` for Codex notification payloads.
- Keep hook commands quiet: exit `0` with no output or emit valid JSON.
- Do not add Desktop log watchers, launchd sound hacks, or other shadow mechanisms unless the user explicitly asks for that workaround.

## Config Layering

- Codex does not document a general `include`, `import`, or `config.d` mechanism for `config.toml`.
- For nested repos under `~/work/github.com/1debit/*`, `~/work/.codex/config.toml` is not a reliable shared project layer because Codex resolves each nested `.git` as a repo root.
- Prefer repo-local `.codex/config.toml` when a repo needs project config, or a Home Manager symlink if one source of truth is required.

## Validation

```fish
jq . config/codex/hooks.json >/dev/null
taplo check config/codex/config.toml
codex debug prompt-input hooks-json-smoke
just check
```

If `codex debug prompt-input` cannot read `~/.codex/sessions` because of sandboxing, rerun the same command with escalated permissions.

For skill YAML when PyYAML is unavailable, use Ruby YAML to parse `SKILL.md` frontmatter and `agents/openai.yaml`.
