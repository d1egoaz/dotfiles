---
name: codex-config-maintenance
description: Maintain Codex config, hooks, approval rules, skills, and AI instruction files in dotfiles. Use when asked about Codex config, `config.toml`, `AGENTS.md`, `CLAUDE.md`, approval rules, `~/.codex/rules`, hooks, notifications, split config, config layering, dotfiles, skills, install skill, or why Codex did or did not trigger a skill.
---

# Codex Config Maintenance

## Overview

Use this skill for Codex configuration and dotfiles-managed AI instructions. Prefer official Codex mechanisms over ad hoc workarounds.

## Source Of Truth

- Edit `~/dotfiles/config/ai/AGENTS.md` for the home-level `AGENTS.md`, `~/.codex/AGENTS.md`, and `~/.claude/CLAUDE.md` content.
- Edit `~/dotfiles/config/codex/config.toml` for shared Codex config.
- Edit `~/dotfiles/config/codex/profiles/personal.toml` for tracked personal profile config.
- Edit ignored `~/dotfiles/config/codex/profiles/work.local.toml` for work-only Codex config.
- Edit `~/dotfiles/config/codex/hooks.json` for shared Codex hooks.
- Edit ignored `~/dotfiles/config/codex/hooks.work.local.json` for work-only local hooks.
- Edit `~/dotfiles/config/codex/rules/10-shared.rules` for tracked shared approvals.
- Keep work-only approvals local-only, for example `~/.codex/rules/90-work-local.rules`.
- Keep work-internal MCP URLs and work project trust entries out of shared `config/codex/config.toml`; put them in ignored `profiles/work.local.toml`.
- Edit `~/dotfiles/config/agents/skills/multi-agent-team/<skill>/` for this grouped user-level skill set. Home Manager links that group to `~/.agents/skills/multi-agent-team`.
- Edit ignored `~/dotfiles/config/agents/skills/work.local/<skill>/` for work-only skills. Office machines link that group to `~/.agents/skills/work-local`.
- Keep ignored local state durable by syncing encrypted `local_state` entries in `secrets/codex.yaml` with `just local-state-sync` or `bin/files/dotfiles-local-state sync`; `just switch` only restores missing files. Restored files are plaintext copies on disk, not runtime-decrypted files.
- Use `bin/files/dotfiles-local-state` for SOPS-backed local files. Codex work rules and work-only skills are resources in that generic local-state schema, not Codex-specific shell helpers.

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
- Codex 0.134.0 and later no longer supports a top-level `profile = "name"` selector in `config.toml`; this repo uses Home Manager to compose the default `~/.codex/config.toml` from the shared config plus the selected Nix profile fragment.
- Profile fragments must either be empty/comment-only or begin with a TOML section header. Home Manager validates this before concatenating fragments so top-level keys cannot accidentally land inside the shared file's last section.
- Office machines use ignored `config/codex/profiles/work.local.toml` when present; if it is missing, Home Manager falls back to shared Codex config and prints a warning.
- Office machines restore ignored work-local skills from encrypted `secrets/codex.yaml` when `config/agents/skills/work.local` is missing or empty.
- For nested work repos, a shared parent `.codex/config.toml` is not a reliable project layer because Codex resolves each nested `.git` as a repo root.
- Prefer repo-local `.codex/config.toml` when a repo needs project config, or a Home Manager symlink if one source of truth is required.

## Validation

```fish
jq . config/codex/hooks.json >/dev/null
taplo check config/codex/config.toml
taplo check config/codex/profiles/personal.toml
test ! -f config/codex/profiles/work.local.toml || taplo check config/codex/profiles/work.local.toml
sh -c 'tmp=$(mktemp "${TMPDIR:-/tmp}/codex-profile.XXXXXX.toml"); { cat config/codex/config.toml; printf "\n"; cat config/codex/profiles/personal.toml; } > "$tmp"; taplo check "$tmp"; rm -f "$tmp"'
codex debug prompt-input hooks-json-smoke
bin/files/dotfiles-local-state check
just check
```

If `codex debug prompt-input` cannot read `~/.codex/sessions` because of sandboxing, rerun the same command with escalated permissions.

For skill YAML when PyYAML is unavailable, use Ruby YAML to parse `SKILL.md` frontmatter and `agents/openai.yaml`.
