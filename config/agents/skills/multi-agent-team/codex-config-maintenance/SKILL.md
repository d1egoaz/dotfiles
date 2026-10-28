---
name: codex-config-maintenance
description: Maintain Codex config, hooks, approvals, skills, and AI instruction files. Use for config.toml, AGENTS.md, rules, hooks, profiles, skill wiring, or trigger behavior.
---

# Codex Config Maintenance

Edit dotfiles sources, never generated targets or installed copies.

- Shared instructions/config: `config/ai/AGENTS.md`, `config/codex/config.toml`
- Codex lead and default subagent per profile: `nix/home-manager/config/xdg.nix`
- Claude models: `config/claude/settings.json`
- Office T3 seats: `chief-of-staff/SKILL.md` beside this skill
- Hooks/rules: `config/codex/hooks.json`, `config/codex/rules/10-shared.rules`
- Skills: `config/agents/skills/multi-agent-team/`

Keep work-only state ignored and encrypted; never print decrypted content. Use
documented hook events only: `Stop`, `PermissionRequest`, `UserPromptSubmit`,
and `SessionStart`. Keep output quiet or valid JSON; notifications use top-level
`notify`.

Keep model IDs and effort at those sources. Changes affect new work only. Preserve signing, approval, and production gates.

Read [`references/config-layering.md`](references/config-layering.md) for config
composition and [`references/validation.md`](references/validation.md) before
validation.
