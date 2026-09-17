---
name: codex-config-maintenance
description: Maintain Codex config, hooks, approvals, skills, and AI instruction files. Use for config.toml, AGENTS.md, rules, hooks, profiles, skill wiring, or trigger behavior.
---

# Codex Config Maintenance

Edit dotfiles sources, never generated targets or installed copies. The single
source for active Codex routing IDs, tiers, effort, and role capability is
`nix/data/agent-routing.toml`.

- Shared instructions/config: `config/ai/AGENTS.md`, `config/codex/config.toml`
- Generated Codex: `config/codex/agents/generated/<profile>/*.toml`
- After registry edits run `just agent-routing-generate`; never edit outputs.
- Hooks/rules: `config/codex/hooks.json`, `config/codex/rules/10-shared.rules`
- Skills: `config/agents/skills/multi-agent-team/`

Keep work-only state ignored and encrypted; never print decrypted content. Use
documented hook events only: `Stop`, `PermissionRequest`, `UserPromptSubmit`,
and `SessionStart`. Keep output quiet or valid JSON; notifications use top-level
`notify`.

The registry owns runtime model, effort, sandbox, and role resolution. Changes
affect new work only. Preserve signing, approval, and production gates.

Read [`references/config-layering.md`](references/config-layering.md) for config
composition and [`references/validation.md`](references/validation.md) before
validation.
