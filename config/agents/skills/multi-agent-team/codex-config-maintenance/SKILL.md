---
name: codex-config-maintenance
description: Maintain Codex config, hooks, approvals, skills, and AI instruction files. Use for config.toml, AGENTS.md, CLAUDE.md, rules, hooks, profiles, skill wiring, or trigger behavior.
---

# Codex Config Maintenance

Edit dotfiles sources, never generated targets or installed copies.

- Shared instructions: `config/ai/AGENTS.md`
- Shared config: `config/codex/config.toml`
- Agent routes: `config/codex/agents/*.toml`
- Personal profile: `config/codex/profiles/personal.toml`
- Shared hooks/rules: `config/codex/hooks.json`,
  `config/codex/rules/10-shared.rules`
- Shared skills: `config/agents/skills/multi-agent-team/`

Keep work-only state in ignored local sources and encrypted local state. Never
print decrypted content.

Use documented Codex settings and hook events only: `Stop`,
`PermissionRequest`, `UserPromptSubmit`, and `SessionStart`. Keep hook output
quiet or valid JSON; notifications use top-level `notify`.

Shared AI instructions own routing; agent TOML owns model, effort, sandbox, and
role. Changes affect new work only. Preserve signing, approval, and production
gates.

Read [`references/config-layering.md`](references/config-layering.md) for config
composition or local-state questions. Read
[`references/validation.md`](references/validation.md) before validation.
