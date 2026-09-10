---
name: codex-config-maintenance
description: Maintain Codex config, hooks, approvals, skills, and AI instruction files. Use for config.toml, AGENTS.md, CLAUDE.md, rules, hooks, profiles, skill wiring, or trigger behavior.
---

# Codex Config Maintenance

Use this skill for Codex configuration and dotfiles-managed AI instructions.
Prefer documented Codex mechanisms and repository source of truth over ad hoc
workarounds.

## Source of truth

Paths below are relative to the dotfiles repository root:

- `config/ai/AGENTS.md`: shared home-level AI instructions.
- `config/codex/config.toml`: shared Codex settings.
- `config/codex/agents/*.toml`: tracked named agents and their routes.
- `config/codex/profiles/personal.toml`: tracked personal profile; keep ignored
  `profiles/work.local.toml` for work-only settings.
- `config/codex/hooks.json`: shared hooks; use an ignored local hooks file for
  machine-only behavior.
- `config/codex/rules/10-shared.rules`: shared approvals; keep local approvals
  outside tracked configuration.
- `config/agents/skills/multi-agent-team/<skill>/`: grouped user skills.
  Keep work-only skills in the ignored local skill group.

Keep work-internal endpoints, trust entries, and local state out of shared
files. Preserve ignored local state through the encrypted local-state workflow;
never print decrypted contents.

## Skills and hooks

- Keep `SKILL.md` focused, put trigger words in frontmatter, and add
  `agents/openai.yaml` for display metadata and invocation policy.
- Use `.agents/skills` for user and repository skills. Restart Codex if a new
  or changed skill does not appear.
- Use only documented hook events: `Stop`, `PermissionRequest`,
  `UserPromptSubmit`, and `SessionStart`. Keep commands quiet or emit valid
  JSON. Do not add a `Notification` hook; use top-level `notify` settings.

## Routing and safety

Keep complete model-routing policy in the shared AI instructions. Named agent
files own their model, reasoning effort, sandbox, and role instructions; a
configuration change does not replace an already-running task. Do not weaken
signing, approval, or production-safety gates while simplifying configuration.

For configuration layering details, read
[`references/config-layering.md`](references/config-layering.md) only when that
model is relevant. For checks, read
[`references/validation.md`](references/validation.md) only when validating a
change.
