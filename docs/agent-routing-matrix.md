# Agent routing matrix

Generated from `nix/data/agent-routing.toml` by `agent-routing-generate`;
do not edit. Each cell is the harness model value and requested effort.

Roles: `spawn-subagent-gather` (read-only, economy), `spawn-subagent-explore` (read-only, balanced), `spawn-subagent-review` (read-only, balanced), `spawn-subagent-implement` (workspace-write, economy).

## office

| Harness | Lead | `spawn-subagent-gather` | `spawn-subagent-explore` | `spawn-subagent-review` | `spawn-subagent-implement` | Unnamed spawn |
|---|---|---|---|---|---|---|
| Codex (Office Codex) | `gpt-6-sol` / high | `gpt-6-luna` / low | `gpt-6-sol` / low | `gpt-6-sol` / low | `gpt-6-luna` / xhigh | `gpt-6-luna` / xhigh |
| Claude Code (Office Claude Code) | `opus` (`claude-opus-5-5`) / high | `sonnet` (`claude-sonnet-5`) / low | `opus` (`claude-opus-5-5`) / low | `opus` (`claude-opus-5-5`) / low | `sonnet` (`claude-sonnet-5`) / xhigh | `sonnet` (`claude-sonnet-5`) / session effort |

## personal

| Harness | Lead | `spawn-subagent-gather` | `spawn-subagent-explore` | `spawn-subagent-review` | `spawn-subagent-implement` | Unnamed spawn |
|---|---|---|---|---|---|---|
| Codex (Personal Codex home tiers) | `xiaomi/mimo-v2.6-pro` / high | `deepseek-v4.1-flash` / low | `deepseek-v4-pro` / low | `deepseek-v4-pro` / low | `deepseek-v4.1-flash` / xhigh | `deepseek-v4.1-flash` / xhigh |

## Claude Code notes

- `config/claude/settings.json` sets the lead (`model`, `effortLevel`), pins each
  alias to its model ID (`ANTHROPIC_DEFAULT_<ALIAS>_MODEL`), sets the unnamed-spawn model
  (`CLAUDE_CODE_SUBAGENT_MODEL`), turns off nesting
  (`CLAUDE_CODE_MAX_SUBAGENT_SPAWN_DEPTH=1`, like Codex `max_depth = 1`),
  and denies the built-in `Explore` so `spawn-subagent-explore` is the only explorer.
- Read-only roles deny `Edit`, `Write`, and `NotebookEdit` but keep `Bash`,
  and `permissionMode` is ignored while the lead runs in auto mode. Read-only
  is enforced by instructions and permission review, not by a sandbox as in Codex.

## Home tier notes

- Home tiers belong to the `personal` profile only; office machines never use them.
- Personal Codex consumes them through the local gateway.
- Bare model IDs route through OpenCode Go; vendor-prefixed IDs route through
  OpenRouter.
