# Agent routing matrix

Generated from `nix/data/agent-routing.toml` by `agent-routing-generate`;
do not edit. Each cell is the harness model value and requested effort.

Roles: `spawn-subagent-explore` (read-only, balanced), `spawn-subagent-implement` (workspace-write, economy), `spawn-subagent-review` (read-only, balanced).

## office

| Harness | Lead | `spawn-subagent-explore` | `spawn-subagent-implement` | `spawn-subagent-review` | Unnamed spawn |
|---|---|---|---|---|---|
| Codex (Office Codex) | `gpt-6-sol` / high | `gpt-6-sol` / low | `gpt-6-luna` / xhigh | `gpt-6-sol` / low | `gpt-6-luna` / xhigh |
| Claude Code (Office Claude Code) | `opus` (`claude-opus-5-5`) / high | `opus` (`claude-opus-5-5`) / low | `sonnet` (`claude-sonnet-5`) / xhigh | `opus` (`claude-opus-5-5`) / low | `sonnet` (`claude-sonnet-5`) / session effort |

## personal

| Harness | Lead | `spawn-subagent-explore` | `spawn-subagent-implement` | `spawn-subagent-review` | Unnamed spawn |
|---|---|---|---|---|---|
| Codex (Personal Codex via OpenCode Go) | `deepseek-v4.1-flash` / high | `deepseek-v4.1-flash` / low | `deepseek-v4.1-flash` / xhigh | `deepseek-v4.1-flash` / low | `deepseek-v4.1-flash` / xhigh |

## Claude Code notes

- `config/claude/settings.json` sets the lead (`model`, `effortLevel`), pins each
  alias to its model ID (`ANTHROPIC_DEFAULT_<ALIAS>_MODEL`), sets the unnamed-spawn model
  (`CLAUDE_CODE_SUBAGENT_MODEL`), turns off nesting
  (`CLAUDE_CODE_MAX_SUBAGENT_SPAWN_DEPTH=1`, like Codex `max_depth = 1`),
  and denies the built-in `Explore` so `spawn-subagent-explore` is the only explorer.
- Read-only roles deny `Edit`, `Write`, and `NotebookEdit` but keep `Bash`,
  and `permissionMode` is ignored while the lead runs in auto mode. Read-only
  is enforced by instructions and permission review, not by a sandbox as in Codex.
