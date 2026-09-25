# Agent routing matrix

Generated from `nix/data/agent-routing.toml` by `agent-routing-generate`;
do not edit. Each cell is the harness model value and requested effort.

Roles: `spawn-subagent-explore` (read-only, balanced), `spawn-subagent-implement` (workspace-write, economy), `spawn-subagent-review` (read-only, balanced).

## office

| Harness | Lead | `spawn-subagent-explore` | `spawn-subagent-implement` | `spawn-subagent-review` | Unnamed spawn |
|---|---|---|---|---|---|
| Codex (Office Codex) | `gpt-6-sol` / high | `gpt-6-sol` / low | `gpt-6-luna` / xhigh | `gpt-6-sol` / low | `gpt-6-luna` / xhigh |
| Claude Code (Office Claude Code) | `config/claude/settings.json` | `opus` / low | `sonnet` / xhigh | `opus` / low | inherits lead |

## personal

| Harness | Lead | `spawn-subagent-explore` | `spawn-subagent-implement` | `spawn-subagent-review` | Unnamed spawn |
|---|---|---|---|---|---|
| Codex (Personal Codex via OpenCode Go) | `deepseek-v4.1-flash` / high | `deepseek-v4.1-flash` / low | `deepseek-v4.1-flash` / xhigh | `deepseek-v4.1-flash` / low | `deepseek-v4.1-flash` / xhigh |
