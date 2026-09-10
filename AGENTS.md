# Dotfiles Agent Instructions

This repository manages macOS configuration with nix-darwin, Home Manager, and Homebrew across office and personal machines.

## Core Commands

Run commands from the repository root with `just`.

| Command | Purpose |
|---|---|
| `just switch` | Activate the current machine configuration |
| `just check` | Format and validate the full Nix configuration |
| `just fmt` | Format Nix files |
| `just dry-run` | Preview the current host configuration |
| `just audit` | Compare live macOS state with dotfiles, read-only |
| `just local-state-sync` | Encrypt current office-only local state |
| `just update` | Update routine flake inputs |
| `just brew` | Update Homebrew and apply the selected Brewfiles |

`just check` runs the formatter first. Protect unrelated dirty files before using it, and verify afterward that their content did not change.

## Profiles

| Username | Host | Profile | Codex lead |
|---|---|---|---|
| `diego.alvarez` | `office-mbp` | office | Sol xhigh |
| `diego` | `personal-mbp` | personal | Terra xhigh |
| `diegoalvarez` | `personal-mini` | personal | Terra xhigh |

Host selection lives in `justfile` and `nix/flake-modules/darwin.nix`. Per-profile settings live in `nix/profiles/`, and host-specific applications live in `Brewfile.<host>`.

## Sources Of Truth

| Area | Source |
|---|---|
| Global AI instructions | `config/ai/AGENTS.md` |
| Shared Codex config | `config/codex/config.toml` |
| Personal Codex profile | `config/codex/profiles/personal.toml` |
| Office Codex profile | ignored `config/codex/profiles/work.local.toml` |
| Native Codex roles | `config/codex/agents/*.toml` |
| Shared and office hooks | `config/codex/hooks.json`, ignored `config/codex/hooks.work.local.json` |
| Shared skills | `config/agents/skills/multi-agent-team/` |
| Office-only skills | ignored `config/agents/skills/work.local/` |
| Shared approval rules | `config/codex/rules/10-shared.rules` |
| Home Manager links and generated Codex config | `nix/home-manager/config/xdg.nix` |
| Encrypted local state | `secrets/local-state.yaml` |

Edit source files, not generated targets. Home Manager composes `~/.codex/config.toml` from the shared config, the selected profile fragment, and the profile's lead model. Do not overwrite that generated file directly.

Office-only endpoints, trust entries, hooks, approval rules, and skills must stay out of tracked shared files. `just switch` restores missing local state but does not encrypt edits. Use `just local-state-sync` when ignored office state changes.

## Repository Rules

- Keep the primary checkout on `main`; use a linked worktree for feature work.
- Preserve unrelated dirty changes and stage exact files only.
- Do not add `--impure` to Nix commands.
- Homebrew owns packages declared in Brewfiles. Nix owns packages declared in profiles and modules.
- Direct symlinks from `config/` update immediately. Nix-managed files require `just switch`.
- Scripts in `bin/files/` must also be listed in `nix/home-manager/config/xdg.nix` when they should appear under `~/.local/bin`.
- Use `$codex-config-maintenance` for Codex configuration, skill, hook, rule, and AI-instruction changes.
- Use the targeted validators from that skill before `just check`.
- If activation needs sudo or another interactive boundary, report it as incomplete. Never replace generated files to bypass activation.

## On-Demand Reference

Read `docs/dotfiles-reference.md` only when the task needs architecture, configuration layering, hooks, secrets, package ownership, machine bootstrap, rollbacks, or the detailed directory map.
