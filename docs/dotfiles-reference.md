# Dotfiles Reference

This document holds detailed operational reference that is intentionally excluded from the always-loaded root `AGENTS.md`.

## Architecture

The repository uses a hybrid macOS configuration:

- Nix-managed files are declared in Home Manager modules and require `just switch`. Their content lives in the Nix store.
- Direct configuration symlinks use `mkOutOfStoreSymlink` and point into `~/dotfiles/config/`, so edits take effect immediately.
- Homebrew owns packages declared in the shared and host-specific Brewfiles.
- Scripts in `bin/files/` are linked into `~/.local/bin` by `nix/home-manager/config/xdg.nix`.

The main evaluation flow is:

```text
nix/flake.nix
  -> nix/flake-modules/darwin.nix
  -> nix/lib/mkDarwinSystem.nix
  -> nix/systems/darwin/default.nix
  -> nix/home-manager/default.nix
  -> nix/home-manager/config/* and nix/home-manager/packages.nix
```

Profiles are layered as follows:

1. `nix/profiles/base.nix` defines packages shared by all machines.
2. `nix/profiles/office.nix` and `nix/profiles/personal.nix` add profile-specific packages and behavior.
3. `nix/profiles/machines.nix` supplies profile settings such as 1Password account, signing key, work organization, and paths.
4. `Brewfile.<host>` adds host-specific Homebrew packages.

The profile and machine configuration are passed to Home Manager modules as `specialArgs`.

## Codex Configuration And Hooks

Shared Codex configuration lives in `config/codex/config.toml`. The canonical
model routing registry is `nix/data/agent-routing.toml`; the generator renders
profile-specific roles, runtime defaults, and the capability reference. Home
Manager prepends the selected lead route and injects the selected fallback route
before appending one profile fragment:

- Office uses ignored `config/codex/profiles/work.local.toml` when present and otherwise warns before using shared config only.
- Personal uses tracked `config/codex/profiles/personal.toml` and treats it as required.

Profile fragments must be empty, comment-only, or begin with a TOML section header. This prevents a fragment's top-level keys from accidentally landing inside the shared file's final section.

The resulting `~/.codex/config.toml` is generated. The activation validates the temporary TOML before replacing it and backs up a pre-existing hand-written target once during migration.

`bin/files/codex-opencode-go-catalog`, run through `just codex-catalog` on personal machines, generates `~/.codex/ocg-catalog.json` to keep the Codex desktop app and CLI on the personal OpenCode Go subscription (DeepSeek V4.1 Flash). Activation does not run the generator; it only points `model_catalog_json` at the file when it already exists. The app has no profile selector, so the activation injects the root keys for personal machines only: `model`, `model_provider`, and `model_catalog_json`. The provider is declared in `config/codex/profiles/personal.toml` as `opencode-gateway` with `requires_openai_auth = true` and a loopback base URL pointing at the `codex-gateway` service (https://github.com/d1egoaz/codex-gateway), which holds the OpenCode key and forwards to OpenCode Go. Keeping the ChatGPT account baseline is what lets account-gated features such as the Chrome plugin keep working; a provider with its own `env_key` or command-backed auth cannot set that flag, and the browser runtime then fails with `Codex auth token is unavailable`. No API key lives in the repository. The id is `opencode-gateway`, never `opencode-go`: the `codex-relay` service on the personal mini owns `opencode-go` and appends its own `[model_providers.opencode-go]` block with an `env_key` after every rebuild, failing closed when that id is defined without one.

The catalog entry also carries the tool plumbing Codex uses for that model, and the catalog replaces the model list, so entry `visibility` decides what the picker offers. The generator has one table per route: `VISIBLE_MODELS` for ids the gateway forwards to OpenCode Go and `OPENROUTER_VISIBLE_MODELS` for ids it forwards to OpenRouter. Only models that answer the matching Responses wire belong in each table, and everything else stays hidden while keeping metadata for subagent routing. The catalog stays out of the repository because its entries embed the prompt text shipped inside the Codex binary. `just codex-catalog` clones a bundled entry in place of the 5.6-era models: those request a GPT-only tool surface (code-mode-only tools plus responses-lite), and non-GPT models answer such requests by printing their native DSML markup as text instead of calling tools. The recipe then restarts the app-server daemon, because the catalog is read at startup.

Because a model entry carries no provider, the gateway decides the route from the model id: the OpenCode Go ids are listed in its `DEFAULT_OPENCODE_MODELS` and everything else is passed through to ChatGPT with the caller's own credentials. That keeps one picker for both providers, so no switching or config reload is needed.

Generated native roles under `config/codex/agents/generated/<profile>/` define
each role's resolved model, reasoning effort, sandbox, and lead-mediated
communication. Change the registry and run `just agent-routing-generate`; do not
edit generated files. Changing these files does not retier a running task.

Four roles are generated: `spawn-subagent-gather` (read-only),
`spawn-subagent-explore` (read-only), `spawn-subagent-implement`
(workspace-write), and `spawn-subagent-review` (read-only). Each role's tier
and effort live in the registry, so retiering a role never renames it. An
unnamed spawn uses the profile default route (`default_subagent_model`) and
inherits the parent sandbox. Deep judgment stays with the frontier lead. Use
the roles only for native subagents, not visible tasks; spawned instance names
retain the resolved model label.

Role keys are the launcher suffix: `roles.gather` generates
`spawn-subagent-gather`. The top-level `efforts` list is a routing policy, not
a claim about every provider's full supported range.

Profiles with a `claude_runtime` also get the same four roles as Claude Code
subagents in `config/claude/agents/generated/<profile>/*.md`, linked file by
file into `~/.claude/agents/` so hand-made agents stay untouched. Each file sets
the Claude `model` alias and `effort` from the role's tier and effort; read-only
roles deny `Edit`, `Write`, and `NotebookEdit`, and the explorer skips user and
project `CLAUDE.md` (`omitClaudeMd`). `config/claude/settings.json` sets the
unnamed-spawn model (`CLAUDE_CODE_SUBAGENT_MODEL`, the runtime's default tier),
turns off nesting (`CLAUDE_CODE_MAX_SUBAGENT_SPAWN_DEPTH=1`), and denies the
built-in `Explore` agent; the tests keep those values in step with the registry.
The generated `docs/agent-routing-matrix.md` lists every profile, harness, and
role route.

Shared hooks live in `config/codex/hooks.json`; office may select ignored `config/codex/hooks.work.local.json`. Keep hooks out of `config.toml` because Codex loads both representations when both exist.

Supported hook events in this setup are `Stop`, `PermissionRequest`, `UserPromptSubmit`, and `SessionStart`. Hook commands should exit successfully without output or emit valid JSON. Notification payloads use top-level `notify` in `config.toml`, not a `Notification` hook.

Validate configuration using the runbook referenced by `$codex-config-maintenance`. To test the existing notification helper directly, run `/bin/sh -c '$HOME/dotfiles/bin/files/codex-notify.sh "$1"' codex-notify '{"type":"agent-turn-complete"}'`.

## Local And Encrypted State

Work-only Codex profiles, hooks, approvals, and skills are ignored plaintext files backed by encrypted `local_state` entries in `secrets/local-state.yaml`.

- `just switch` restores a missing ignored resource on office machines.
- `just switch` does not update encrypted state when the plaintext file changes.
- `just local-state-sync` encrypts current ignored state.
- `bin/files/dotfiles-local-state check` compares plaintext resources with encrypted state without printing decrypted content.

Do not put work endpoints, project trust entries, internal URLs, or work-only rules into shared tracked files.

## Secrets

Runtime secrets live in 1Password. Concrete workflow `op://` references are encrypted in `secrets/op-env-cache.yaml`, not repeated in public documentation or Nix modules.

Profile settings in `nix/profiles/machines.nix` select the 1Password account and vault. Authenticate interactively with `eval (op signin)` in Fish or `eval $(op signin)` in zsh and Bash, then verify with `op whoami`.

Short-lived GUI or agent workflows may use `op-env-cache` for repeatedly accessed environment variables. Its cache is plaintext on disk until expiry or logout, so do not use it for high-sensitivity or long-lived credentials. Use `op-env-cache logout <name>` to remove a cache.

## Git Commit Signing

Office uses `~/.ssh/codex-signing-office-ed25519.pub`; `personal-mbp` uses
`~/.ssh/codex-signing-personal-ed25519.pub`. The `personal-mini` automation host
uses `~/.ssh/r2claw2-bot.pub` with the R2-Claw2 bot identity. Private keys stay
local. After activation, enroll a passphrase-protected key once with `ssh-add
--apple-use-keychain -t 86400 <private-key>`; macOS restores Keychain-backed SSH
keys at login. The key must already exist and its public key must be registered
in GitHub as a signing key. In the UI, paste the `.pub` file contents beginning
with `ssh-ed25519`, not its `SHA256:` fingerprint or private key.

Office `~/work` keeps HTTPS remotes and uses the GitHub credential helper. Do
not expose or broaden its `repo`/`workflow` credential, or rewrite it to SSH.

## Personal Machine Signing Setup

Use this after the signing configuration is available in `~/dotfiles`:

```zsh
cd ~/dotfiles && just switch
test -e ~/.ssh/codex-signing-personal-ed25519 && echo "Signing key already exists" || ssh-keygen -t ed25519 -a 100 -f ~/.ssh/codex-signing-personal-ed25519 -C "personal-mac-git-signing"
ssh-add --apple-use-keychain -t 86400 ~/.ssh/codex-signing-personal-ed25519
```

The user must enter the non-empty key passphrase. Do not overwrite an existing
key. Register the resulting public key with GitHub as a signing key only:

```zsh
gh ssh-key add ~/.ssh/codex-signing-personal-ed25519.pub --type signing --title "Personal Mac Git signing"
```

That command writes to GitHub and needs explicit authorization. At future
logins, macOS loads all Keychain-backed SSH keys into its native agent.

## Adding Packages And Configuration

- Add a package for all users to `hmPackages` in `nix/profiles/base.nix`.
- Add profile-specific packages to `nix/profiles/office.nix` or `nix/profiles/personal.nix`.
- Use `systemPackages` only when the program must be system-wide.
- Add Homebrew packages to `Brewfile` or the relevant `Brewfile.<host>`.
- Prefer a Home Manager module when it models the application correctly.
- Use an out-of-store symlink for configuration that should update immediately.
- Add new reusable executables to `bin/files/` and the explicit script list in `nix/home-manager/config/xdg.nix`.

## Adding A Machine

1. Add the host in `nix/flake-modules/darwin.nix` with its username and profile.
2. Add the username-to-host mapping in `justfile`.
3. Add profile settings in `nix/profiles/machines.nix` if the existing profile is insufficient.
4. Add `Brewfile.<host>` when host-specific applications are required.
5. Validate before running `darwin-rebuild switch --flake ./nix#<host>`.

## Rollbacks And Nix Behavior

- Keep evaluation pure. Do not add `--impure`.
- Run `darwin-rebuild --rollback` to move back one generation.
- Run `darwin-rebuild --list-generations` to list generations.
- Run `darwin-rebuild --switch-generation N` to select a generation.
- The flake configures nix-community Cachix for binary caches.

## Directory Map

```text
nix/
  flake.nix
  flake-modules/darwin.nix
  lib/mkDarwinSystem.nix
  profiles/{base,office,personal,machines}.nix
  data/agent-routing.toml
  data/agent-routing.generated.nix
  home-manager/
    default.nix
    packages.nix
    config/{programs,xdg}.nix
    config/apps/
  systems/darwin/default.nix
  packages/
config/
  ai/AGENTS.md
  agents/skills/
  codex/
bin/files/
Brewfile*
```
