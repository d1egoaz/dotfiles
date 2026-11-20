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

Shared Codex configuration lives in `config/codex/config.toml`. Codex lead and
default subagent models per profile live inline in
`nix/home-manager/config/xdg.nix`. Home Manager composes these defaults with the
shared config and one profile fragment:

- Office uses ignored `config/codex/profiles/work.local.toml` when present and otherwise warns before using shared config only.
- Personal uses tracked `config/codex/profiles/personal.toml` and treats it as required.

Profile fragments must be empty, comment-only, or begin with a TOML section header. This prevents a fragment's top-level keys from accidentally landing inside the shared file's final section.

The resulting `~/.codex/config.toml` is generated. The activation validates the temporary TOML before replacing it and backs up a pre-existing hand-written target once during migration.

Set Claude models in `config/claude/settings.json`. Office T3 orchestration seats
live in `~/work/agent-skills/chief-of-staff/SKILL.md` (office-only).

Shared hooks live in `config/codex/hooks.json`; office may select ignored `config/codex/hooks.work.local.json`. Keep hooks out of `config.toml` because Codex loads both representations when both exist.

Supported hook events in this setup are `Stop`, `PermissionRequest`, `UserPromptSubmit`, and `SessionStart`. Hook commands should exit successfully without output or emit valid JSON. Notification payloads use top-level `notify` in `config.toml`, not a `Notification` hook.

Validate configuration using the runbook referenced by `$codex-config-maintenance`.

## T3 Code Themes

`~/.t3/userdata/themes` links to `config/t3/themes`. Publish color changes through T3 (`t3_theme_set`), which writes `<themeId>.json` there.

T3 has no global stylesheet, so custom CSS lives in each theme's `css` field. `config/t3/features.css` is the source of truth for that CSS: approval buttons, the sidebar Settle button, and the background-work card under the thread details panel. Edit it, run `just t3-theme-features` to copy it into every theme (or `just t3-theme-features <themeId>`), then switch to another theme and back in T3 to reload. `just check` fails when a theme's `css` drifts from the file. To port the features to a new theme, add its JSON to `config/t3/themes` and run the recipe.

`tokyo-night-plus.json` ("Tokyo Night Plus") is the only theme in use. It is based on [SunkenInTime/t3-themes `themes/tokyo-night.json`](https://github.com/SunkenInTime/t3-themes/blob/main/themes/tokyo-night.json), imported at upstream commit `57a681245f98e47542bf6016f4e6d005cf7a9a03`. Local changes:

- The `css` field comes from `config/t3/features.css`.
- The light variant's `secondary` is `#dadce3` (upstream `#707280`) and `secondaryForeground` is `#343b59` (upstream `#ffffff`). The upstream gray rendered the changed-files card as a dark slab.
- The light variant's `surfaceOverlay` is `#eff0f4` (upstream `#ffffff`), so popovers and the thread details card sit one soft step above the canvas, as they do in dark.

To compare with upstream, fetch the raw file with `gh api repos/SunkenInTime/t3-themes/contents/themes/tokyo-night.json -H "Accept: application/vnd.github.raw"`, then diff its `colors` and `variants` with `jq -S`.

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
--apple-use-keychain <private-key>`; the login loader restores the active
profile key from Keychain without a fixed expiry. The key must already exist,
and its public key must be registered in GitHub as a signing key. In the UI,
paste the `.pub` file contents beginning with `ssh-ed25519`, not its `SHA256:`
fingerprint or private key.

Office `~/work` keeps HTTPS remotes and uses the GitHub credential helper. Do
not expose or broaden its `repo`/`workflow` credential, or rewrite it to SSH.

## Personal Machine Signing Setup

Use this after the signing configuration is available in `~/dotfiles`:

```zsh
cd ~/dotfiles && just switch
test -e ~/.ssh/codex-signing-personal-ed25519 && echo "Signing key already exists" || ssh-keygen -t ed25519 -a 100 -f ~/.ssh/codex-signing-personal-ed25519 -C "personal-mac-git-signing"
ssh-add --apple-use-keychain ~/.ssh/codex-signing-personal-ed25519
```

The user must enter the non-empty key passphrase. Do not overwrite an existing
key. Register the resulting public key with GitHub as a signing key only:

```zsh
gh ssh-key add ~/.ssh/codex-signing-personal-ed25519.pub --type signing --title "Personal Mac Git signing"
```

That command writes to GitHub and needs explicit authorization. At future
logins, the login loader adds the active profile key to the native agent from
Keychain.

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
