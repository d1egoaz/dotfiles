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

Shared Codex configuration lives in `config/codex/config.toml`. Home Manager prepends the selected lead model and appends one profile fragment:

- Office uses ignored `config/codex/profiles/work.local.toml` when present and otherwise warns before using shared config only.
- Personal uses tracked `config/codex/profiles/personal.toml` and treats it as required.

Profile fragments must be empty, comment-only, or begin with a TOML section header. This prevents a fragment's top-level keys from accidentally landing inside the shared file's final section.

The resulting `~/.codex/config.toml` is generated. The activation validates the temporary TOML before replacing it and backs up a pre-existing hand-written target once during migration.

Tracked native roles under `config/codex/agents/` define each role's model, reasoning effort, sandbox, and lead-mediated communication. The shared config defines the unnamed Luna-xhigh fallback and concurrency limits. Changing these files does not retier a running task.

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

Git commits use the active profile's standard OpenSSH signing public key:
`~/.ssh/codex-signing-office-ed25519.pub` for office and
`~/.ssh/codex-signing-personal-ed25519.pub` for personal. The corresponding
encrypted private key is local, untracked key material. GitHub push
authentication is separate from the signing key. macOS already supplies the
native OpenSSH login-session agent and socket. A one-shot login loader restores
the active profile key from macOS Keychain after each reboot or login. After
activation, restart Codex or open a fresh terminal so it stops inheriting the
previous 1Password socket.

On a machine without the selected profile key, bootstrap it from a macOS login
session with `codex-signing-key bootstrap --profile "$PROFILE"`. The helper
refuses to overwrite either expected key path, requires encryption before it
loads the key, and prints a `gh ssh-key add --type signing` command for a user
to review and run separately. It never contacts GitHub itself. For an existing
profile key, including the office signing key, use
`codex-signing-key unlock --profile "$PROFILE"`; it loads the key for 86,400
seconds without a per-signature confirmation, stores the passphrase in macOS
Keychain, adds the public key to local Git verification, and lets eligible
Codex tasks continue while the screen is locked. Future login sessions restore
the key from Keychain without another passphrase prompt. The resulting automatic
machine signature is machine-key provenance, not evidence of per-commit human
review. Preserve the `Assisted-by` footer as the distinct AI-attribution record.

Office repositories under `~/work` retain their HTTPS remotes. The existing
GitHub CLI/macOS Keychain credential-helper path handles unattended publication,
while SSH remains an explicitly selected path and does not affect commit
signing. The existing HTTPS credential has broad `repo` and `workflow` scopes.
Do not expose, copy, or broaden that credential.

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
