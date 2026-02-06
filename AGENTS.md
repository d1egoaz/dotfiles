# AGENTS.md

This file provides guidance to Code agents  when working with code in this repository.

## Repository Overview

This is a macOS dotfiles repository using nix-darwin + Home Manager + Homebrew for system configuration across multiple machines. The setup prioritizes stability (nixpkgs-25.05-darwin) while maintaining a pragmatic hybrid approach where some configs are managed by Nix and others are direct symlinks for immediate editing.

## Core Commands

All commands use the `just` command runner. Working directory should be the repo root.

```bash
# Primary workflow
just switch        # Apply configuration (auto-detects machine by username)
just check         # Validate Nix configuration
just fmt           # Format Nix files
just lint          # Format + check

# Updates and maintenance
just update        # Update flake inputs
just gc            # Garbage collect old generations
just brew          # Update Homebrew packages (includes machine-specific)
just dry-run       # Preview changes without applying

# Initial setup (new machine)
just install-nix
just install-darwin
```

## Architecture

### Username-Based Auto-Detection

The system auto-detects which machine configuration to use based on `whoami`:
- `diego.alvarez` → `office-mbp` (profile: office)
- `diego` → `personal-mbp` (profile: personal)
- `diegoalvarez` → `personal-mini` (profile: personal)

This mapping is defined in:
- `justfile` line 68: `_host` variable
- `nix/flake-modules/darwin.nix`: Host configurations with user mappings

### Profile System

The configuration uses a three-tier profile system:

1. **Base Profile** (`nix/profiles/base.nix`)
   - Packages and Homebrew selections common to ALL machines
   - Exports: `hmPackages`, `systemPackages`, `brewTaps`, `brewCasks`, `brews`

2. **Specific Profiles** (`nix/profiles/{office,personal}.nix`)
   - Machine-specific packages and settings
   - Imports base profile and adds/overrides
   - Profile is passed as `specialArgs` to all Home Manager modules

3. **Per-Host Customization**
   - Host-specific Brewfiles: `Brewfile.{office-mbp,personal-mbp,personal-mini}`
   - Conditional logic using the `profile` variable in modules

### Module Flow

```
flake.nix
  ↓
flake-modules/darwin.nix (defines hosts)
  ↓
lib/mkDarwinSystem.nix (system builder)
  ↓
├─ systems/darwin/default.nix (system-level config)
└─ home-manager/default.nix (user-level config)
     ↓
     ├─ config/programs.nix (Nix-managed programs)
     ├─ config/xdg.nix (file symlinks)
     ├─ config/apps/* (app-specific modules)
     └─ packages.nix (pulls from profiles)
```

### Hybrid Configuration Approach

This repo uses two configuration strategies:

1. **Nix-Managed** (requires `just switch`)
   - Declared in `home-manager/config/programs.nix` or app modules
   - Examples: git, zsh, fish, bat configs
   - Content lives in `/nix/store`

2. **Direct Symlinks** (immediate changes)
   - Declared in `home-manager/config/xdg.nix` using `mkOutOfStoreSymlink`
   - Symlinks point directly to `~/dotfiles/config/<app>/`
   - Examples: sketchybar, wezterm, ghostty, aerospace
   - Edit files directly, no rebuild needed

The `mkOutOfStoreSymlink` function is key: it creates symlinks that point to the repo instead of `/nix/store`, enabling live editing while maintaining declarative management.

### Custom Scripts

Scripts in `bin/files/` are symlinked to `~/.local/bin/` via `home.file` entries in `xdg.nix`. AWS helper functions (awsprofile, aws-sso-automator) are sourced directly by shell configs rather than symlinked.

## Adding New Functionality

### Adding Packages

**User packages** (most common):
- Add to `hmPackages` in `nix/profiles/base.nix` (all machines)
- Or in `nix/profiles/{office,personal}.nix` (specific machines)

**System packages** (rare):
- Add to `systemPackages` in profile files
- These are available system-wide

### Adding Configuration Files

**Option 1: Nix Module** (when Home Manager supports it)
- Add to `home-manager/config/programs.nix` or create new module in `config/apps/`
- Changes require `just switch`
- Example: git, shell configs

**Option 2: Direct Symlink** (for live editing)
1. Create config dir: `config/<app>/`
2. Add symlink in `home-manager/config/xdg.nix`:
   ```nix
   configFile."<app>".source =
     config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/dotfiles/config/<app>";
   ```
3. Edit files directly, changes take effect immediately

### Adding a New Machine

1. Add host configuration in `nix/flake-modules/darwin.nix`:
   ```nix
   new-machine = mkDarwinSystem {
     user = "username";
     profile = "personal"; # or "office"
     inherit system;
   };
   ```

2. Update `justfile` line 68 to add username mapping

3. Optional: Create `Brewfile.new-machine` for machine-specific apps

4. Run: `darwin-rebuild switch --flake ./nix#new-machine`

### Working with Profiles

Access the current profile in any Home Manager module:
```nix
{ profile, ... }:
{
  # Profile is available as specialArg
  home.packages = if profile == "office" then [ pkgs.tool ] else [];
}
```

Profile is also exported as `$PROFILE` environment variable in shell sessions.

## Secrets Management

Secrets are managed with sops-nix:
- Encrypted file: `nix/profiles/office.private.sops.json`
- Edit: `sops nix/profiles/office.private.sops.json`
- GPG key configured in `.sops.yaml`
- Secrets defined in `home-manager/config/secrets.nix`
- Decrypted at runtime to `~/.local/state/sops-nix/secrets/`

## Important Concepts

### Impure Evaluation

The `--impure` flag is used in `just switch` because:
- Profile files may use `builtins.fetchGit` for external resources (fonts repo)
- Office profile may require decrypting sops files at eval time

### Rollbacks

```bash
darwin-rebuild --rollback              # Roll back one generation
darwin-rebuild --list-generations      # List all generations
darwin-rebuild --switch-generation N   # Jump to specific generation
```

### Cachix

The flake uses nix-community cachix for binary caches (configured in `flake.nix` nixConfig).

## Directory Structure Key Locations

```
nix/
├── flake.nix                    # Entry point
├── flake-modules/               # Modular flake config
│   └── darwin.nix              # Host definitions
├── lib/
│   └── mkDarwinSystem.nix      # System builder function
├── profiles/                    # Profile-based configs
│   ├── base.nix                # Shared across all machines
│   ├── office.nix              # Work-specific
│   └── personal.nix            # Personal machines
├── home-manager/               # User environment
│   ├── default.nix             # Entry point
│   ├── packages.nix            # Pulls from profiles
│   └── config/                 # Configuration modules
│       ├── programs.nix        # Nix-managed programs
│       ├── xdg.nix            # File symlinks
│       ├── secrets.nix         # SOPS secrets
│       └── apps/              # App-specific modules
├── systems/darwin/             # macOS system-level settings
└── packages/                   # Custom package definitions

config/                         # Direct config files (symlinked)
bin/files/                      # Custom scripts (symlinked to ~/.local/bin)
Brewfile*                       # Homebrew package definitions
```
