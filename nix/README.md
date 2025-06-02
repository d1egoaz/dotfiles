# Diego's Nix Configuration

A clean, modern macOS configuration using nix-darwin and Home Manager with flakes.

## Machines

- **office-mbp**: Office MacBook Pro M-chip (user: diego.albeiroalvarezzuluag)
- **personal-mbp**: Personal MacBook Pro M-chip (user: diego)
- **personal-mini**: Personal Mac Mini M-chip (user: diegoalvarez)

## Bootstrap

### Prerequisites

Install Nix using the Determinate Systems installer:

```bash
curl --proto '=https' --tlsv1.2 -sSf -L https://install.determinate.systems/nix | sh -s -- install
```

Install just command runner:

```bash
# On macOS with Homebrew
brew install just

# Or with cargo
cargo install just
```

### Initial Setup

Bootstrap machine by running:

**Office MacBook Pro:**

```bash
nix run nix-darwin -- switch --flake github:d1egoaz/dotfiles#office-mbp
```

**Personal MacBook Pro:**

```bash
nix run nix-darwin -- switch --flake github:d1egoaz/dotfiles#personal-mbp
```

**Personal Mac Mini:**

```bash
nix run nix-darwin -- switch --flake github:d1egoaz/dotfiles#personal-mini
```

### Apply Changes Locally

```bash
# Auto-detect current machine and rebuild
just nix-switch

# Or target specific machines
just nix-rebuild office-mbp
just nix-rebuild personal-mbp
just nix-rebuild personal-mini
```

**Manual rebuild:**

```bash
darwin-rebuild switch --flake .#office-mbp
# or
darwin-rebuild switch --flake .#personal-mbp
# or
darwin-rebuild switch --flake .#personal-mini
```

### Development Commands

```bash
# Format code
just nix-fmt
# Check configuration
just nix-check
# Update flake inputs
just nix-update
# Garbage collection
just nix-gc
```

### Available Commands

List all available recipes:

```bash
just --list
# or simply
just
```

## Configuration Structure

```
nix/
├── flake.nix                    # Main flake configuration with host parameters
├── treefmt.nix                  # Code formatting config
├── shared/                      # Consolidated shared configurations
│   ├── macos-home.nix          # Complete Home Manager configuration
│   ├── macos-system.nix        # Complete system configuration
│   ├── system.nix              # Common system settings
│   ├── darwin-common.nix       # Shared macOS settings
│   ├── programs/               # Modular program configurations
│   │   ├── zsh.nix            # Shell config with aliases and plugins
│   │   ├── git.nix            # Git configuration with profiles
│   │   ├── fzf.nix            # Fuzzy finder settings
│   │   └── vim.nix            # Editor configuration
│   └── services/               # Service configurations
│       └── aerospace.nix       # Window management (AeroSpace)
└── flakes/
    └── emacs/                  # Custom Emacs configuration
        └── flake.nix
```

## Architecture

This configuration uses a **consolidated architecture** that eliminates code duplication while maintaining host-specific flexibility:

### Single Source of Truth

- **`shared/macos-home.nix`** - Complete Home Manager configuration for all macOS hosts
- **`shared/macos-system.nix`** - Complete system configuration for all macOS hosts
- **`flake.nix`** - Host definitions with parameterized differences

### Host Differentiation

Host-specific differences are handled through parameters in `flake.nix`:

- **Usernames** - Different users per host
- **Homebrew Casks** - Host-specific GUI applications:
  - `office-mbp`: `slack`
  - `personal-mbp`: `discord`
  - `personal-mini`: `discord`, `notion`

### Benefits

- **Zero Duplication** - Each configuration line exists only once
- **Single Maintenance Point** - Changes apply to all hosts automatically
- **Type Safety** - Nix ensures consistency across hosts
- **Parametric Flexibility** - Easy to add host-specific differences

## Key Features

### Package Management

- **Organized by Category** - Development, terminal, system, cloud tools, etc.
- **Modern CLI Tools** - bat, btop, fd, fzf, ripgrep, starship, and more
- **Development Environment** - Go, Rust, Git ecosystem, formatters, linters

### System Configuration

- **Window Management** - AeroSpace + Sketchybar with custom keybindings
- **Shell Setup** - Zsh with plugins, starship prompt, and modern tools
- **macOS Optimizations** - Dock, Finder, keyboard settings
- **Homebrew Integration** - For GUI applications

### User Environment

- **Custom Emacs** - Built with native compilation and patches
- **Git Configuration** - Multiple profiles (work/personal) with conditional includes
- **Shell Enhancements** - FZF, zoxide, direnv integration
- **GPG Setup** - Agent configuration with Touch ID support

## Modular Program Configuration

Program configurations are organized in dedicated modules for maintainability:

- **`shared/programs/zsh.nix`** - Complete shell setup including:

  - Extensive alias collection (git, kubernetes, system utilities)
  - Oh My Zsh with plugins (git, kubectl, fzf)
  - Custom initialization and history settings

- **`shared/programs/git.nix`** - Git configuration with:

  - Delta integration for better diffs
  - GPG signing setup
  - Conditional includes for work/personal profiles
  - Global gitignore and file attributes

- **`shared/programs/fzf.nix`** - Fuzzy finder with custom keybindings and colors

- **`shared/programs/vim.nix`** - Editor configuration with essential settings

## Adding Configurations

### Packages

**User packages** (installed in home directory):
Add to `shared/macos-home.nix` in the appropriate category.

**System packages** (available system-wide):
Add to `shared/system.nix`.

### Program Configurations

Add new modules in `shared/programs/` and import them in `shared/macos-home.nix`.

### Host-Specific Configurations

**For new host-specific parameters:**

1. Add parameter to `mkSystem` function in `flake.nix`
2. Use parameter in `shared/macos-system.nix` or `shared/macos-home.nix`
3. Set parameter value for each host in `darwinConfigurations`

**For host-specific GUI applications:**
Add to the `hostCasks` array for the specific host in `flake.nix`.

### Adding New Hosts

```nix
# In flake.nix darwinConfigurations
new-host = mkSystem {
  system = "aarch64-darwin";
  user = "newuser";
  host = "new-host";
  hostCasks = [ "app1" "app2" ];
};
```

## Philosophy

This configuration follows modern Nix best practices:

- **Consolidated architecture** - Eliminate duplication through shared configurations
- **Parameterized differences** - Handle variations through explicit parameters
- **Single source of truth** - Each configuration exists in exactly one place
- **Explicit over implicit** - Clear dependency management
- **Function-based modules** - Clean, reusable components
- **Direct input access** - No complex overlays for simple cases
- **Organized structure** - Logical separation of concerns
- **Modular design** - Program configs split into focused files
- **Declarative configuration** - Pure Nix without external file dependencies
- **Consistent formatting** - Automated code styling

## Troubleshooting

**Build errors:**

```bash
nix flake check  # Validate configuration
nix fmt          # Fix formatting issues
```

**Rollback changes:**

```bash
darwin-rebuild --rollback
```

**Clean up:**

```bash
just nix-gc      # Remove old generations
```

**Brew apps not in the Application folder:**

```bash
PATH="/opt/homebrew/bin:$PATH" brew reinstall --cask <app>
```

**Configuration issues:**

- Ensure new program modules are imported in `shared/macos-home.nix`
- Check for configuration conflicts between modules
- Validate module syntax with `nix flake check`
- For host-specific issues, verify parameters in `flake.nix`
