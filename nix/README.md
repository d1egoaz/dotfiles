# Diego's Nix Configuration

A clean, modern macOS configuration using nix-darwin and Home Manager with flakes.

## Machines

- **office-mbp**: Office MacBook Pro M-chip (user: diego.albeiroalvarezzuluag)
- **personal-mbp**: Personal MacBook Pro M-chip (user: diego)
- **personal-mini**: Personal Mac Mini M-chip (user: diego)

## Bootstrap

### Prerequisites

Install Nix using the Determinate Systems installer:

```bash
curl --proto '=https' --tlsv1.2 -sSf -L https://install.determinate.systems/nix | sh -s -- install
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
just nix-office-mbp
just nix-personal-mbp
just nix-personal-mini
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
├── flake.nix                    # Main flake configuration
├── treefmt.nix                  # Code formatting config
├── machines/
│   ├── macbook-pro.nix         # MacBook Pro machine config
│   └── mac-mini.nix            # Mac Mini config (inherits from macbook-pro)
├── users/
│   ├── diego/
│   │   ├── darwin.nix          # macOS system config for diego
│   │   ├── darwin-common.nix   # Shared macOS settings
│   │   ├── home-manager-darwin.nix  # macOS-specific home config
│   │   ├── home-manager-common.nix  # Cross-platform home config
│   │   ├── programs/           # Modular program configurations
│   │   │   ├── zsh.nix        # Shell config with aliases and plugins
│   │   │   ├── git.nix        # Git configuration with profiles
│   │   │   ├── fzf.nix        # Fuzzy finder settings
│   │   │   └── vim.nix        # Editor configuration
│   │   └── services/           # Service configurations
│   │       └── aerospace.nix   # Window management (AeroSpace)
│   └── diego.albeiroalvarezzuluag/
│       ├── darwin.nix          # macOS system config for work user
│       └── home-manager-darwin.nix  # Work user home config
└── flakes/
    └── emacs/                  # Custom Emacs configuration
        └── flake.nix
```

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

- **`programs/zsh.nix`** - Complete shell setup including:

  - Extensive alias collection (git, kubernetes, system utilities)
  - Oh My Zsh with plugins (git, kubectl, fzf)
  - Custom initialization and history settings

- **`programs/git.nix`** - Git configuration with:

  - Delta integration for better diffs
  - GPG signing setup
  - Conditional includes for work/personal profiles
  - Global gitignore and file attributes

- **`programs/fzf.nix`** - Fuzzy finder with custom keybindings and colors

- **`programs/vim.nix`** - Editor configuration with essential settings

## Adding Packages

**User packages** (installed in home directory):
Add to `users/diego/home-manager-common.nix` in the appropriate category.

**System packages** (available system-wide):
Add to the respective machine configuration in `machines/`.

**Program configurations**:
Add new modules in `users/diego/programs/` and import them in `home-manager-common.nix`.

## Philosophy

This configuration follows modern Nix best practices:

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
just nix-check  # Validate configuration
just nix-fmt          # Fix formatting issues
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

**Modular configuration issues:**

- Ensure new program modules are imported in `home-manager-common.nix`
- Check for configuration conflicts between modules
- Validate module syntax with `nix flake check`
