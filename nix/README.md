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
make nix-switch

# Or target specific machines
make nix-office-mbp
make nix-personal-mbp
make nix-personal-mini
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
make nix-fmt
# Check configuration
make nix-check
# Update flake inputs
make nix-update
# Garbage collection
make nix-gc
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
│   │   └── home-manager-common.nix  # Cross-platform home config
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
- **Git Configuration** - Multiple profiles (work/personal)
- **Shell Enhancements** - FZF, zoxide, direnv integration
- **GPG Setup** - Agent configuration with Touch ID support

## Adding Packages

**User packages** (installed in home directory):
Add to `users/diego/home-manager-common.nix` in the appropriate category.

**System packages** (available system-wide):
Add to the respective machine configuration in `machines/`.

## Philosophy

This configuration follows modern Nix best practices:

- **Explicit over implicit** - Clear dependency management
- **Function-based modules** - Clean, reusable components
- **Direct input access** - No complex overlays for simple cases
- **Organized structure** - Logical separation of concerns
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
make nix-gc      # Remove old generations
```
