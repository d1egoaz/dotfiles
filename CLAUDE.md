# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Repository Overview

This repository contains Diego's dotfiles, primarily focused on Nix-based system configuration for macOS. It uses nix-darwin and Home Manager with flakes to manage system configuration.

## Command Reference

### Nix Commands

```bash
# List all available commands
just

# Auto-detect current machine and rebuild (main command)
just nix-switch

# Format nix files
just nix-fmt

# Check nix flake
just nix-check

# Quick format and check
just nix-lint

# Update nix flake
just nix-update

# Garbage collect nix store
just nix-gc

# Quick dry run for office-mbp
just nix-dry-run

# Use nix-home-manager for darwin switch
just nh-switch
```

### Manual Rebuild Commands

```bash
# Target specific machines
darwin-rebuild switch --flake .#office-mbp
darwin-rebuild switch --flake .#personal-mbp
darwin-rebuild switch --flake .#personal-mini

# Rollback changes
darwin-rebuild --rollback
```

### Binary Installation

```bash
# Install binary scripts from bin/files to ~/.local/bin
cd bin && make install

# Remove installed binary scripts
cd bin && make uninstall
```

## Architecture Overview

### Machines

- **office-mbp**: Office MacBook Pro M-chip (user: diego.albeiroalvarezzuluag)
- **personal-mbp**: Personal MacBook Pro M-chip (user: diego)
- **personal-mini**: Personal Mac Mini M-chip (user: diegoalvarez)

### Directory Structure

- **nix/**: Main Nix configuration directory
  - **flake.nix**: Primary entry point for Nix configuration
  - **profiles/**: Machine-specific configuration profiles
    - **base.nix**: Common configuration shared across all machines
    - **office.nix**: Office-specific settings and packages
    - **personal.nix**: Personal machine configurations
  - **home-manager/**: User environment configuration
    - **default.nix**: Main entry point for user environment
    - **packages.nix**: Organized package collections
    - **config/**: Program-specific configuration files
  - **systems/darwin/**: macOS system-level settings
  - **lib/**: Helper functions and utilities
  - **flake-modules/**: Flake configuration modules
  - **overlays/**: Package overrides
  - **packages/**: Custom package definitions
  
- **bin/**: Custom scripts and utilities
  - **files/**: Binary scripts symlinked to ~/.local/bin
  - **makefile**: Installation script for binaries

- **alfred/**: Alfred workflows and configurations
  - **Alfred.alfredpreferences/**: Alfred preferences directory

- **justfile**: Command runner configuration with common tasks

## Workflow

1. Edit configuration files in the `nix/` directory
2. Test changes with `just nix-check` or `just nix-dry-run`
3. Apply changes with `just nix-switch`
4. If needed, roll back with `darwin-rebuild --rollback`

## Troubleshooting

- **Build errors**: Run `nix flake check` to validate configuration or `nix fmt` to fix formatting issues
- **Brew apps not in Application folder**: Run `/opt/homebrew/bin/brew reinstall --cask <app>`
- **Configuration issues**: Ensure new modules are properly imported and check for conflicts