# ============================================================================
# TL;DR: Just Syntax Reference
# ============================================================================
#
# Basic recipe:               recipe-name:
# With parameters:            recipe-name PARAM:
# Recipe dependencies:        recipe: dep1 dep2
# Private recipe:             _recipe-name:        (hidden from --list)
#
# Commands:
#   @command                  Silent (don't echo command)
#   command                   Normal (shows command being run)
#   #!/usr/bin/env bash       Shebang for multi-line shell scripts
#
# Variables:
#   var := "value"            Assignment
#   var := `command`          Command execution (backticks)
#   {{var}} or {{PARAM}}      Variable/parameter interpolation
#
# Conditionals:
#   var := if condition { "a" } else { "b" }
#
# Examples from this file:
#   _host := if `whoami` == "diego" { "personal" } else { "office" }
#   @echo "Silent message"
#   just _private-recipe {{_host}}
#   lint: nix-fmt nix-check    (runs nix-fmt, then nix-check)

# ============================================================================
# Nix System Management
# ============================================================================

# Default recipe - show available commands
default:
    @just --list

# Garbage collect nix store
nix-gc:
    nix-collect-garbage
    nix-store --gc

# Update nix flake
nix-update:
    cd nix && nix flake update

# Format nix files
nix-fmt:
    cd nix && nix fmt

# Check nix flake
nix-check:
    cd nix && nix flake check

# Quick format and check
lint: nix-fmt nix-check

# ============================================================================
# macOS (nix-darwin) Systems
# ============================================================================

# Auto-detect host based on user (1:1 relationship)
_host := if `whoami` == "diego.albeiroalvarezzuluag" { "office-mbp" } else if `whoami` == "diegoalvarez" { "personal-mini" } else { "personal-mbp" }

# Install nix-darwin (run this first on macOS)
nix-install-darwin:
    @echo "🚀 Installing nix-darwin..."
    @echo "📝 Note: This will prompt for sudo password during system activation"
    sudo nix --extra-experimental-features nix-command --extra-experimental-features flakes run nix-darwin -- switch --flake ./nix#{{_host}}

# Auto-detect current machine and rebuild (main command)
nix-switch:
    @echo "🔍 Auto-detected macOS host: {{_host}}"
    @just _nix-darwin-switch {{_host}}

# Manual override: rebuild specific host
nix-rebuild HOST:
    @just _nix-darwin-switch {{HOST}}

# Quick dry run for office-mbp
nix-dry-run:
    @echo "🧪 Dry run for office-mbp - showing what would change without applying..."
    darwin-rebuild build --flake ./nix#office-mbp --dry-run

# Internal: macOS system rebuild with diff
_nix-darwin-switch HOST:
    #!/usr/bin/env bash
    set -euo pipefail
    echo "💻 Rebuilding Darwin system: {{HOST}}"

    # Build and switch
    darwin-rebuild switch --flake ./nix#{{HOST}}

    echo "✅ Darwin system rebuild complete!"
