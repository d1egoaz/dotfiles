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

# Auto-detect and set HOST variable
_host := if `whoami` == "diego.albeiroalvarezzuluag" { "office-mbp" } else if `whoami` == "diegoalvarez" { "personal-mini" } else if `sysctl -n hw.model 2>/dev/null | grep -qi mini || echo ""` != "" { "personal-mini" } else { "personal-mbp" }

# Install nix-darwin (run this first on macOS)
nix-install-darwin:
    @echo "🚀 Installing nix-darwin..."
    @echo "📝 Note: This will prompt for sudo password during system activation"
    sudo nix --extra-experimental-features nix-command --extra-experimental-features flakes run nix-darwin -- switch --flake ./nix#{{_host}}

# Rebuild office MacBook Pro
nix-office-mbp:
    @just _nix-darwin-switch office-mbp

# Rebuild personal MacBook Pro
nix-personal-mbp:
    @just _nix-darwin-switch personal-mbp

# Rebuild personal Mac Mini
nix-personal-mini:
    @just _nix-darwin-switch personal-mini

# Auto-detect current machine and rebuild
nix-switch:
    @echo "🔍 Auto-detected macOS host: {{_host}}"
    @just _nix-darwin-switch {{_host}}

# Internal: macOS system rebuild with diff
_nix-darwin-switch HOST:
    #!/usr/bin/env bash
    set -euo pipefail
    echo "💻 Rebuilding Darwin system: {{HOST}}"

    # Get current generation
    PREV_GEN=$(sudo nix-env -p /nix/var/nix/profiles/system --list-generations | awk '{print $1}' | tail -1)

    # Rebuild system
    cd nix && sudo darwin-rebuild switch --flake .#{{HOST}} --verbose

    # Get new generation and show diff
    NEW_GEN=$(sudo nix-env -p /nix/var/nix/profiles/system --list-generations | awk '{print $1}' | tail -1)

    if [ "$PREV_GEN" != "$NEW_GEN" ]; then
        echo "📦 Changes between generations $PREV_GEN → $NEW_GEN:"
        sudo nix store diff-closures /nix/var/nix/profiles/system-$PREV_GEN-link /nix/var/nix/profiles/system-$NEW_GEN-link
    else
        echo "✅ No new generation created."
    fi
