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
#   lint: fmt check    (runs fmt, then check)
# ============================================================================
# Nix System Management
# ============================================================================

# Default recipe - show available commands
default:
    @just --list

# Install Nix package manager on a new machine
install-nix:
    @echo "🚀 Installing Nix package manager..."
    curl --proto '=https' --tlsv1.2 -sSf -L https://install.determinate.systems/nix | sh -s -- install

# Garbage collect nix store
gc:
    nix-collect-garbage
    nix-store --gc
    echo "✅ Garbage collection completed successfully!"

# Update nix flake
update:
    nix flake update --flake ./nix --refresh

# Clear Nix tarball cache (fixes "Truncated tar archive" errors during flake update)
fix-nix-cache:
    rm -rf ~/.cache/nix/tarball
    @echo "✅ Nix tarball cache cleared. Re-run: just update"

# Format nix files
fmt:
    nix run ./nix#formatter.aarch64-darwin -- -C nix

# Check nix flake
check: fmt
    nix flake check ./nix --show-trace

# Quick format and check
lint: check

# ============================================================================
# macOS (nix-darwin) Systems
# ============================================================================
# Auto-detect host based on user (1:1 relationship)

[private]
_host := if `whoami` == "diego" { "personal-mbp" } else if `whoami` == "diegoalvarez" { "personal-mini" } else if `whoami` == "diego.alvarez" { "office-mbp" } else { error("Unknown user: run `whoami` and add to justfile") }

# Install nix-darwin (run this first on macOS)
install-darwin:
    @echo "🚀 Installing nix-darwin..."
    @echo "📝 Note: This will prompt for sudo password during system activation"
    sudo nix --extra-experimental-features nix-command --extra-experimental-features flakes run nix-darwin -- switch --flake ./nix#{{ _host }}

# Auto-detect current machine and rebuild (main command)
switch:
    nh darwin switch ./nix#darwinConfigurations.{{ _host }} -- --impure

# Quick dry run for the current host
dry-run:
    @echo "🧪 Dry run for {{ _host }} - showing what would change without applying..."
    darwin-rebuild build --flake ./nix#{{ _host }} --dry-run

darwin-switch: switch

# Install/update from Brewfiles (base + host-specific)

# update and upgrade homebrew packages
brew:
    #!/usr/bin/env bash
    set -euo pipefail
    if [[ "{{ _host }}" == "office-mbp" ]]; then
      if [[ -z "${OP_ACCOUNT:-}" || -z "${OP_VAULT:-}" ]]; then
        echo "❌ OP_ACCOUNT/OP_VAULT not set. Run 'just switch' first to export them."
        exit 1
      fi
      echo "🏢 Work machine detected: setting up Homebrew token from 1Password..."
      export HOMEBREW_GITHUB_API_TOKEN=$(op read "op://$OP_VAULT/Homebrew GitHub Token/credential" --account "$OP_ACCOUNT")
      echo "🏢 Upgrading Compass..."
      brew upgrade 1debit/chime/compass
    fi
    brew update
    if [[ -f ./Brewfile ]]; then
      echo "Bundling base Brewfile..."
      brew bundle --file ./Brewfile
    fi
    if [[ -f ./Brewfile.{{ _host }} ]]; then
      echo "Bundling host Brewfile: Brewfile.{{ _host }}..."
      brew bundle --file ./Brewfile.{{ _host }}
    fi
    brew cleanup --prune=all
