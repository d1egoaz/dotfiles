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
    cd nix && nix flake update --refresh

# Format nix files
fmt:
    cd nix && nix fmt

# Check nix flake
check:
    cd nix && nix flake check --show-trace

# Quick format and check
lint: fmt check

# ============================================================================
# macOS (nix-darwin) Systems
# ============================================================================

# Auto-detect host based on user (1:1 relationship)
_host := if `whoami` == "diego" { "personal-mbp" } else if `whoami` == "diegoalvarez" { "personal-mini" } else { "office-mbp" }

# Install nix-darwin (run this first on macOS)
install-darwin:
    @echo "🚀 Installing nix-darwin..."
    @echo "📝 Note: This will prompt for sudo password during system activation"
    sudo nix --extra-experimental-features nix-command --extra-experimental-features flakes run nix-darwin -- switch --flake ./nix#{{_host}}

# Auto-detect current machine and rebuild (main command)
switch:
    #!/usr/bin/env bash
    set -euo pipefail
    nh darwin switch ./nix#darwinConfigurations.{{_host}} -- --impure

# Quick dry run for the current host
dry-run:
    @echo "🧪 Dry run for {{_host}} - showing what would change without applying..."
    darwin-rebuild build --flake ./nix#{{_host}} --dry-run

darwin-switch:
    @echo "🔍 Switching host {{_host}} with darwin-rebuild"
    darwin-rebuild switch --flake ./nix#{{_host}} --impure

# Install/update from Brewfiles (base + host-specific)
# update and upgrade homebrew packages
brew:
    #!/usr/bin/env bash
    set -euo pipefail
    if [[ "{{_host}}" == "office-mbp" ]]; then
      echo "🏢 Work machine detected ({{_host}}): upgrading Compass..."
      brew upgrade 1debit/chime/compass
    fi

    #!/usr/bin/env bash
    set -euo pipefail
    brew update
    if [[ -f ./Brewfile ]]; then
      echo "Bundling base Brewfile..."
      brew bundle --file ./Brewfile
    fi
    if [[ -f ./Brewfile.{{_host}} ]]; then
      echo "Bundling host Brewfile: Brewfile.{{_host}}..."
      brew bundle --file ./Brewfile.{{_host}}
    fi
    brew upgrade
    # brew reinstall hyprnote
    brew upgrade --cask
    brew cleanup --prune=all
