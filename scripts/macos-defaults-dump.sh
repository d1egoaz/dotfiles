#!/usr/bin/env bash
# Dump macOS defaults for key domains to compare across machines.
# Usage: ./macos-defaults-dump.sh > machine-defaults.txt
# Then diff two machine outputs to find what needs Nix config.

set -euo pipefail

echo "=== Machine: $(scutil --get ComputerName) ==="
echo "=== Date: $(date -Iseconds) ==="
echo "=== User: $(whoami) ==="
echo ""

# Domains that are commonly customized and supported by nix-darwin
DOMAINS=(
  NSGlobalDomain
  com.apple.dock
  com.apple.finder
  com.apple.trackpad
  com.apple.AppleMultitouchTrackpad
  com.apple.screencapture
  com.apple.screensaver
  com.apple.menuextra.clock
  com.apple.WindowManager
  com.apple.spaces
  com.apple.SoftwareUpdate
  com.apple.Safari
  com.apple.desktopservices
  com.apple.LaunchServices
  com.apple.loginwindow
  com.apple.assistant.support
  com.apple.Siri
)

for domain in "${DOMAINS[@]}"; do
  echo "--- ${domain} ---"
  defaults read "${domain}" 2>/dev/null || echo "(empty or not found)"
  echo ""
done

# Keyboard settings
echo "--- Keyboard ---"
defaults read -g InitialKeyRepeat 2>/dev/null || true
defaults read -g KeyRepeat 2>/dev/null || true
defaults read -g ApplePressAndHoldEnabled 2>/dev/null || true

# Accessibility
echo ""
echo "--- Accessibility ---"
defaults read com.apple.Accessibility 2>/dev/null || echo "(empty)"
