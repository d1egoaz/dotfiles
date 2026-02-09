{
  # ============================================================================
  # Nix Configuration
  # ============================================================================

  nix = {
    # Disabled for Determinate Nix - it manages its own daemon
    enable = false;

    # Note: All nix.* options are disabled when enable = false
    # Configure GC, optimization, and settings via Determinate Nix instead
  };
}
