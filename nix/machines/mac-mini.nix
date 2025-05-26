{ pkgs, ... }:

{
  # ============================================================================
  # Mac Mini Machine Configuration
  # ============================================================================

  # Import base macOS configuration
  imports = [
    ./macbook-pro.nix
  ];

  # Mac Mini specific configurations can go here
  # For now, it inherits all settings from MacBook Pro
}
