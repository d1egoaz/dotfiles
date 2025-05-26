{ pkgs, ... }:

{
  # ============================================================================
  # Linux System Configuration (Ubuntu Server with Nix)
  # ============================================================================

  # Note: This configuration is for Linux systems (like Ubuntu Server)
  # that have Nix package manager installed, NOT for NixOS systems.
  #
  # Since this is Ubuntu Server + Nix (not NixOS), we don't configure:
  # - User accounts (handled by Ubuntu)
  # - System services (handled by Ubuntu)
  # - Boot configuration (handled by Ubuntu)
  # - Network configuration (handled by Ubuntu)
  #
  # This file exists mainly for the flake structure and potential
  # future migration to NixOS.

  # Empty configuration - Ubuntu handles all system configuration
}
