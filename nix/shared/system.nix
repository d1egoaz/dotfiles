{ pkgs, ... }:

{
  # ============================================================================
  # Shared macOS System Configuration
  # ============================================================================

  # System packages
  environment.systemPackages = with pkgs; [
    home-manager
  ];

  # ============================================================================
  # Nix Configuration
  # ============================================================================

  nix = {
    enable = true;

    settings = {
      experimental-features = [
        "nix-command"
        "flakes"
      ];
      auto-optimise-store = false;
    };
    gc = {
      automatic = true;
      interval = {
        Weekday = 0;
        Hour = 2;
        Minute = 0;
      };
      options = "--delete-older-than 10d";
    };
  };

  # ============================================================================
  # System Programs
  # ============================================================================

  programs.gnupg.agent = {
    enable = true;
  };

  # ============================================================================
  # System State
  # ============================================================================

  # nix-darwin state version - determines the format of system state files
  # This should match the nix-darwin version you first installed with
  # Version 5 corresponds to nix-darwin releases from ~2023 onwards
  # Only change this if you know what you're doing - see: darwin-rebuild changelog
  system.stateVersion = 5;
}
