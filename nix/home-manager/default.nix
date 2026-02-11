{
  inputs,
  user,
  ...
}:

{
  # ============================================================================
  # Home Manager Configuration
  # ============================================================================

  imports = [
    # Tokyo Night theme support
    inputs.tokyonight.homeManagerModules.default

    # Application configurations
    ./config/apps

    # Package management - single consolidated file
    ./packages.nix

    # General configuration modules
    ./config/environment.nix
    ./config/xdg.nix
    ./config/programs.nix
  ];

  # ============================================================================
  # Home Manager Settings
  # ============================================================================

  # This value determines the Home Manager release that your
  # configuration is compatible with. You can update this value to
  # reflect your desired release channel, or you can leave it as-is
  # to use the release channel corresponding to your system.
  home.stateVersion = "25.05"; # Please read the comment before changing.

  # The home.username and home.homeDirectory options are used by some
  # programs to determine where to place configuration files.
  home.username = user;
  home.homeDirectory = "/Users/${user}";

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  # ============================================================================
  # Tokyo Night Theme Configuration
  # ============================================================================

  tokyonight = {
    enable = true;
    style = "night"; # Options: "night", "storm", "day", "moon"
  };

  # ============================================================================
  # macOS Per-Host Defaults (uses defaults -currentHost write)
  # ============================================================================

  targets.darwin.currentHostDefaults."com.apple.controlcenter".BatteryShowPercentage = true;
}
