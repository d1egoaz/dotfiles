{
  pkgs,
  user,
  hostCasks ? [ ],
  ...
}:

{
  imports = [
    # Services
    ./services/aerospace.nix
    ./services/jankyborders.nix

    # System configuration
    ./system/defaults.nix
    ./system/programs.nix
    ./system/security.nix

    # Nix configuration
    ./nix-settings.nix

    # Darwin-specific packages/apps
    (import ./homebrew.nix { inherit hostCasks; })
  ];

  # ============================================================================
  # System Configuration
  # ============================================================================

  # Set primary user for this configuration
  system.primaryUser = user;

  # The user should already exist, but we need to set this up so Nix knows
  # what our home directory is (https://github.com/LnL7/nix-darwin/issues/423).
  users.users.${user} = {
    home = "/Users/${user}";
    shell = pkgs.zsh;
  };

  # System packages
  environment.systemPackages = with pkgs; [
    home-manager
  ];

  # ============================================================================
  # System State
  # ============================================================================

  system.stateVersion = 5;
}
