{
  pkgs,
  user,
  hostCasks ? [],
  ...
}:

{
  # ============================================================================
  # Consolidated macOS System Configuration
  # ============================================================================
  # Shared system configuration for all macOS hosts
  # Used by all macOS systems (office-mbp, personal-mbp, personal-mini)

  imports = [
    ./system.nix
    ./darwin-common.nix
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

  # ============================================================================
  # Host-specific Applications
  # ============================================================================

  # Host-specific homebrew applications (passed as parameter)
  homebrew.casks = hostCasks;
}
