{
  pkgs,
  user,
  profile,
  systemPackages,
  ...
}:

{
  imports = [
    # Services
    ./services/aerospace.nix

    # System configuration
    (import ./system/defaults.nix { inherit profile; })
    ./system/programs.nix
    (import ./system/security.nix { inherit pkgs; })

    # Nix configuration
    ./nix-settings.nix

    # Darwin-specific packages/apps managed manually via Brewfile (nix-darwin homebrew disabled)
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

  # System packages from profile configuration
  environment.systemPackages = systemPackages;

  system.activationScripts.postActivation.text = ''
    if command -v /opt/homebrew/bin/mas >/dev/null 2>&1; then
    echo "Upgrading Mac App Store apps..."
      /opt/homebrew/bin/mas upgrade --verbose
    fi
  '';

  # ============================================================================
  # System State
  # ============================================================================

  system.stateVersion = 5;
}
