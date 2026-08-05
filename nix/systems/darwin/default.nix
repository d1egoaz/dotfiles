{
  pkgs,
  user,
  systemPackages,
  ...
}:

{
  imports = [
    # Services
    ./services/aerospace.nix

    # System configuration
    ./system/defaults.nix
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
    # Home Manager writes the per-host Control Center defaults. Restart the
    # process so visibility changes take effect during the same switch.
    /usr/bin/sudo --user=${user} --set-home -- /usr/bin/killall ControlCenter 2>/dev/null || true
  '';

  # ============================================================================
  # System State
  # ============================================================================

  system.stateVersion = 5;
}
