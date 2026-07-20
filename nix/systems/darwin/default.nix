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
    # Control Center menu bar items (requires -currentHost for ByHost plist)
    echo "Configuring Control Center menu bar items..."
    /usr/bin/sudo --user=${user} --set-home -- /usr/bin/defaults -currentHost write com.apple.controlcenter Weather -int 2
    /usr/bin/sudo --user=${user} --set-home -- /usr/bin/defaults -currentHost write com.apple.controlcenter Sound -int 16
    /usr/bin/sudo --user=${user} --set-home -- /usr/bin/defaults -currentHost write com.apple.controlcenter NowPlaying -int 8
    /usr/bin/sudo --user=${user} --set-home -- /usr/bin/killall ControlCenter 2>/dev/null || true
  '';

  # ============================================================================
  # System State
  # ============================================================================

  system.stateVersion = 5;
}
