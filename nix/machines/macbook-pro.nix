{ pkgs, ... }:

{
  # ============================================================================
  # MacBook Pro Machine Configuration
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
  };

  # ============================================================================
  # System Programs
  # ============================================================================

  # programs = {
  #   gnupg.agent.enable = true;
  # };
  programs.gnupg = {
    # enable = true;

    agent = {
      enable = true; # turn on the gpg-agent
      # pinentryPackage = pkgs.pinentry_mac;

      # pinentryFlavor = "mac";     # use macOS pinentry
      # extraConfig    = ''         # all your gpg.conf settings
      #   # cache timeouts
      #   default-cache-ttl 600
      #   max-cache-ttl     7200

      #   # your custom settings
      #   auto-key-retrieve
      #   no-emit-version
      #   default-key 4DF4C58193BBB0863AB37A6DC63945863D4B9E77
      #   encrypt-to   4DF4C58193BBB0863AB37A6DC63945863D4B9E77
      #   # pinentry-mode loopback
      # '';
    };
  };
  # services.gpg-agent = {
  #   enable = true;
  #   pinentryPackage = pkgs.pinentry_mac;
  # };

  # services.gpg-agent = {
  #   enable = true;
  #   pinentryPackage = pkgs.pinentry_mac;
  # };

  # ============================================================================
  # System State
  # ============================================================================

  # nix-darwin state version - determines the format of system state files
  # This should match the nix-darwin version you first installed with
  # Version 5 corresponds to nix-darwin releases from ~2023 onwards
  # Only change this if you know what you're doing - see: darwin-rebuild changelog
  system.stateVersion = 5;
}
