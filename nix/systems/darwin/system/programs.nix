_:

{
  # ============================================================================
  # System Programs
  # ============================================================================

  programs = {
    zsh = {
      enable = true;
      enableCompletion = false; # Disable here, let home-manager handle it
    };
    gnupg.agent.enable = false; # Disabled - using Home Manager GPG agent instead
    fish.enable = true;
  };
}
