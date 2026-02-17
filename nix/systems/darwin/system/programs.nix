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
    fish.enable = true;
  };
}
