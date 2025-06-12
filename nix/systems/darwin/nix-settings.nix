{
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
      trusted-users = [ "root" "@admin" ];
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
}
