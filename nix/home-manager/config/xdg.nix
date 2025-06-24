{ ... }:

{
  # ============================================================================
  # XDG Configuration and Directory Management
  # ============================================================================

  xdg = {
    enable = true;
    # Configuration files
    configFile = {
      "sketchybar".source = ./apps/sketchybar;
    };

    # Data files
    dataFile = {
    };
  };
}
