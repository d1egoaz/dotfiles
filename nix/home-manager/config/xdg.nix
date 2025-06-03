{ ... }:

{
  # ============================================================================
  # XDG Configuration and Directory Management
  # ============================================================================

  xdg = {
    enable = true;
    # Configuration files
    configFile = {
      "sketchybar".source = ../../../stow/sketchybar/.config/sketchybar;
    };

    # Data files
    dataFile = {
    };
  };
}
