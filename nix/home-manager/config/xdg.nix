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
      "starship.toml".source = ../../../stow/starship/.config/starship.toml;
      "tmux/tmux.conf".source = ../../../stow/tmux/.config/tmux/tmux.conf;
    };

    # Data files
    dataFile = {
    };
  };
}
