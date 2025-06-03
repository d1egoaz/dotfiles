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
      "zellij/config.kdl".text = ''
        show_startup_tips false
        copy_on_select false
        keybinds {
          normal {
            bind "Ctrl Alt n" { NewPane; }
            bind "Ctrl Alt h" { MoveFocusOrTab "Left"; }
            bind "Ctrl Alt l" { MoveFocusOrTab "Right"; }
            bind "Ctrl Alt j" { MoveFocusOrTab "Down"; }
            bind "Ctrl Alt k" { MoveFocusOrTab "Up"; }
            bind "Ctrl Alt p" { SwitchFocus; }
          }
          pane {
            bind "h" "Left" { MoveFocus "Left"; }
            bind "l" "Right" { MoveFocus "Right"; }
            bind "j" "Down" { MoveFocus "Down"; }
            bind "k" "Up" { MoveFocus "Up"; }
            bind "p" { SwitchFocus; }
          }
          tmux {
            bind "-" { NewPane "Down"; SwitchToMode "Normal"; }
            bind "/" { NewPane "Right"; SwitchToMode "Normal"; }
          }
        }
      '';
    };

    # Data files
    dataFile = {
    };
  };
}
