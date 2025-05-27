{
  pkgs,
  ...
}:

{
  # Import common configuration
  imports = [ ./home-manager-common.nix ];

  # ============================================================================
  # macOS-specific Configuration
  # ============================================================================

  home = {
    packages = with pkgs; [
      # Security and encryption (macOS)
      pinentry_mac

      # Window management and UI
      aerospace
      jankyborders
      sketchybar

      # Terminal emulator
      wezterm
    ];

    # macOS-specific environment variables
    sessionVariables = {
      # macOS-specific editor configuration
      EMACS_ADDITIONAL_DIR = "$HOME/dotfiles-private/chime";

      # Homebrew settings
      HOMEBREW_NO_ANALYTICS = "1";
      HOMEBREW_NO_ENV_HINTS = "1";
      GPG_TTY = "${builtins.getEnv "TTY"}";

      # Work-related environment variables are set in .zprivate to avoid committing sensitive data
    };
  };

  # ============================================================================
  # macOS-specific XDG Configuration Files
  # ============================================================================

  xdg.configFile = {
    # Terminal emulator
    "wezterm/wezterm.lua".source = ../../../stow/wezterm/.config/wezterm/wezterm.lua;
    # "iterm/config.json".source = ../../../stow/iterm/config.json;  # Uncomment if using iTerm

    # Window management and UI
    "aerospace/aerospace.toml".source = ../../../stow/aerospace/.config/aerospace/aerospace.toml;
    "sketchybar".source = ../../../stow/sketchybar/.config/sketchybar;
    "borders/bordersrc".source = ../../../stow/borders/.config/borders/bordersrc;
  };
}
