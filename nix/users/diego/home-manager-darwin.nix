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

  # ============================================================================
  # Terminal Configuration
  # ============================================================================

  programs.wezterm = {
    enable = true;
    # Use your existing Lua configuration
    extraConfig = builtins.readFile ../../../stow/wezterm/.config/wezterm/wezterm.lua;
  };

  xdg.configFile = {
    "sketchybar".source = ../../../stow/sketchybar/.config/sketchybar;
  };
}
