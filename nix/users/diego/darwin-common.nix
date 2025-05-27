{ pkgs, currentSystemUser, ... }:

{
  # ============================================================================
  # macOS System Configuration
  # ============================================================================

  # Primary user for system defaults
  system.primaryUser = currentSystemUser;

  # System programs
  programs.zsh.enable = true;

  # ============================================================================
  # Homebrew Configuration
  # ============================================================================

  homebrew = {
    enable = true;
    onActivation = {
      autoUpdate = false;
      upgrade = true;
    };

    # Global settings
    global = {
      brewfile = true;
      lockfiles = false;
    };

    # Applications
    casks = [
      "1password"
      "excalidrawz"
      "google-chrome"
      # "hyprnote"
      # "opensuperwhisper"
      "slack"
      "tidal"
      # "cursor"  # TODO: fix, Already installed manually - skip to avoid permission conflicts
    ];

    # Mac App Store apps
    masApps = {
      # Add Mac App Store apps here if needed
      # "App Name" = app_id;
    };
  };

  # ============================================================================
  # System Defaults and Preferences
  # ============================================================================

  system.defaults = {
    # Dock configuration
    dock = {
      autohide = true;
      show-recents = false;
      orientation = "left";
      magnification = true;
      largesize = 112;
    };

    # Finder configuration
    finder = {
      AppleShowAllExtensions = true;
      ShowPathbar = true;
      ShowStatusBar = true;
      # Desktop settings
      ShowExternalHardDrivesOnDesktop = true;
      ShowRemovableMediaOnDesktop = true;
      # Default view style (List view)
      FXPreferredViewStyle = "Nlsv";
    };

    # Global system preferences
    NSGlobalDomain = {
      # Keyboard and interface
      AppleKeyboardUIMode = 3; # Full keyboard access
      AppleShowScrollBars = "Always";

      # Text substitutions
      NSAutomaticQuoteSubstitutionEnabled = false;
      NSAutomaticPeriodSubstitutionEnabled = false;
      NSAutomaticCapitalizationEnabled = true;

      # Interface appearance
      AppleInterfaceStyleSwitchesAutomatically = true;
      "_HIHideMenuBar" = true;

      # Navigation
      AppleEnableSwipeNavigateWithScrolls = false;
    };

    # Input devices
    trackpad = {
      Clicking = true; # Tap to click
    };
  };

  # ============================================================================
  # Security and Input
  # ============================================================================

  # Keyboard settings
  system.keyboard = {
    enableKeyMapping = true;
    remapCapsLockToEscape = true;
  };

  # Enable sudo with Touch ID
  security.pam.services.sudo_local.touchIdAuth = true;
}
