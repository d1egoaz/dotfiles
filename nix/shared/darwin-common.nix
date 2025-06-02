{ pkgs, ... }:

{
  imports = [
    ./services/aerospace.nix
  ];

  # ============================================================================
  # macOS System Configuration
  # ============================================================================

  # Primary user is set by the individual user config files
  # No need for currentSystemUser parameter

  # System programs
  programs.zsh.enable = true;

  services = {
    jankyborders = {
      enable = true;
      active_color = "0xaaff00ff";
      hidpi = true;
      inactive_color = "0xff414550";
      style = "round";
      width = 6.0;
    };
  };

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
      "alfred"
      "1password"
      "excalidrawz"
      "google-chrome"
      # "hyprnote"
      # "opensuperwhisper"
      "shottr"
      "tidal"
      # "cursor"  # TODO: fix, Already installed manually - skip to avoid permission conflicts
    ];

    # Mac App Store apps
    # These app IDs are from using the mas CLI app or browsing the Store
    # https://github.com/mas-cli/mas
    #
    # $ nix shell nixpkgs#mas
    # $ mas search <app name>
    masApps = {
      # "App Name" = app_id;
    };
  };

  fonts.packages = [
    pkgs.atkinson-hyperlegible
  ];

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
      AppleShowAllExtensions = true; # show all file extensions
      ShowPathbar = true;
      ShowStatusBar = true;
      ShowExternalHardDrivesOnDesktop = true;
      ShowRemovableMediaOnDesktop = true;
      FXPreferredViewStyle = "Nlsv"; # Default view style (List view)
      FXEnableExtensionChangeWarning = false; # disable warning when changing file extension
      _FXShowPosixPathInTitle = true; # show full path in finder title
      QuitMenuItem = true; # enable quit menu item
    };

    # Global system preferences
    NSGlobalDomain = {
      # Enables full keyboard access to all controls (not just text fields and lists).
      AppleKeyboardUIMode = 3;
      AppleShowScrollBars = "Always";

      # Controls the delay before a key starts repeating when held down.
      # 120, 94, 68 (default), 35, 25, 15
      InitialKeyRepeat = 15;
      # 120, 90, 60, 30, 12, 6 (default), 2
      KeyRepeat = 6;
      AppleShowAllExtensions = true;
      # Disables the press-and-hold pop-up for accented characters
      ApplePressAndHoldEnabled = false;

      # Disables window opening/closing animations
      NSAutomaticWindowAnimationsEnabled = false;

      # Text substitutions
      # Disables smart quotes " " instead of " "
      NSAutomaticQuoteSubstitutionEnabled = false;
      # Disables automatic insertion of a period when you double-space
      NSAutomaticPeriodSubstitutionEnabled = false;
      # Enables auto-capitalization at the beginning of sentences
      NSAutomaticCapitalizationEnabled = true;

      # Interface appearance
      # Enables automatic switching between light and dark mode based on time of day.
      AppleInterfaceStyleSwitchesAutomatically = false;
      # Hides the menu bar when not in use
      "_HIHideMenuBar" = true;
    };

    # Input devices
    trackpad = {
      Clicking = true; # Tap to click
    };

    # Custom user preferences for settings not directly supported
    CustomUserPreferences = {
      # Siri configuration - disable Siri completely
      "com.apple.assistant.support" = {
        "Assistant Enabled" = false;
      };
      "com.apple.Siri" = {
        StatusMenuVisible = false;
        VoiceTriggerUserEnabled = false;
      };
    };
  };

  system.keyboard = {
    enableKeyMapping = true;
    remapCapsLockToEscape = true;
  };

  # ============================================================================
  # Security and Input
  # ============================================================================

  # Enable sudo with Touch ID
  security.pam.services.sudo_local.touchIdAuth = true;
  security.sudo.extraConfig = ''
    Defaults timestamp_timeout=30
  '';
}
