{
  # ============================================================================
  # macOS System Defaults and Preferences
  # ============================================================================

  system.defaults = {
    # Dock configuration
    dock = {
      autohide = true;
      show-recents = false;
      orientation = "left";
      magnification = true;
      show-process-indicators = true;
      largesize = 112;
    };

    # Finder configuration
    finder = {
      AppleShowAllExtensions = true;
      AppleShowAllFiles = true; # Show hidden files in Finder.
      FXEnableExtensionChangeWarning = false;
      FXPreferredViewStyle = "Nlsv";
      FXRemoveOldTrashItems = true; # Automatically delete items from trash after 30 days.
      QuitMenuItem = true;
      ShowExternalHardDrivesOnDesktop = true;
      ShowPathbar = true; # Show the path bar at the bottom of a Finder window.
      ShowRemovableMediaOnDesktop = true;
      ShowStatusBar = true; # Show the status bar at the bottom of a Finder window.
      _FXShowPosixPathInTitle = true;
    };

    # Global system preferences
    NSGlobalDomain = {
      AppleKeyboardUIMode = 3;
      AppleShowScrollBars = "Always";
      AppleShowAllFiles = true; # Always show file extensions in Finder
      InitialKeyRepeat = 15;
      KeyRepeat = 6;
      AppleShowAllExtensions = true;
      ApplePressAndHoldEnabled = false;
      NSAutomaticWindowAnimationsEnabled = false;
      NSAutomaticQuoteSubstitutionEnabled = false;
      NSAutomaticPeriodSubstitutionEnabled = false;
      NSAutomaticCapitalizationEnabled = true;
      AppleInterfaceStyleSwitchesAutomatically = false;
      "_HIHideMenuBar" = true;
    };

    # Input devices
    trackpad = {
      Clicking = true;
    };

    # Custom user preferences
    CustomUserPreferences = {
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
}
