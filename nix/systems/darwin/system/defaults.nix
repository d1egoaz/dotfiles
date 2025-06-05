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
      ShowPathbar = true;
      ShowStatusBar = true;
      ShowExternalHardDrivesOnDesktop = true;
      ShowRemovableMediaOnDesktop = true;
      FXPreferredViewStyle = "Nlsv";
      FXEnableExtensionChangeWarning = false;
      _FXShowPosixPathInTitle = true;
      QuitMenuItem = true;
    };

    # Global system preferences
    NSGlobalDomain = {
      AppleKeyboardUIMode = 3;
      AppleShowScrollBars = "Always";
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
