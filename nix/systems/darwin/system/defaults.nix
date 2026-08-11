{ lib, ... }:

let
  menuBar = import ../../../lib/macosMenuBar.nix { inherit lib; };
in
{
  # ============================================================================
  # macOS System Defaults and Preferences
  # ============================================================================

  system.defaults = {
    # Dock configuration
    dock = {
      autohide = true;
      autohide-delay = 0.0;
      autohide-time-modifier = 0.0;
      show-recents = false;
      orientation = "left";
      magnification = true;
      show-process-indicators = true;
      largesize = 112;
      tilesize = 64;
      expose-group-apps = true; # Group windows by app in Mission Control

      # Disable hot corners (1 = disabled)
      wvous-tl-corner = 1;
      wvous-tr-corner = 1;
      wvous-bl-corner = 1;
      wvous-br-corner = 1;
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
      AppleEnableSwipeNavigateWithScrolls = false; # Disable two-finger swipe back/forward
      AppleWindowTabbingMode = "manual"; # Do not automatically tab windows
      NSWindowShouldDragOnGesture = true; # Cmd+Ctrl drag to move windows
      NSStatusItemSpacing = 0;
      NSStatusItemSelectionPadding = 0;

      # Expand save and print panels by default
      NSNavPanelExpandedStateForSaveMode = true;
      NSNavPanelExpandedStateForSaveMode2 = true;
      PMPrintingExpandedStateForPrint = true;
      PMPrintingExpandedStateForPrint2 = true;
    }
    // menuBar.systemDefaults.global;

    # Input devices
    trackpad = {
      Clicking = true;
    };

    # Custom user preferences
    CustomUserPreferences = {
      NSGlobalDomain = {
        AppleAccentColor = 5; # Purple accent color
        inherit (menuBar.systemDefaults.globalCustom) AppleMenuBarVisibleInFullscreen;
        SLSMenuBarUseBlurredAppearance = 1; # Show menu bar background
        "com.apple.mouse.scaling" = 1.0; # Mouse tracking speed
        "com.apple.trackpad.scaling" = 1.0; # Trackpad tracking speed
      };
      "com.apple.assistant.support" = {
        "Assistant Enabled" = false;
      };
      "com.apple.Siri" = {
        StatusMenuVisible = false;
        VoiceTriggerUserEnabled = false;
      };
      "com.apple.screencapture" = {
        location = "/private/tmp";
        showsClicks = true;
      };
      "com.apple.WindowManager" = {
        AppWindowGroupingBehavior = true;
        EnableTiledWindowMargins = false; # No margins around tiled windows
        HideDesktop = true; # Don't show desktop items on click
        StageManagerHideWidgets = false;
        StandardHideWidgets = false;
      };
      "com.apple.loginwindow" = {
        TALLogoutSavesState = false; # Don't restore windows on login
      };
      "com.mitchellh.ghostty" = {
        # macOS routes Cmd+H to the native "Hide Ghostty" menu item before
        # Ghostty can handle the keypress, so move hide to Ctrl+Cmd+H to free
        # Cmd+H for Ghostty's split navigation keybind.
        NSUserKeyEquivalents = {
          "Hide Ghostty" = "@^h";
        };
      };
      "com.superultra.Homerow" = {
        "non-search-shortcut" = "⌘;";
        "scroll-shortcut" = "⇧⌘J";
        "label-font-size" = 12;
        "theme-id" = "light";
        "use-search-predicate" = false;
        "launch-at-login" = true;
      };
      # Keep native menu bar behavior consistent across all managed Macs.
      "com.apple.menuextra.clock" = menuBar.systemDefaults.clock;
      "com.apple.controlcenter" = menuBar.systemDefaults.controlCenter;
    };
  };

  system.keyboard = {
    enableKeyMapping = true;
    remapCapsLockToEscape = true;
  };
}
