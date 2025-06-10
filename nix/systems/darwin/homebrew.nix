{
  hostCasks ? [ ],
  ...
}:

{
  # ============================================================================
  # Homebrew Configuration
  # ============================================================================

  homebrew = {
    enable = true;
    onActivation = {
      autoUpdate = true; # Update Homebrew itself before installing/upgrading
      upgrade = true; # Upgrade all formulae and casks to latest versions
      cleanup = "zap"; # Remove unmanaged packages and all associated files
      extraFlags = [ "--force " ];
    };

    # Custom taps
    taps = [
      "fastrepl/hyprnote"
    ];

    # Global settings
    global = {
      brewfile = true;
      lockfiles = false;
    };

    # Shared applications for all hosts
    # FAQ: /opt/homebrew/bin/brew uninstall --cask <app> --zap --force
    casks = [
      "1password"
      "OpenSuperWhisper"
      "alfred"
      {
        name = "cursor";
        greedy = true;
      }
      "excalidrawz"
      "hyprnote"
      "iterm2"
      "shottr"
      "tidal"
    ] ++ hostCasks; # Add host-specific casks

    # Mac App Store apps
    masApps = {
      # "App Name" = app_id;
    };
  };
}
