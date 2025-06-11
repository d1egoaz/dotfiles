{
  casks ? [ ],
  taps ? [ ],
  masApps ? { },
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
      extraFlags = [ "--force" ]; # Force re-installation of packages
    };

    # Custom taps
    taps = taps;

    # Global settings
    global = {
      brewfile = true;
      lockfiles = false;
    };

    # Shared applications for all hosts
    # FAQ: /opt/homebrew/bin/brew uninstall --cask <app> --zap --force
    casks = casks;

    # Mac App Store apps
    masApps = masApps;
  };
}
