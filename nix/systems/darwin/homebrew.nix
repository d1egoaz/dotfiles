{
  taps,
  casks,
  brews,
  masApps,
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

    # Global settings
    global = {
      brewfile = true;
      lockfiles = false;
    };

    # Custom taps, brews, casks, and Mac App Store apps
    inherit
      taps
      casks
      brews
      masApps
      ;
    # FAQ: /opt/homebrew/bin/brew uninstall --cask <app> --zap --force
  };
}
