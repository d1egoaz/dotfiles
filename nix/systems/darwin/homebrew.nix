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
      autoUpdate = false;
      upgrade = true;
    };

    # Global settings
    global = {
      brewfile = true;
      lockfiles = false;
    };

    # Shared applications for all hosts
    casks = [
      "alfred"
      "1password"
      "excalidrawz"
      "google-chrome"
      "shottr"
      "tidal"
    ] ++ hostCasks; # Add host-specific casks

    # Mac App Store apps
    masApps = {
      # "App Name" = app_id;
    };
  };
}
