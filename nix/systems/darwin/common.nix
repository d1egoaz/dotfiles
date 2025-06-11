let
  commonTaps = [ "fastrepl/hyprnote" ];
  commonCasks = [
    "1password"
    "alfred"
    "excalidrawz"
    "gpg-suite"
    "hyprnote"
    "iterm2"
    "OpenSuperWhisper"
    "shottr"
    "tidal"
    "zoom"
    {
      name = "cursor";
      greedy = true;
    }
  ];
  commonBrews = [ ];
  commonMasApps = { };
in
{

  # ============================================================================
  # Taps
  # ============================================================================
  personalTaps = commonTaps ++ [ ];
  officeTaps = commonTaps ++ [ "1debit/chime" ];

  # ============================================================================
  # Casks
  # ============================================================================
  personalCasks = commonCasks ++ [
    "discord"
    "nordvpn"
    "plex"
    {
      name = "google-chrome";
      greedy = true;
    }
  ];
  officeCasks = commonCasks ++ [
    "notion"
    "slack"
  ];

  # ============================================================================
  # Brews
  # ============================================================================
  personalBrews = commonBrews ++ [ ];
  officeBrews = commonBrews ++ [ ];

  # ============================================================================
  # Mac App Store Apps
  # ============================================================================
  personalMasApps = commonMasApps // {
    "Emby" = 992180193;
    "Infuse" = 1136220934;
    "Tailscale" = 1475387142;
    "UHF" = 6443751726;
    "Unifi Protect" = 1392492235;
    "Unifi" = 1057750338;
    "WhatsApp" = 310633997;
  };
  officeMasApps = commonMasApps // { };
}
