{ pkgs, base }:

# Personal profile: extends `base` with packages/apps used on personal machines.

{
  # ---------------- Homebrew ----------------
  taps = base.taps ++ [ ];

  casks = base.casks ++ [
    "nordvpn" # NordVPN client
    "plex" # Plex media server
  ];

  brews = base.brews ++ [ ];
  masApps = base.masApps // {
    "Emby" = 992180193; # Emby media
    "Infuse" = 1136220934; # Infuse player
    "UHF" = 6443751726; # UHF remote
    "Unifi Protect" = 1392492235; # UniFi Protect
    "Unifi" = 1057750338; # UniFi Network
  };

  # ---------------- Home-Manager packages ----------------
  hmPackages =
    base.hmPackages
    ++ (with pkgs; [
      discord # Discord chat
      google-chrome # Google Chrome browser
      tailscale # WireGuard mesh VPN
      whatsapp-for-mac # WhatsApp desktop
      zoom-us # Zoom meetings
    ]);
}
