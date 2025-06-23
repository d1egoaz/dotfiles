{ pkgs, base }:

{
  # ---------------- Homebrew ----------------
  taps = base.taps ++ [ ];

  casks = base.casks ++ [
    "chatgpt" # ChatGPT desktop
    "google-chrome" # Google Chrome browser
    "nordvpn" # NordVPN client
    "plex" # Plex media server
    "whatsapp" # WhatsApp desktop
    "zoom" # Zoom meetings
  ];

  brews = base.brews ++ [ ];

  masApps = base.masApps // {
    "Emby" = 992180193; # Emby media
    "Infuse" = 1136220934; # Infuse player
    "UHF" = 6443751726; # UHF remote
    "Unifi Protect" = 1392492235; # UniFi Protect
    "Unifi" = 1057750338; # UniFi Network
  };

  # ---------------- System packages ----------------
  systemPackages = base.systemPackages ++ (with pkgs; [
  ]);

  # ---------------- Home-Manager packages ----------------
  hmPackages =
    base.hmPackages
    ++ (with pkgs; [
      discord # Discord chat
      tailscale # WireGuard mesh VPN
    ]);
}
