{ pkgs, base }:

{
  # ---------------- Homebrew ----------------
  taps = base.taps ++ [ ];

  casks = base.casks ++ [
    "chatgpt" # ChatGPT desktop
    "google-chrome" # Google Chrome browser
    "little-snitch" # Little Snitch firewall
    "nordvpn" # NordVPN client
    "plex" # Plex media server
    "whatsapp" # WhatsApp desktop
    "zoom" # Zoom meetings
  ];

  brews = base.brews ++ [
    "mas" # Mac App Store
  ];

  masApps = base.masApps // {
    "Emby" = 992180193; # Emby media
    "Infuse" = 1136220934; # Infuse player
    "UHF" = 6443751726; # UHF
    "Unifi Protect" = 1392492235; # UniFi Protect
    "Unifi" = 1057750338; # UniFi Network
    "Perplexity" = 6714467650; # Perplexity AI
    "Kindle" = 302584613; # Kindle
    "1Password for Safari" = 1569813296; # 1Password for Safari
  };

  # ---------------- System packages ----------------
  systemPackages =
    base.systemPackages
    ++ (with pkgs; [
    ]);

  # ---------------- Home-Manager packages ----------------
  hmPackages =
    base.hmPackages
    ++ (with pkgs; [
      discord # Discord chat
      tailscale # WireGuard mesh VPN
    ]);
}
