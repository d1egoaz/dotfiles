{ config, pkgs, ... }:

{
  # ============================================================================
  # Linux Server Machine Configuration
  # ============================================================================

  # ============================================================================
  # Boot Configuration
  # ============================================================================

  boot.loader = {
    systemd-boot.enable = true;
    efi.canTouchEfiVariables = true;
  };

  # ============================================================================
  # Filesystem Configuration
  # ============================================================================

  fileSystems = {
    # Root filesystem (adjust the device path as needed for your server)
    "/" = {
      device = "/dev/disk/by-label/nixos";
      fsType = "ext4";
    };

    # Boot partition (adjust as needed)
    "/boot" = {
      device = "/dev/disk/by-label/boot";
      fsType = "vfat";
    };
  };

  # ============================================================================
  # Network and Localization
  # ============================================================================

  # Networking
  networking.networkmanager.enable = true;

  # Timezone and locale
  time.timeZone = "America/Vancouver";
  i18n.defaultLocale = "en_US.UTF-8";

  # ============================================================================
  # Services
  # ============================================================================

  services.openssh.enable = true;

  # ============================================================================
  # Nix Configuration
  # ============================================================================

  nix.settings = {
    experimental-features = [
      "nix-command"
      "flakes"
    ];
    auto-optimise-store = true;
  };

  # ============================================================================
  # System Packages and Programs
  # ============================================================================

  # Essential system packages
  environment.systemPackages = with pkgs; [
    vim
    git
    curl
    wget
  ];

  # System programs
  programs.zsh.enable = true;

  # ============================================================================
  # System State
  # ============================================================================

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken.
  system.stateVersion = "25.05";
}
