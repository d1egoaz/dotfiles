{
  pkgs,
  ...
}:

{
  # Import common configuration
  imports = [ ./home-manager-common.nix ];

  # ============================================================================
  # Linux-specific Configuration
  # ============================================================================

  home.packages = with pkgs; [
    # Security and encryption (Linux)
    pinentry-gtk2
  ];
}
