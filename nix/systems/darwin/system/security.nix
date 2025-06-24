{ user, pkgs }: {
  # ============================================================================
  # Security and Input
  # ============================================================================

  security.pam.services.sudo_local.touchIdAuth = true;
  security.sudo.extraConfig = ''
    Defaults timestamp_timeout=30
    ${user} ALL=(ALL) NOPASSWD: ALL
  '';
}
