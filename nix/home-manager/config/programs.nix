{
  pkgs,
  inputs,
  ...
}:

{
  # ============================================================================
  # Fonts
  # ============================================================================

  fonts.fontconfig.enable = true;

  # ============================================================================
  # Program Configurations
  # ============================================================================

  programs = {
    # ========================================================================
    # Terminal and Shell Programs
    # ========================================================================

    # Terminal enhancements
    bat = {
      enable = true;
      config = {
        style = "numbers,changes,header";
      };
      extraPackages = with pkgs.bat-extras; [ batman ];
    };

    lsd = {
      enable = true;
      enableZshIntegration = true;
      enableFishIntegration = true;
    };

    # Environment and navigation
    direnv = {
      enable = true;
      nix-direnv.enable = true;
    };

    zoxide = {
      enable = true;
      enableZshIntegration = true;
      enableFishIntegration = true;
      options = [
        "--cmd"
        "cd"
      ];
    };

    # ========================================================================
    # Development Tools
    # ========================================================================

    # GitHub CLI configuration
    gh = {
      enable = true;
      gitCredentialHelper.enable = true;
      settings = {
        version = "1";
        editor = "";
        git_protocol = "https";
        aliases = {
          pc = "pr checkout";
          pv = "pr view";
        };
      };
    };

    # GPG configuration
    gpg = {
      enable = true;
      settings = {
        auto-key-retrieve = true;
        no-emit-version = true;
        default-key = "4DF4C58193BBB0863AB37A6DC63945863D4B9E77";
        encrypt-to = "4DF4C58193BBB0863AB37A6DC63945863D4B9E77";
      };
    };
  };

  # ============================================================================
  # Services
  # ============================================================================

  services = {
    # GPG Agent service
    gpg-agent = {
      enable = true;
      defaultCacheTtl = 43200;
      enableSshSupport = true;
      enableZshIntegration = true;
      enableFishIntegration = true;
      maxCacheTtl = 86400;
      # macOS-specific pinentry
      pinentry.package = pkgs.pinentry_mac;
    };
  };
}
