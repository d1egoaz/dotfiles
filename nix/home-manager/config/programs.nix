{
  pkgs,
  inputs,
  ...
}:

let
  # Import custom packages
  customEmacs = import ../../packages/emacs.nix { inherit pkgs; };
in

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
    };

    # Environment and navigation
    direnv = {
      enable = true;
      nix-direnv.enable = true;
    };

    zoxide = {
      enable = true;
      enableZshIntegration = true;
      options = [
        "--cmd"
        "cd"
      ];
    };

    # Modern terminal workspace
    # https://github.com/zellij-org/zellij/blob/main/zellij-utils/assets/config/default.kdl
    zellij = {
      enable = true;
      enableZshIntegration = false;
      attachExistingSession = false;
      exitShellOnExit = false;
      settings = {
        default_mode = "normal";
        default_layout = "compact";
        simplified_ui = true;
        pane_frames = false;
        theme = "tokyo-night-storm";
        session_serialization = false;
      };
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

    emacs = {
      enable = true;
      package = customEmacs;
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
      maxCacheTtl = 86400;
      # macOS-specific pinentry
      pinentry.package = pkgs.pinentry_mac;
    };
  };
}
