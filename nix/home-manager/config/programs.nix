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
  home.packages = with pkgs; [
    atkinson-hyperlegible
  ];

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

    starship.enable = true;
    tmux.enable = true;

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

    # ========================================================================
    # Applications
    # ========================================================================

    # macOS-specific programs
    wezterm = {
      enable = true;
      # Use your existing Lua configuration
      extraConfig = builtins.readFile ../../../stow/wezterm/.config/wezterm/wezterm.lua;
    };
  };

  # ============================================================================
  # Services
  # ============================================================================

  # GPG Agent service
  services.gpg-agent = {
    enable = true;
    defaultCacheTtl = 43200;
    enableSshSupport = true;
    enableZshIntegration = true;
    maxCacheTtl = 86400;
    # macOS-specific pinentry
    pinentry.package = pkgs.pinentry_mac;
  };

  services.emacs = {
    # run as daemon
    enable = true;
    package = customEmacs;
  };
}
