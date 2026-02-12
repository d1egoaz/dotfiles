{
  pkgs,
  lib,
  profile,
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
    # Window Management
    # ========================================================================

    # AeroSpace window manager - disabled in favor of manual management
    # Config managed via XDG symlink to ~/dotfiles/config/aerospace/aerospace.toml
    # Service managed via manual launchd agent to use external config
    # aerospace = {
    #   enable = true;
    #   launchd.enable = true;
    # };

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
      enable = false;
      enableZshIntegration = true;
      enableFishIntegration = true;
    };

    eza = {
      enable = true;
      enableZshIntegration = true;
      enableFishIntegration = true;
      colors = "always";
      git = false;
      icons = "always";
      extraOptions = [
        "-lah"
        "--group-directories-first"
      ];
      theme.source =
        let
          repo = pkgs.fetchFromGitHub {
            owner = "eza-community";
            repo = "eza-themes";
            rev = "c03051f67e84110fbae91ab7cbc377b3460f035c";
            sha256 = "1kd60fnwd8vh6jvkyz48rk5bs12a2g7yy6b3qbj7i1p0vwgvnh58";
          };
        in
        "${repo}/themes/tokyonight.yml";
    };

    fd = {
      enable = true;
      hidden = true; # Enables `--hidden`
      extraOptions = [
        "--no-ignore" # Ignores `.gitignore` and similar
      ];
      ignores = [
        ".git/"
        "*.bak"
      ];
    };

    # Environment and navigation
    direnv = {
      enable = true;
      enableZshIntegration = true;
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

    wezterm = {
      enable = true;
      enableZshIntegration = true;
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

    # SSH configuration with 1Password agent integration
    ssh =
      let
        op1PasswordAgent = "\"~/Library/Group Containers/2BUA8C4S2C.com.1password/t/agent.sock\"";
      in
      {
        enable = true;
        enableDefaultConfig = false; # Manually configure defaults in matchBlocks."*"
        matchBlocks =
          # Work GitHub - forces work SSH key
          lib.optionalAttrs (profile == "office") {
            "github.com-work" = {
              hostname = "github.com";
              user = "git";
              identityFile = "~/.ssh/github-work-auth.pub";
              identitiesOnly = true;
              extraOptions.IdentityAgent = op1PasswordAgent;
            };
          }
          // {
            "*".extraOptions = {
              IdentityAgent = op1PasswordAgent;
              AddKeysToAgent = "yes";
            };
          };
      };

    # GPG configuration - DISABLED (no longer needed, using SSH signing)
    # gpg = {
    #   enable = true;
    #   settings = {
    #     auto-key-retrieve = true;
    #     no-emit-version = true;
    #     default-key = "4DF4C58193BBB0863AB37A6DC63945863D4B9E77";
    #     encrypt-to = "4DF4C58193BBB0863AB37A6DC63945863D4B9E77";
    #   };
    # };
  };

  # ============================================================================
  # Services
  # ============================================================================

  # services = {
  #   # GPG Agent service - DISABLED (no longer needed)
  #   # Run once: `gpg --decrypt ~/.authinfo.gpg` to save it in keychain
  #   gpg-agent = {
  #     enable = true;
  #     defaultCacheTtl = 43200;
  #     enableSshSupport = true;
  #     enableZshIntegration = true;
  #     enableFishIntegration = true;
  #     maxCacheTtl = 86400;
  #     # macOS-specific pinentry
  #     pinentry.package = pkgs.pinentry_mac;
  #   };
  # };
}
