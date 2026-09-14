{
  config,
  pkgs,
  profile,
  ...
}:

let
  signingPrivateKey = "${config.home.homeDirectory}/.ssh/codex-signing-${profile}-ed25519";
in
{
  # macOS already starts com.openssh.ssh-agent with a login-session
  # SSH_AUTH_SOCK. This one-shot loader restores only the active profile key
  # from macOS Keychain, rather than replacing the platform agent.
  launchd.agents.codexSigningKeychainLoader = {
    enable = true;
    config = {
      ProgramArguments = [
        "/usr/bin/ssh-add"
        "--apple-load-keychain"
        "-t"
        "86400"
        signingPrivateKey
      ];
      RunAtLoad = true;
      ProcessType = "Interactive";
    };
  };

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
          # https://github.com/eza-community/eza-themes
          repo = pkgs.fetchFromGitHub {
            owner = "eza-community";
            repo = "eza-themes";
            rev = "c03051f67e84110fbae91ab7cbc377b3460f035c"; # 2025-01
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

    # Explicit SSH remotes may use the 1Password agent. Office Git remotes are
    # no longer rewritten from HTTPS to SSH, so unattended Chime publication
    # uses the existing HTTPS credential-helper path instead.
    ssh =
      let
        op1PasswordAgent = "\"~/Library/Group Containers/2BUA8C4S2C.com.1password/t/agent.sock\"";
      in
      {
        enable = true;
        enableDefaultConfig = false; # Manually configure defaults in matchBlocks."*"
        matchBlocks = {
          "*".extraOptions = {
            IdentityAgent = op1PasswordAgent;
            AddKeysToAgent = "yes";
          };
        };
      };
  };
}
