{
  pkgs,
  inputs,
  ...
}:

{
  imports = [
    inputs.tokyonight.homeManagerModules.default
    ./programs/zsh.nix
    ./programs/git.nix
    ./programs/fzf.nix
    ./programs/vim.nix
  ];
  # ============================================================================
  # Home Manager Configuration
  # ============================================================================
  # https://nix-community.github.io/home-manager/options.xhtml

  tokyonight = {
    enable = false;
    style = "night"; # Options: "night", "storm", "day", "moon"
  };

  home = {
    stateVersion = "25.05";

    # ========================================================================
    # Packages - Organized by category
    # ========================================================================
    packages = with pkgs; [
      # ======================================================================
      # Development Tools
      # ======================================================================
      cargo # Rust package manager
      gh # GitHub CLI tool
      git # Version control system
      go # Go programming language
      golangci-lint # Go linter
      gopls # Go language server

      # ======================================================================
      # Build and System Tools
      # ======================================================================
      cmake # Cross-platform build system
      comma # Run programs without installing them
      glibtool # GNU libtool
      nix-tree # Visualize Nix package dependencies

      # ======================================================================
      # Terminal and Shell Utilities
      # ======================================================================
      bat # Cat with syntax highlighting and git integration
      btop # Resource monitor (htop alternative)
      direnv # Environment variable manager per directory
      fd # Fast find alternative
      fzf # Fuzzy finder for files and commands
      gum # Glamorous shell scripts
      lsd # ls deluxe with colors and icons
      ripgrep # Fast grep alternative
      starship # Cross-shell prompt
      tmux # Terminal multiplexer
      tree # Directory structure display
      zoxide # Smart cd command

      # ======================================================================
      # System and Network Utilities
      # ======================================================================
      bash # Bourne Again Shell
      coreutils # GNU core utilities
      curl # URL transfer tool
      gnupg # GNU Privacy Guard for encryption
      htop # Process viewer
      jq # JSON processor
      less # Pager program
      socat # Socket CAT
      unzip # Archive extraction tool
      wget # Web file downloader
      xh # Friendly HTTP client
      yq # YAML processor
      zsh # Advanced shell with features

      # ======================================================================
      # Text Processing and Formatting
      # ======================================================================
      delta # Git diff viewer
      graphviz # Graph visualization software
      nixfmt-rfc-style # Nix code formatter
      pandoc # Universal document converter
      prettierd # Prettier daemon
      shellcheck # Shell script analyzer
      shfmt # Shell script formatter
      taplo-cli # TOML toolkit
      yamllint # YAML linter
      yaml-language-server # YAML language server

      # ======================================================================
      # Development and Productivity
      # ======================================================================
      # Custom Emacs with native compilation (direct from inputs)
      inputs.emacs-flake.packages.${pkgs.system}.default
      google-chrome # Web browser

      # ======================================================================
      # Editor Support
      # ======================================================================
      aspell # Spell checker
      aspellDicts.en # English dictionary for aspell

      # ======================================================================
      # Cloud and Infrastructure
      # ======================================================================
      argocd # GitOps continuous delivery tool
      cloudflared # Cloudflare tunnel client
      kubecolor # Colorized kubectl
      kubernetes-helm # Kubernetes package manager

      # ======================================================================
      # Database Tools
      # ======================================================================
      mysql-client # MySQL command-line client
      postgresql # PostgreSQL client tools

      # ======================================================================
      # Media and Graphics
      # ======================================================================
      ffmpeg # Multimedia framework

      # ======================================================================
      # AI and Machine Learning
      # ======================================================================
      ollama # Local AI model runner

      # ======================================================================
      # Fonts
      # ======================================================================
      atkinson-hyperlegible-next # Hyperlegible font family
      nerd-fonts.fira-code # Fira Code with Nerd Font patches
      nerd-fonts.hack # Hack font with Nerd Font patches
      nerd-fonts.jetbrains-mono # JetBrains Mono with Nerd Font patches
    ];

    # ========================================================================
    # Environment Variables
    # ========================================================================
    # /etc/profiles/per-user/$USER/etc/profile.d/hm-session-vars.sh
    sessionVariables = {
      # Editor configuration
      EDITOR = "emacsclient -r -a emacs";
      VISUAL = "emacsclient -r -c -a emacs";
      ALTERNATE_EDITOR = "vim";

      # Locale settings
      LANG = "en_US.UTF-8";
      LC_ALL = "en_US.UTF-8";
      LC_CTYPE = "en_US.UTF-8";

      # Development environment
      GOPATH = "$HOME/go";
      GOPRIVATE = "github.com/1debit/*";

      # Kubernetes
      KUBECONFIG = "$HOME/.kube/config";

      ASPELL_CONF = "dict-dir ${pkgs.aspellDicts.en}/lib/aspell";
    };

    # Additional PATH entries
    sessionPath = [
      "$HOME/.local/bin"

      # Development tools
      # TODO: check disabling these paths
      # "$GOPATH/bin"
      # "$HOME/.cargo/bin"
    ];
  };

  # ============================================================================
  # XDG Configuration Files
  # ============================================================================

  xdg = {
    enable = true;

    configFile = {
      # Shell and terminal
      "starship.toml".source = ../../../stow/starship/.config/starship.toml;
      "tmux/tmux.conf".source = ../../../stow/tmux/.config/tmux/tmux.conf;
    };
  };

  # ============================================================================
  # Home Files (dotfiles in $HOME)
  # ============================================================================

  home.file = {
    # ".zprivate" - Not managed by home-manager, sourced directly in zsh initContent
  };

  # ============================================================================
  # Program Configurations
  # ============================================================================

  programs = {
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

    # Terminal enhancements
    bat = {
      enable = true;
      config = {
        style = "numbers,changes,header";
      };
      tokyonight.enable = true;
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
  };

  services.gpg-agent = {
    enable = true;
    defaultCacheTtl = 43200;
    enableSshSupport = true;
    enableZshIntegration = true;
    maxCacheTtl = 86400;
  };

  services.emacs = {
    # run as daemon
    enable = true;
    package = inputs.emacs-flake.packages.${pkgs.system}.default;
  };
}
