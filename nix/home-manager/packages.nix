{ pkgs, inputs, ... }:

let
  # Import custom packages
  customEmacs = import ../packages/emacs.nix { inherit pkgs; };
in

{
  home.packages = with pkgs; [
    # ========================================================================
    # Programming Languages and Core Dev Tools
    # ========================================================================
    cargo # Rust package manager
    gh # GitHub CLI tool
    git # Version control system
    go # Go programming language
    golangci-lint # Go linter
    gopls # Go language server

    # ========================================================================
    # Build and System Tools
    # ========================================================================
    cmake # Cross-platform build system
    comma # Run programs without installing them
    just # Command runner (make alternative)
    nix-tree # Visualize Nix package dependencies

    # ========================================================================
    # Text Processing and Formatting
    # ========================================================================
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

    # ========================================================================
    # Terminal and CLI Tools
    # ========================================================================
    bash # Bourne Again Shell
    bat # Better cat with syntax highlighting
    btop # Resource monitor
    coreutils # GNU core utilities
    curl # Command line tool for transferring data
    direnv # Environment variable manager per directory
    fd # Find alternative
    fzf # Fuzzy finder
    gnupg # GNU Privacy Guard for encryption
    gum # Glamorous shell scripts
    jq # JSON processor
    less # Pager
    lsd # ls deluxe with colors and icons
    ripgrep # Fast text search
    socat # Socket CAT
    starship # Cross-shell prompt
    tmux # Terminal multiplexer
    tree # Directory listing
    wget # File downloader
    xh # Friendly HTTP client
    yq # YAML processor
    zellij # Modern terminal workspace
    zoxide # Smart cd command
    zsh # Advanced shell with features

    # ========================================================================
    # Cloud and Infrastructure
    # ========================================================================
    argocd # GitOps continuous delivery tool
    cloudflared # Cloudflare tunnel client
    kubecolor # Colorized kubectl

    # ========================================================================
    # Database Tools
    # ========================================================================
    mysql-client # MySQL command-line client
    postgresql # PostgreSQL client tools

    # ========================================================================
    # AI and Machine Learning
    # ========================================================================
    ollama # Local AI model runner

    # ========================================================================
    # Emacs Editor Support
    # ========================================================================
    glibtool # GNU libtool
    aspell # Spell checker
    aspellDicts.en # English dictionary for aspell

    # ========================================================================
    # Fonts
    # ========================================================================
    atkinson-hyperlegible-next # Hyperlegible font family
    nerd-fonts.fira-code # Fira Code with Nerd Font patches
    nerd-fonts.hack # Hack font with Nerd Font patches
    nerd-fonts.jetbrains-mono # JetBrains Mono with Nerd Font patches

    # ========================================================================
    # macOS-Specific Tools and Window Management
    # ========================================================================
    aerospace # Window manager for macOS
    jankyborders # Window borders for macOS
    pinentry_mac # GPG pinentry for macOS
    sketchybar # Status bar for macOS

    # ========================================================================
    # Custom Applications
    # ========================================================================
    # Custom Emacs with macOS enhancements
    customEmacs
  ];
}
