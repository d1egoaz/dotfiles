{ pkgs }:

# Base profile: packages and Homebrew selections common to ALL machines.
# Consumers may extend this via `personal.nix`, `office.nix`, etc.

let
  # ---------------- Custom Packages ----------------
  customEmacs = import ../packages/emacs.nix { inherit pkgs; };

  # ---------------- Homebrew ----------------
  taps = [
    "fastrepl/hyprnote" # hyprnote tap for fastrepl
  ];

  casks = [
    "1password" # 1Password
    "alfred" # Alfred launcher
    "excalidrawz" # ExcalidrawZ whiteboard
    "gpg-suite" # GPG Suite
    "hyprnote" # Hyprnote markdown notes
    "iterm2" # iTerm2 terminal
    "OpenSuperWhisper" # OpenSuperWhisper
    "shottr" # Screenshot tool
    "tidal" # TIDAL desktop client
    # The Cursor IDE (needs greedy install)
    {
      name = "cursor";
      greedy = true;
    }
  ];

  brews = [ ]; # add formulae common to every machine here
  masApps = { }; # Mac App Store apps common to all machines

  # ---------------- Home-Manager packages ----------------
  hmCommon = with pkgs; [
    # =====================================================================
    # Programming Languages and Core Dev Tools
    # =====================================================================
    cargo # Rust package manager
    customEmacs # Custom Emacs with macOS enhancements
    gh # GitHub CLI tool
    git # Version control system
    go # Go programming language
    golangci-lint # Go linter
    gopls # Go language server
    nixd # Nix language server
    _1password-cli # 1Password CLI

    # =====================================================================
    # Build and System Tools
    # =====================================================================
    cmake # Cross-platform build system
    comma # Run programs without installing them
    devbox # Portable, isolated dev environments
    just # Command runner (make alternative)
    nix-tree # Visualise Nix package dependencies
    watch # Execute a program periodically

    # =====================================================================
    # Text Processing and Formatting
    # =====================================================================
    delta # Git diff viewer
    graphviz # Graph visualization software
    nixfmt-rfc-style # Nix code formatter
    pandoc # Universal document converter
    prettierd # Prettier daemon
    shellcheck # Shell script analyzer
    shfmt # Shell script formatter
    stylua # Lua code formatter
    taplo-cli # TOML toolkit
    yamllint # YAML linter
    yaml-language-server # YAML language server

    # =====================================================================
    # Terminal and CLI Tools
    # =====================================================================
    bash # Bourne Again Shell
    bat # Better cat with syntax highlighting
    btop # Resource monitor
    coreutils # GNU core utilities
    curl # Command-line file transfers
    direnv # Directory-local env vars
    dust # Disk usage analyzer
    fd # Find alternative
    fzf # Fuzzy finder
    gnupg # GNU Privacy Guard
    gum # Glamorous shell scripts
    hyperfine # CLI benchmarking tool
    jq # JSON processor
    less # Pager
    lsd # ls deluxe with icons
    ripgrep # Fast text search
    socat # Socket CAT
    starship # Prompt
    tree # Directory listing
    wget # File downloader
    xh # Friendly HTTP client
    yq # YAML processor
    zoxide # Smart cd
    zsh # Advanced shell
    fish # Friendly interactive shell

    # =====================================================================
    # Cloud and Infrastructure
    # =====================================================================
    argocd # GitOps CD tool
    cloudflared # Cloudflare Tunnel client
    kubecolor # Colorized kubectl
    kubectl # Kubernetes CLI

    # =====================================================================
    # Database Tools
    # =====================================================================
    mysql-client # MySQL client
    postgresql # PostgreSQL client tools

    # =====================================================================
    # Media Tools
    # =====================================================================
    imagemagick # Image manipulation suite
    ghostscript # PostScript & PDF interpreter

    # =====================================================================
    # AI and Machine Learning
    # =====================================================================
    ollama # Local AI model runner
    github-mcp-server # GitHub MCP server

    # =====================================================================
    # Emacs Editor Support
    # =====================================================================
    glibtool # GNU libtool
    aspell # Spell checker
    aspellDicts.en # English dictionary

    # =====================================================================
    # Fonts
    # =====================================================================
    atkinson-hyperlegible-next # Hyperlegible font
    nerd-fonts.hack # Hack Nerd Font
    nerd-fonts.jetbrains-mono # JetBrains Mono Nerd Font

    # =====================================================================
    # macOS-Specific Tools & Window Management
    # =====================================================================
    aerospace # Window manager
    jankyborders # Window borders
    pinentry_mac # GPG pinentry for macOS
    sketchybar # Status bar
  ];
  # Keep hmPackages for compatibility with other profiles
  hmPackages = hmCommon;
in
{
  inherit
    taps
    casks
    brews
    masApps
    hmPackages
    ;
}
