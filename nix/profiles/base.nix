{ pkgs }:

# Base profile: packages and Homebrew selections common to ALL machines.

let
  # ---------------- Custom Packages ----------------
  customEmacs = import ../packages/emacs.nix { inherit pkgs; };

  # ---------------- Homebrew ----------------
  taps = [
    "fastrepl/hyprnote" # hyprnote tap for fastrepl
  ];

  casks = [
    "1password" # 1Password
    "OpenSuperWhisper" # OpenSuperWhisper
    "alfred" # Alfred launcher
    "cursor" # The Cursor IDE
    "excalidrawz" # ExcalidrawZ whiteboard
    "font-sf-pro" # SF Pro font
    "gpg-suite" # GPG Suite
    "hyprnote" # Hyprnote markdown notes
    "iterm2" # iTerm2 terminal
    "shottr" # Screenshot tool
    "tidal" # TIDAL desktop client
  ];

  brews = [
    "codex" # Codex OpenAI
  ];

  masApps = { }; # Mac App Store apps common to all machines

  # ---------------- System packages ----------------
  systemPackages = with pkgs; [
    home-manager # Home Manager itself needs to be system-wide
  ];

  # ---------------- Home-Manager packages ----------------
  hmPackages = with pkgs; [
    # =====================================================================
    # Programming Languages and Core Dev Tools
    # =====================================================================
    cargo # Rust package manager
    claude-code # Claude Code CLI tool
    customEmacs # Custom Emacs with macOS enhancements
    gh # GitHub CLI tool
    git # Version control system
    go # Go programming language
    golangci-lint # Go linter
    gopls # Go language server
    nixd # Nix language server
    nodejs # Node.js
    _1password-cli # 1Password CLI
    python3 # Python 3
    uv # Python package manager
    black # Python code formatter

    # =====================================================================
    # Build and System Tools
    # =====================================================================
    cmake # Cross-platform build system
    comma # Run programs without installing them
    devbox # Portable, isolated dev environments
    just # Command runner (make alternative)
    nh # Nix helper
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
    # Custom fonts
    (pkgs.runCommand "custom-fonts" { } ''
      mkdir -p $out/share/fonts/opentype
      mkdir -p $out/share/fonts/truetype

      # PragmataPro OTF fonts
      cp ${../fonts/PragmataPro-Regular0.9}/"OTF-CFF fonts (optional)"/*.otf $out/share/fonts/opentype/

      # PragmataPro TTF fonts
      cp ${../fonts/PragmataPro-Regular0.9}/*.ttf $out/share/fonts/truetype/

      # DengXian font
      cp ${../fonts/DengXian}/*.otf $out/share/fonts/opentype/

      # Essential PragmataPro fonts
      cp ${../fonts/EssentialPragmataPro}/*.ttf $out/share/fonts/truetype/
    '')

    # =====================================================================
    # macOS-Specific Tools & Window Management
    # =====================================================================
    aerospace # Window manager
    jankyborders # Window borders
    pinentry_mac # GPG pinentry for macOS
    sketchybar # Status bar
  ];
in
{
  # Export all base configuration sets for use by system profiles
  inherit
    taps
    casks
    brews
    masApps
    systemPackages
    hmPackages
    ;
}
