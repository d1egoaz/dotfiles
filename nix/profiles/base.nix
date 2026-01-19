{ pkgs }:

# Base profile: packages and Homebrew selections common to ALL machines.

let
  # ---------------- Custom Packages ----------------

  # ---------------- External Fonts Repo ----------------
  # Use a private repository for proprietary fonts (Pragmata*).
  # Note: builtins.fetchGit runs at eval time and can access private repos.
  fontsRepo = builtins.fetchGit {
    url = "https://github.com/d1egoaz/fonts.git";
    rev = "1b361b6e214713b15352cc24d77b2785706bb220";
  };

  # ---------------- System packages ----------------
  systemPackages = with pkgs; [
    home-manager # Home Manager itself needs to be system-wide
  ];

  # ---------------- Home-Manager packages ----------------
  hmPackages = with pkgs; [
    # =====================================================================
    # Programming Languages and Core Dev Tools
    # =====================================================================
    black # Python code formatter
    cargo # Rust package manager
    clippy
    rustfmt
    gh # GitHub CLI tool
    git # Version control system
    git-crypt # Git encryption tool
    go # Go programming language
    golangci-lint # Go linter
    gopls # Go language server
    nixd # Nix language server
    nodejs # Node.js
    python3 # Python 3
    uv # Python package manager
    sops # Encrypted file storage

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
    grandperspective # Disk usage analyzer

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
    taplo # TOML toolkit
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
    eza # ls alternative
    fd # Find alternative
    fzf # Fuzzy finder
    gnupg # GNU Privacy Guard
    gum # Glamorous shell scripts
    hyperfine # CLI benchmarking tool
    jq # JSON processor
    less # Pager
    # lsd # ls deluxe with icons
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
    wezterm # Terminal emulator (config via XDG symlink)

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
    mariadb # MariaDB/MySQL client (mysql-client renamed)
    postgresql # PostgreSQL client tools

    # =====================================================================
    # Media Tools
    # =====================================================================
    imagemagick # Image manipulation suite
    ghostscript # PostScript & PDF interpreter

    # =====================================================================
    # AI and Machine Learning
    # =====================================================================
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
    # Fonts managed via Nix (including local/proprietary fonts)
    (pkgs.runCommand "custom-fonts" { } ''
      mkdir -p $out/share/fonts/opentype
      mkdir -p $out/share/fonts/truetype

      # NOTE: Don't forget to update `rev/sha` in fontsRepo definition

      # PragmataPro OTF fonts (from private repo)
      cp ${fontsRepo}/PragmataPro-Regular0.903/"OTF-CFF fonts (optional)"/*.otf $out/share/fonts/opentype/

      # PragmataPro TTF fonts (from private repo)
      cp ${fontsRepo}/PragmataPro-Regular0.903/*.ttf $out/share/fonts/truetype/

      # Essential PragmataPro fonts (from private repo)
      cp ${fontsRepo}/EssentialPragmataPro/*.ttf $out/share/fonts/truetype/

      # DengXian font (kept locally for now)
      cp ${../fonts/DengXian}/*.otf $out/share/fonts/opentype/
    '')

    # =====================================================================
    # macOS-Specific Tools & Window Management
    # =====================================================================
    # aerospace # Window manager (installed via Homebrew cask)
    jankyborders # Window borders
    mas # Mac App Store
    pinentry_mac # GPG pinentry for macOS
    sketchybar # Status bar (config via XDG symlink)
  ];
in
{
  # Export base configuration sets for use by system profiles
  inherit systemPackages hmPackages;
}
