{
  pkgs,
  inputs,
  ...
}:

{
  imports = [ inputs.tokyonight.homeManagerModules.default ];
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
    # Packages - Only packages that are in devbox global
    # ========================================================================
    packages = with pkgs; [
      # Development tools
      btop
      cargo
      gh
      git
      go
      golangci-lint
      gopls

      # Build tools
      cmake
      glibtool

      # Editor
      emacs-custom
      aspell
      aspellDicts.en

      # Shell and terminal utilities
      bat
      direnv
      fd
      fzf
      gum
      htop
      lsd
      ripgrep
      starship
      tmux
      tree
      zoxide

      # Text processing and formatting
      delta
      graphviz
      jq
      nixfmt-rfc-style
      pandoc
      prettierd
      shellcheck
      shfmt
      taplo-cli
      yamllint
      yq
      shellcheck
      yaml-language-server

      # System utilities
      bash
      coreutils
      gnupg
      socat
      xh

      # Cloud and Kubernetes tools
      kubernetes-helm
      argocd
      kubecolor
      cloudflared

      # Database tools
      mysql-client
      postgresql

      # Media
      ffmpeg

      # AI tools
      ollama

      # Fonts
      atkinson-hyperlegible-next
      nerd-fonts.fira-code
      nerd-fonts.hack
      nerd-fonts.jetbrains-mono
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

      # Development environment
      GOPATH = "$HOME/go";
      GOPRIVATE = "github.com/1debit/*";
      GOROOT = "${pkgs.go}/share/go";

      # Kubernetes
      KUBECONFIG = "$HOME/.kube/config";

      # Rust configuration
      CARGO_HOME = "$HOME/.cargo";

      ASPELL_CONF = "dict-dir ${pkgs.aspellDicts.en}/lib/aspell";
    };

    # Additional PATH entries
    sessionPath = [
      "$HOME/.local/bin"

      # Development tools
      "$GOPATH/bin"
      "$GOROOT/bin"
      "$CARGO_HOME/bin"
    ];
  };

  # ============================================================================
  # XDG Configuration Files
  # ============================================================================

  xdg = {
    enable = true;

    configFile = {
      # Linting and formatting
      "yamllint/config".source = ../../../stow/yamllint/.config/yamllint/config;

      # Git configuration
      "git/config".source = ../../../stow/git/.config/git/config;
      "git/config-work".source = ../../../stow/git/.config/git/config-work;
      "git/config-personal".source = ../../../stow/git/.config/git/config-personal;
      "git/attributes".source = ../../../stow/git/.config/git/attributes;
      "git/ignore".source = ../../../stow/git/.config/git/ignore;

      # GitHub CLI
      "gh/config.yml".source = ../../../stow/gh/.config/gh/config.yml;
      "gh/hosts.yml".source = ../../../stow/gh/.config/gh/hosts.yml;

      # Shell and terminal
      "starship.toml".source = ../../../stow/starship/.config/starship.toml;
      "tmux/tmux.conf".source = ../../../stow/tmux/.config/tmux/tmux.conf;
    };
  };

  # ============================================================================
  # Home Files (dotfiles in $HOME)
  # ============================================================================

  home.file = {
    # Security and encryption
    # ".gnupg/gpg.conf".source = ../../../stow/gnupg/.gnupg/gpg.conf;
    # ".gnupg/gpg-agent.conf".source = ../../../stow/gnupg/.gnupg/gpg-agent.conf;

    # Editor and tools
    ".vimrc".source = ../../../stow/vim/dot-vimrc;
    ".vale.ini".source = ../../../stow/vale/config;

    # Shell configuration
    ".aliases".source = ../../../stow/aliases/dot-aliases;
    # ".zshrc" - Let home-manager generate this with proper starship integration
    # ".zprofile" - No longer needed, everything managed by home-manager
    # ".zprivate" - Not managed by home-manager, sourced directly in zsh initContent
    # Note: fzf-tab plugin will be managed by home-manager zsh configuration instead
  };

  # ============================================================================
  # Program Configurations
  # ============================================================================

  programs = {

    # Shell with plugins
    zsh = {
      enable = true;
      autosuggestion.enable = true;
      syntaxHighlighting.enable = true;

      # History settings
      history = {
        size = 10000;
        save = 10000;
      };

      # Shell options and initialization
      initContent = ''
        # Load zsh profiling (if needed)
        zmodload zsh/zprof

        # Handle dumb/tramp terminals gracefully
        if [[ $TERM == "dumb" ]]; then
          unsetopt zle
          PS1='$ '
          return
        fi
        if [[ $TERM == "tramp" ]]; then
          unsetopt zle
          PS1='[\u@\h \w]$ '
          return
        fi

        # Interactive keybindings
        bindkey jk vi-cmd-mode

        # --- Interactive Functions ---
        # ediff: Launches Emacs ediff between two files.
        ediff() {
          if [ -z "$2" ]; then
            echo "Usage: ediff <FILE1> <FILE2>"
          else
            emacsclient -r --eval "(ediff-files \"$1\" \"$2\")"
          fi
        }

        # Emacs vterm integration (if inside Emacs and vterm available)
        if [[ "$INSIDE_EMACS" = "vterm" ]] &&
           [[ -n ''${EMACS_VTERM_PATH} ]] &&
           [[ -f "''${EMACS_VTERM_PATH}/etc/emacs-vterm-zsh.sh" ]]; then
          source "''${EMACS_VTERM_PATH}/etc/emacs-vterm-zsh.sh"

          ff() {
            vterm_cmd find-file "$(realpath "''${@:-.}")"
          }

          msg() {
            vterm_cmd message "%s" "$*"
          }
        fi

        # Increase file descriptor limit (helpful for some tools)
        ulimit -n 2048

        # --- Plugins and Completions ---

        # Use Emacs keybindings in zsh
        bindkey -e

        # Terminal alternate mode initialization (for some key sequences)
        if (( ''${+terminfo[smkx]} )) && (( ''${+terminfo[rmkx]} )); then
          zle-line-init() { echoti smkx; }
          zle-line-finish() { echoti rmkx; }
          zle -N zle-line-init
          zle -N zle-line-finish
        fi

        # Fuzzy history search (using up/down arrows)
        if [[ -n "''${terminfo[kcuu1]}" ]]; then
          autoload -U up-line-or-beginning-search
          zle -N up-line-or-beginning-search
          bindkey -M emacs "''${terminfo[kcuu1]}" up-line-or-beginning-search
          bindkey -M viins "''${terminfo[kcuu1]}" up-line-or-beginning-search
          bindkey -M vicmd "''${terminfo[kcuu1]}" up-line-or-beginning-search
        fi
        if [[ -n "''${terminfo[kcud1]}" ]]; then
          autoload -U down-line-or-beginning-search
          zle -N down-line-or-beginning-search
          bindkey -M emacs "''${terminfo[kcud1]}" down-line-or-beginning-search
          bindkey -M viins "''${terminfo[kcud1]}" down-line-or-beginning-search
          bindkey -M vicmd "''${terminfo[kcud1]}" down-line-or-beginning-search
        fi

        # Alt-key bindings for word navigation
        bindkey '^[[1;3D' backward-word
        bindkey '^[[1;3C' forward-word

        # Bind Ctrl-x Ctrl-e to edit the current command line in $EDITOR
        autoload -U edit-command-line
        zle -N edit-command-line
        bindkey '\C-x\C-e' edit-command-line

        # GPG: set TTY and update agent on each command execution
        export GPG_TTY=$TTY
        _gpg-agent_update-tty_preexec() {
          gpg-connect-agent updatestartuptty /bye &>/dev/null
        }
        autoload -U add-zsh-hook
        add-zsh-hook preexec _gpg-agent_update-tty_preexec

        # Source aliases if they exist
        [ -e ~/.aliases ] && source ~/.aliases

        # Source private/sensitive environment variables
        [ -e ~/.zprivate ] && source ~/.zprivate

        # Set up advanced completion options
        zstyle ':completion:*:*:*:*:*' menu select
        zstyle ':completion:*' matcher-list \
          'm:{[:lower:][:upper:]}={[:upper:][:lower:]}' \
          'r:|=*' 'l:|=* r:|=*'
        unset CASE_SENSITIVE HYPHEN_INSENSITIVE
        autoload -Uz compinit && compinit
      '';

      # Additional plugins
      plugins = [
        {
          name = "fzf-tab";
          src = pkgs.fetchFromGitHub {
            owner = "Aloxaf";
            repo = "fzf-tab";
            rev = "v1.1.2";
            sha256 = "sha256-Qv8zAiMtrr67CbLRrFjGaPzFZcOiMVEFLg1Z+N6VMhg=";
          };
        }
      ];
    };

    # Development tools
    git.enable = true;
    git.delta.tokyonight.enable = true;

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

    # Fuzzy finder with custom configuration
    fzf = {
      enable = true;

      enableZshIntegration = true;
      tokyonight.enable = true;
      defaultOptions = [
        "--exact"
        "--height 30%"
        "--no-preview"
        "--layout reverse"
        "--multi"
        "-0"
        "--no-info"
        "--pointer ●"
        "--color gutter:-1,pointer:#00ff00"
      ];
      defaultCommand = "fd --type f --hidden --follow --exclude .git";
      changeDirWidgetCommand = "fd --type f --hidden --follow --exclude .git";
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
    pinentry.package = pkgs.pinentry_mac;
  };

  services.emacs = {
    # run as daemon
    enable = true;
    package = pkgs.emacs-custom;
  };
}
