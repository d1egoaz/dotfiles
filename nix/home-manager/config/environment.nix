{
  pkgs,
  opConfig,
  profile,
  ...
}:

{
  # ============================================================================
  # Environment Variables and Session Configuration
  # ============================================================================

  home.sessionVariables = {
    # Editor configuration
    EDITOR = "emacsclient -r -a emacs";
    VISUAL = "emacsclient -r -c -a emacs";
    ALTERNATE_EDITOR = "vim";
    PAGER = "less";
    LESS = "-R -X -F"; # -X: no alternate screen (preserves scrollback), -F: quit if fits, -R: colors
    KUBECOLOR_PAGER = ""; # disable kubecolor's pager, let output stay in scrollback
    MANPAGER = "bat";

    # Locale settings
    LANG = "en_US.UTF-8";
    LC_ALL = "en_US.UTF-8";
    LC_CTYPE = "en_US.UTF-8";

    # Development environment
    GOPRIVATE = opConfig.go_private;

    # Kubernetes
    KUBECONFIG = "$HOME/.kube/config";

    # Spell checker for emacs
    ASPELL_CONF = "dict-dir ${pkgs.aspellDicts.en}/lib/aspell";

    # macOS-specific environment variables
    EMACS_ADDITIONAL_DIR = "$HOME/dotfiles-private/chime";
    GPG_TTY = "${builtins.getEnv "TTY"}";

    # Active profile for conditional scripts
    PROFILE = profile;

    # Homebrew
    HOMEBREW_NO_ANALYTICS = "1";
    HOMEBREW_NO_INSECURE_REDIRECT = "1";
    # HOMEBREW_CASK_OPTS = "--require-sha";
    HOMEBREW_NO_ENV_HINTS = "1";
  };

  home.sessionPath = [
    "$HOME/.local/bin"
    "$HOME/.nix-profile/bin"
    "/etc/profiles/per-user/$USER/bin"
    "/run/current-system/sw/bin"
    "/nix/var/nix/profiles/default/bin"
    "/opt/homebrew/bin"
  ];
}
