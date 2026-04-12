{
  pkgs,
  machineConfig,
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
    GOPRIVATE = machineConfig.go_private;
    # User-writable npm global installs for Nix-managed Node.js
    NPM_CONFIG_PREFIX = "$HOME/.local/share/npm-global";

    # Kubernetes
    KUBECONFIG = "$HOME/.kube/config";

    # Spell checker for emacs
    ASPELL_CONF = "dict-dir ${pkgs.aspellDicts.en}/lib/aspell";

    # Active profile for conditional scripts
    PROFILE = profile;

    # 1Password configuration (for scripts that need op access)
    OP_ACCOUNT = machineConfig.op_account;
    OP_VAULT = machineConfig.op_vault;
    SSH_AUTH_SOCK = "$HOME/Library/Group Containers/2BUA8C4S2C.com.1password/t/agent.sock";

    # Homebrew
    HOMEBREW_NO_ANALYTICS = "1";
    HOMEBREW_NO_INSECURE_REDIRECT = "1";
    # HOMEBREW_CASK_OPTS = "--require-sha";
    HOMEBREW_NO_ENV_HINTS = "1";
  }
  // (
    if machineConfig.emacs_additional_dir != "" then
      { EMACS_ADDITIONAL_DIR = machineConfig.emacs_additional_dir; }
    else
      { }
  );

  home.sessionPath = [
    "$HOME/.local/share/npm-global/bin"
    "$HOME/.local/bin"
    "$HOME/.nix-profile/bin"
    "/etc/profiles/per-user/$USER/bin"
    "/run/current-system/sw/bin"
    "/nix/var/nix/profiles/default/bin"
    "/opt/homebrew/bin"
  ];
}
