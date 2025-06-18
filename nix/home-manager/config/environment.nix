{ pkgs, ... }:

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
    MANPAGER = "bat";

    # Locale settings
    LANG = "en_US.UTF-8";
    LC_ALL = "en_US.UTF-8";
    LC_CTYPE = "en_US.UTF-8";

    # Development environment
    GOPRIVATE = "github.com/1debit/*";

    # Kubernetes
    KUBECONFIG = "$HOME/.kube/config";

    # Spell checker for emacs
    ASPELL_CONF = "dict-dir ${pkgs.aspellDicts.en}/lib/aspell";

    # macOS-specific environment variables
    EMACS_ADDITIONAL_DIR = "$HOME/dotfiles-private/chime";
    GPG_TTY = "${builtins.getEnv "TTY"}";

    # Homebrew
    HOMEBREW_NO_ANALYTICS = "1";
    HOMEBREW_NO_INSECURE_REDIRECT = "1";
    # HOMEBREW_CASK_OPTS = "--require-sha";
    HOMEBREW_NO_ENV_HINTS = "1";
  };

  home.sessionPath = [
    "$HOME/.local/bin"
  ];
}
