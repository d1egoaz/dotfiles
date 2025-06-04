{ pkgs, ... }:

{
  programs.zsh = {
    enable = false;

    # History settings
    history = {
      size = 10000;
      save = 10000;
    };

    # Shell aliases
    shellAliases = {
      # File listing - handled by lsd module: ls, l, ll, la, lt, lla

      # bat everywhere
      b = "command bat";

      # git shortcuts
      gs = "git status -s";
      gd = "git diff";
      gpu = "git pull";

      # tmux
      tm = "zellij attach -s default";

      # utilities
      fkill = "ps -fea | fzf | cut -d\" \" -f1 | xargs kill -9";
      cwd = "pwd | tr -d \"\\r\\n\" | pbcopy"; # copy working directory

      # kubernetes shortcuts
      kaf = "k apply -f";
      kdn = "k describe nodes $(kgnn | fzf-tmux)";
      kdp = "k describe pod $(kgpn | fzf-tmux)";
      kgd = "k get deployments";
      kgn = "k get namespaces";
      kgnn = "k get nodes -o name | cut -d'/' -f2";
      kgp = "k get pods";
      kgpw = "k get pods -w";
      kgpn = "k get pods -o name | cut -d'/' -f2";
      kgrs = "k get replicasets";
      kgs = "k get services";
      kgss = "k get statefulsets";
      kl = "k logs -f $(kgpn | fzf-tmux --prompt \"k8s pod > \")";
      klk = "k logs -f $(k get pods -o name -l app=kafka | cut -d'/' -f2 | fzf-tmux --prompt \"k8s pod > \") kafka";
      kpf = "k port-forward $(kgpn | fzf-tmux --prompt \"k8s pod > \")";
      kpip = "k get pod $(kgpn | fzf-tmux) -o json | jq \".status.podIP\"";
      kx = "k exec -it $(kgpn | fzf-tmux) -- ";
      kxb = "kx /bin/bash";
      kgpis = "kgp -o jsonpath='{.items[*].spec.containers[*].image}' | tr -s '[[:space:]]' '\\n' | sort | uniq -c";
      kgpi = "k get pod $(kgpn | fzf-tmux) -o jsonpath=\"{.spec.containers[*].image}\" | tr -s \"[[:space:]]\" \"\\n\" | sort";
    };

    completionInit = ''
      zstyle ':completion:*' use-cache on
      zstyle ':completion:*' cache-path ~/.zcompcache
      autoload -Uz compinit
      compinit -C
    '';

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

      # Source private/sensitive environment variables
      [ -e ~/.zprivate ] && source ~/.zprivate

      # Set up advanced completion options
      zstyle ':completion:*:*:*:*:*' menu select
      zstyle ':completion:*' matcher-list \
        'm:{[:lower:][:upper:]}={[:upper:][:lower:]}' \
        'r:|=*' 'l:|=* r:|=*'
      unset CASE_SENSITIVE HYPHEN_INSENSITIVE

      if [[ -z $WEZTERM_EXECUTABLE ]]; then
        unset -f __wezterm_set_user_var
        unset -f __wezterm_user_vars_precmd
        unset -f __wezterm_osc7
      fi
    '';

    plugins = [
      {
        name = "zsh-autosuggestions";
        src = pkgs.fetchFromGitHub {
          owner = "zsh-users";
          repo = "zsh-autosuggestions";
          rev = "v0.7.1";
          sha256 = "sha256-iJdWopZwHpSyYl5/FQXEW7gl/SrKaYDEtTH9cGP7iPo";
        };
      }
      {
        name = "zsh-syntax-highlighting";
        src = pkgs.fetchFromGitHub {
          owner = "zsh-users";
          repo = "zsh-syntax-highlighting";
          rev = "0.8.0";
          sha256 = "sha256-iJdWopZwHpSyYl5/FQXEW7gl/SrKaYDEtTH9cGP7iPo";
        };
      }
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
}
