{ pkgs, ... }:

{
  programs.fish = {
    enable = true;

    # Shell abbreviations (only for simple expansions)
    shellAbbrs = {
      # Git shortcuts
      gs = "git status -s";
      gd = "git diff";
      gpu = "git pull";
    };

    # Shell aliases (direct port from zsh)
    shellAliases = {
      # bat everywhere
      b = "command bat";

      # utilities
      fkill = "ps -fea | fzf | cut -d' ' -f1 | xargs kill -9";
      cwd = "pwd | tr -d '\\r\\n' | pbcopy";

      # kubernetes shortcuts - direct ports from zsh
      kaf = "k apply -f";
      kgd = "k get deployments";
      kgn = "k get namespaces";
      kgnn = "k get nodes -o name | cut -d'/' -f2";
      kgp = "k get pods";
      kgpw = "k get pods -w";
      kgpn = "k get pods -o name | cut -d'/' -f2";
      kgrs = "k get replicasets";
      kgs = "k get services";
      kgss = "k get statefulsets";
      kdn = "k describe nodes (kgnn | fzf --prompt 'k8s node > ')";
      kdp = "k describe pod (kgpn | fzf --prompt 'k8s pod > ')";
      kl = "k logs -f (kgpn | fzf --prompt 'k8s pod > ')";
      klk = "k logs -f (k get pods -o name -l app=kafka | cut -d'/' -f2 | fzf --prompt 'k8s pod > ') kafka";
      kpf = "k port-forward (kgpn | fzf --prompt 'k8s pod > ')";
      kpip = "k get pod (kgpn | fzf) -o json | jq '.status.podIP'";
      kx = "k exec -it (kgpn | fzf) -- ";
      kxb = "kx /bin/bash";
      kgpis = "kgp -o jsonpath='{.items[*].spec.containers[*].image}' | tr -s ' ' '\\n' | sort | uniq -c";
      kgpi = "k get pod (kgpn | fzf) -o jsonpath='{.spec.containers[*].image}' | tr -s ' ' '\\n' | sort";
    };

    # Fish functions (only for complex logic)
    functions = {
      # Emacs ediff function
      ediff = {
        description = "Launch Emacs ediff between two files";
        body = ''
          if test (count $argv) -ne 2
            echo "Usage: ediff <FILE1> <FILE2>"
            return 1
          end
          emacsclient -r --eval "(ediff-files \"$argv[1]\" \"$argv[2]\")"
        '';
      };
    };

    # Startup initialization
    interactiveShellInit = ''
      # Handle dumb/tramp terminals gracefully
      if test "$TERM" = "dumb"; or test "$TERM" = "tramp"
        function fish_prompt
          echo '$ '
        end
        return
      end

      # Set file descriptor limit
      ulimit -n 2048

      # GPG TTY setup
      set -gx GPG_TTY (tty)

      # Source private environment variables if they exist
      if test -f ~/.zprivate
        source ~/.zprivate
      end

      # Emacs vterm integration setup
      if test "$INSIDE_EMACS" = "vterm"; and test -n "$EMACS_VTERM_PATH"; and test -f "$EMACS_VTERM_PATH/etc/emacs-vterm-fish.sh"
        source "$EMACS_VTERM_PATH/etc/emacs-vterm-fish.sh"
      end

      # Enable fzf key bindings if available
      if command -q fzf_key_bindings
        fzf_key_bindings
      end

      # Disable fish greeting
      set -g fish_greeting

      # Enable vi mode to match zsh
      fish_vi_key_bindings

      # Ctrl-x Ctrl-e to edit command line
      bind \cx\ce edit_command_buffer

      # Remove wezterm functions if not in wezterm
      if not set -q WEZTERM_EXECUTABLE
        functions -e __wezterm_set_user_var 2>/dev/null
        functions -e __wezterm_user_vars_precmd 2>/dev/null
        functions -e __wezterm_osc7 2>/dev/null
      end
    '';

    # Fish plugins
    plugins = [
      # fzf.fish (better fzf integration)
      {
        name = "fzf.fish";
        src = pkgs.fetchFromGitHub {
          owner = "PatrickF1";
          repo = "fzf.fish";
          rev = "v10.3";
          sha256 = "sha256-T8KYLA/r/gOKvAivKRoeqIwE2pINlxFQtZJHpOy9GMM=";
        };
      }
    ];
  };
}
