{ pkgs, config, ... }:

{
  programs.fish = {
    enable = true;

    # Shell abbreviations (only for simple expansions)
    shellAbbrs = {
      # Git shortcuts
      d = "cd ~/dotfiles";
      gs = "git status -s";
      gd = "git diff";
      gpu = "git pull";

      # utilities
      fkill = "ps -fea | fzf | cut -d' ' -f1 | xargs kill -9";
      cwd = "pwd | tr -d '\\r\\n' | pbcopy";
      cdtmp = "cd (mktemp --directory)";

      # kubernetes shortcuts - simple ones stay as abbrs
      kaf = "k apply -f";
      kgd = "k get deployments";
      kgn = "k get namespaces";
      kgp = "k get pods";
      kgpw = "k get pods -w";
      kgrs = "k get replicasets";
      kgs = "k get services";
      kgss = "k get statefulsets";
    };

    # Custom functions
    functions = {
      loop = ''
        set -l interval $argv[1]
        set -e argv[1]
        while true
            date
            eval $argv
            echo "-----"
            echo
            sleep $interval
        end
      '';
      # kubernetes functions - need to be functions (not abbrs) to work in command substitutions
      kgpn = "k get pods -o name | cut -d'/' -f2";
      kgnn = "k get nodes -o name | cut -d'/' -f2";
      kdn = "k describe nodes (kgnn | fzf --prompt 'k8s node > ')";
      kdp = "k describe pod (kgpn | fzf --prompt 'k8s pod > ')";
      kl = "k logs -f (kgpn | fzf --prompt 'k8s pod > ')";
      klk = "k logs -f (k get pods -o name -l app=kafka | cut -d'/' -f2 | fzf --prompt 'k8s pod > ') kafka";
      kpf = "k port-forward (kgpn | fzf --prompt 'k8s pod > ')";
      kpip = "k get pod (kgpn | fzf) -o json | jq '.status.podIP'";
      kx = "k exec -it (kgpn | fzf) --";
      kxb = "k exec -it (kgpn | fzf) -- /bin/bash";
      kgpis = "k get pods -o jsonpath='{.items[*].spec.containers[*].image}' | tr -s ' ' '\\n' | sort | uniq -c";
      kgpi = "k get pod (kgpn | fzf) -o jsonpath='{.spec.containers[*].image}' | tr -s ' ' '\\n' | sort";
    };

    # Shell aliases
    shellAliases = {
      ".." = "cd ..";
      "..." = "cd ../..";
      "...." = "cd ../../../";
      "....." = "cd ../../../../";
      # bat everywhere
      b = "command bat";
      man = "command batman";
    };

    # Startup initialization
    interactiveShellInit = ''
      set -U fish_greeting

      # Handle dumb/tramp terminals gracefully
      if test "$TERM" = "dumb"; or test "$TERM" = "tramp"
        function fish_prompt
          echo '$ '
        end
        return
      end

      # Set file descriptor limit (suppress errors on macOS)
      ulimit -n 2048 2>/dev/null; or true

      set -gx BAT_THEME "tokyonight_night"
      set -gx MANPAGER "sh -c 'col -bx | bat -l man -p'"
      set -gx _ZO_DOCTOR 0

      # Load SOPS-encrypted secrets
      if test -r "${config.sops.secrets.OPENAI_API_KEY.path}"
        set -gx OPENAI_API_KEY (cat "${config.sops.secrets.OPENAI_API_KEY.path}")
      end

      if test -r "${config.sops.secrets.HOMEBREW_GITHUB_API_TOKEN.path}"
        set -gx HOMEBREW_GITHUB_API_TOKEN (cat "${config.sops.secrets.HOMEBREW_GITHUB_API_TOKEN.path}")
      end

      # Work environment variables
      set -gx WORK_DIR_PATH ~/work
      set -gx LOCAL_GEM_PATH $WORK_DIR_PATH
      set -gx AWS_REGION us-east-1

      # Source private environment variables if they exist
      if test -f ~/.zprivate
        source ~/.zprivate
      end

      # Source custom AWS profile function
      if test -f ~/dotfiles/bin/files/awsprofile.fish
        source ~/dotfiles/bin/files/awsprofile.fish
      end

      # Emacs vterm integration setup
      if test "$INSIDE_EMACS" = "vterm"; and test -n "$EMACS_VTERM_PATH"; and test -f "$EMACS_VTERM_PATH/etc/emacs-vterm-fish.sh"
        source "$EMACS_VTERM_PATH/etc/emacs-vterm-fish.sh"
      end

      # fzf.fish
      set fzf_fd_opts --hidden --max-depth 5 --exclude .git
      set fzf_diff_highlighter delta --paging=never --width=20

      # Configure fzf keybindings: Ctrl-T (directory), Ctrl-R (history), Ctrl-G (git log), Ctrl-S (git status), Ctrl-V (variables)
      fzf_configure_bindings --directory=\ct --history=\cr --git_log=\cg --git_status=\cs --variables=\cv

      # Remove wezterm functions if not in wezterm
      if not set -q WEZTERM_EXECUTABLE
        functions -e __wezterm_set_user_var 2>/dev/null
        functions -e __wezterm_user_vars_precmd 2>/dev/null
        functions -e __wezterm_osc7 2>/dev/null
      end

      # Custom functions
      function pwd_tilde
        string replace --regex "^$HOME" "~" "$PWD"
      end

      function fish_title
        echo (pwd_tilde)": "(status current-command)
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
