{
  pkgs,
  opConfig,
  profile,
  ...
}:

{
  programs.zsh = {
    enable = true;
    defaultKeymap = "emacs"; # Use emacs keybindings
    autosuggestion.enable = true;
    enableCompletion = false; # Disable to prevent slow compaudit
    enableVteIntegration = false;
    syntaxHighlighting.enable = true;
    # zprof.enable = true;  # Uncomment to profile startup performance

    # Shell aliases - keep navigation and bat as instant aliases
    shellAliases = {
      # bat everywhere
      b = "command bat";
      man = "command batman";
      # 1Password secret aliases (--account ensures correct vault regardless of active session)
      op-openai = "op read --account ${opConfig.op_account} 'op://${opConfig.op_vault}/OpenAI API/credential'";
    }
    // (
      if profile == "office" then
        {
          op-homebrew = "op read --account ${opConfig.op_account} 'op://${opConfig.op_vault}/Homebrew GitHub Token/credential'";
        }
      else
        { }
    );

    # Fish-style abbreviations (expand inline)
    zsh-abbr = {
      enable = true;
      abbreviations = {
        # Git shortcuts
        gs = "git status -s";
        gd = "git diff";
        gpu = "git pull";
        d = "cd ~/dotfiles";
        cdtmp = "cd $(mktemp --directory)";

        # utilities
        fkill = "ps -fea | fzf | cut -d' ' -f1 | xargs kill -9";
        cwd = "pwd | tr -d '\\r\\n' | pbcopy";

        # kubernetes shortcuts
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
        kdn = "k describe nodes $(kgnn | fzf --prompt 'k8s node > ')";
        kdp = "k describe pod $(kgpn | fzf --prompt 'k8s pod > ')";
        kl = "k logs -f $(kgpn | fzf --prompt 'k8s pod > ')";
        klk = "k logs -f $(k get pods -o name -l app=kafka | cut -d'/' -f2 | fzf --prompt 'k8s pod > ') kafka";
        kpf = "k port-forward $(kgpn | fzf --prompt 'k8s pod > ')";
        kpip = "k get pod $(kgpn | fzf) -o json | jq '.status.podIP'";
        kx = "k exec -it $(kgpn | fzf) -- ";
        kxb = "kx /bin/bash";
        kgpis = "kgp -o jsonpath='{.items[*].spec.containers[*].image}' | tr -s ' ' '\\n' | sort | uniq -c";
        kgpi = "k get pod $(kgpn | fzf) -o jsonpath='{.spec.containers[*].image}' | tr -s ' ' '\\n' | sort";
      };
    };

    # Startup initialization
    initContent = ''
      # Fast completion initialization (skip security checks)
      autoload -Uz compinit
      compinit -C  # Always skip security checks for fast startup

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

      # Increase file descriptor limit (helpful for some tools)
      ulimit -n 2048

      # Alt-key bindings for word navigation
      bindkey '^[[1;3D' backward-word
      bindkey '^[[1;3C' forward-word

      # Source private/sensitive environment variables
      [ -e ~/.zprivate ] && source ~/.zprivate

      # FZF configuration
      export FZF_DEFAULT_COMMAND='fd --type f --hidden --follow --exclude .git'
      export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"
      export FZF_ALT_C_COMMAND='fd --type d --hidden --follow --exclude .git'

      # work
      export WORK_DIR_PATH=~/work
      export LOCAL_GEM_PATH=$WORK_DIR_PATH
      export AWS_REGION=us-east-1

      # Disable zoxide doctor warnings
      export _ZO_DOCTOR=0
    '';

    # Zsh plugins
    plugins = [
      {
        name = "fzf-tab";
        src = pkgs.fetchFromGitHub {
          owner = "Aloxaf";
          repo = "fzf-tab";
          rev = "v1.2.0";
          sha256 = "sha256-q26XVS/LcyZPRqDNwKKA9exgBByE0muyuNb0Bbar2lY=";
        };
      }
    ];

    historySubstringSearch = {
      enable = true;
      searchUpKey = [
        "^[[A"
        "^P"
      ]; # Up arrow and Ctrl+P
      searchDownKey = [
        "^[[B"
        "^N"
      ]; # Down arrow and Ctrl+N
    };

    # History configuration
    history = {
      size = 10000;
      save = 10000;
      expireDuplicatesFirst = true;
      ignoreDups = true;
      ignoreSpace = true;
      share = true;
    };

    oh-my-zsh = {
      enable = false;
    };

    # Profiling - uncomment to enable startup profiling
    # zprof.enable = true;  # Shows startup time breakdown
  };
}
