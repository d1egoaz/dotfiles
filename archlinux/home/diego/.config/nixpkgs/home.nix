{ config, lib, pkgs, ... }:

# my channels:
# home-manager https://github.com/nix-community/home-manager/archive/master.tar.gz
# nixos-unstable-small https://nixos.org/channels/nixos-unstable-small
# nixpkgs-unstable https://nixos.org/channels/nixpkgs-unstable

let
  fonts.fontconfig.enable = true;
  prefferedFont = "Iosevka Term SS08";
  emacs_community_overlay = (import (builtins.fetchTarball {
    url = "https://github.com/nix-community/emacs-overlay/archive/master.tar.gz";
  }));
  #unstable = import <nixos-unstable-small> { overlays = [ emacs_community_overlay ]; };
  # check https://github.com/NixOS/nixpkgs/pull/120731
  #unstable = import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/5ef56ad9f17993bda954e5c0527984b3d6fa570a.tar.gz") {overlays = [ emacs_community_overlay ]; config= { allowUnfree = true ; } ;} ;
  unstable = import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/d9e18f4e7f77fffde95384d36cc8ac5d1d51b356.tar.gz") {overlays = [ emacs_community_overlay ]; config= { allowUnfree = true ; } ;} ;

  pkgsunstable = import <nixpkgs-unstable> { };

in {
  home.username = "diegoalvarez";
  home.homeDirectory = "/Users/diegoalvarez";
  home.stateVersion = "21.05";
  nixpkgs.config.allowUnfree = true;

  # sort packages by name ignoring source prefix in emacs `sort-regexp-fields`:
  # records to sort: `\w+\(.*\)$`, key: `\1`
  home.packages = [
    # same as (pkgs.aspellWithDicts (dicts: with dicts; [en en-computers en-science])) # rm -rf ~/.emacs.d/.local/etc/*spell*
    (unstable.aspellWithDicts (d: [ d.en d.es d.en-computers d.en-science ]))
    pkgs     .gnused
    unstable .docker-compose
    pkgs     .ejson
    # unstable .exa
    unstable .fd
    unstable .gitAndTools.git-crypt
    unstable .gnupg
    unstable .gnuplot
    unstable .go
    unstable .google-cloud-sdk
    pkgs     .graphviz
    pkgs     .gtypist
    pkgs     .htop
    unstable .iosevka-bin
    unstable .kafkacat
    pkgs     .languagetool
    pkgs     .manpages
    pkgs     .mitmproxy
    pkgs     .mpv
    pkgs     .nixfmt
    pkgs     .pandoc
    unstable .ripgrep
    pkgs     .shellcheck
    pkgs     .shfmt
    unstable .tldr
    #unstable .vscode
    pkgs     .watch
  ];

  # example xdg
  # xdg.configFile."git/attributes".source = ./resources/gitattributes;

  home.file = {
    "bin/ktmux".source = ./resources/bin/ktmux;
    "bin/kc".source = ./resources/bin/kc;
    "bin/ktail".source = ./resources/bin/ktail;
    ".ignore".source = ./resources/ignore;
    ".aliases".source = ./resources/aliases;
  };

  programs = {
    home-manager.enable = true; # Let Home Manager install and manage itself.

    alacritty = {
      enable = true;
      package = unstable.alacritty;
      settings = {
        env.TERM = "xterm-256color";
        colors.cursor = {
          text = "0x1d1f21";
          cursor = "0xc5c8c6";
        };
        cursor.style = "Block";
        font = {
          size = 14;
          normal.family = "Iosevka Term SS08";
          bold.family = "Iosevka Term SS08";
          italic.family = "Iosevka Term SS08";
        };
        window.dimensions = {
          lines = 60;
          columns = 200;
        };
      };
    };

    autojump = {
      enable = true;
      enableZshIntegration = true;
    };

    # NOTE: run `bat cache --build`, TODO: run this somehow in some nix after hook: postInstall
    bat = {
      enable = true;
      config = {
        pager = "less -FR";
        style = "numbers,changes,header";
        theme = "base16tomorrownight";
      };
      themes = {
        base16tomorrownight = builtins.readFile (pkgs.fetchFromGitHub {
          owner = "chriskempson";
          repo = "base16-textmate"; # Bat uses sublime syntax for its themes
          rev = "cab66929126a14acafe37cf9c24c9e700716cd5a";
          sha256 = "02hzzdx7llg3554nzr2kqswhfsdcs91c1519w4b0rb9s9fgq5awj";
        } + "/Themes/base16-tomorrow-night.tmTheme");
      };
    };

    exa = {
      enable = true;
      enableAliases = true;
    };

#(emacsGit.override
  #{ nativeComp = true; })
    emacs = {
      enable = true;
      package = unstable.emacsGcc;
      #package = unstable.emacsGit;
      extraPackages = epkgs: [ epkgs.vterm ];
    };

    fzf = {
      enable = true;
      package = unstable.fzf;
      enableZshIntegration = true;
    };

    git = {
      userEmail = "diego.canada@icloud.com";
      userName = "Diego Alvarez";
      signing = {
        key = "4DF4C58193BBB0863AB37A6DC63945863D4B9E77";
        signByDefault = true;
      };
      delta.enable = true; # https://github.com/dandavison/delta
      enable = true;
      package = unstable.gitAndTools.gitFull;
      includes = [{
        path = ./resources/gitconfigwork;
        condition = "gitdir:~/src/";
      }];
      extraConfig = {
        branch.sort = "-committerdate";
        core.commentChar = "@"; # so I can use emacs pull request reviews package
        credential.helper = "store --file /opt/dev/var/private/git_credential_store";
        diff.algorithm = "patience";
        github.user = "d1egoaz";
        merge.conflictstyle = "diff3";
        protocol.version = "2";
        pull.ff = "only";
        url."https://github.com/Shopify/".insteadOf = [
          "git@github.com:Shopify/"
          "git@github.com:shopify/"
          "ssh://git@github.com/Shopify/"
          "ssh://git@github.com/shopify/"
        ];
      };
      ignores = [ ".DS_STORE" ];
      attributes = [
        "*.el    diff=lisp"
        "*.go    diff=golang"
        "*.lisp  diff=lisp"
        "*.md    diff=markdown"
        "*.rb    diff=ruby"
        "*.rs    diff=rust"
      ];
    };

    gh = {
      enable = true;
      aliases =  {
          pc = "pr checkout";
          pv = "pr view";
        };
      gitProtocol = "https";
    };

    jq = {
      enable = true;
    };

    vim = {
      enable = true;
      plugins = [ unstable.vimPlugins.base16-vim ];
      extraConfig = builtins.readFile ./resources/vimrc;
    };

    tmux = {
      enable = true;
      package = unstable.tmux;
      shortcut = "a";
      keyMode = "vi";
      plugins = with unstable.tmuxPlugins; [
        continuum
        fzf-tmux-url # prefix u -> fzf urls
        pain-control # panes with vi like movements hjkl
        resurrect
        sensible
        {
          plugin = jump;
          extraConfig = "set -g @jump-key 'f'";
        }
      ];
      extraConfig = builtins.readFile ./resources/tmux.conf;
    };

    zsh = {
      enable = true;
      enableAutosuggestions = true;
      enableCompletion = true;
      defaultKeymap = "emacs";
      oh-my-zsh = {
        enable = true;
        plugins = [ "colored-man-pages" "gpg-agent" ];
      };
      history = {
        size = 50000;
        save = 50000;
      };
      initExtraBeforeCompInit = builtins.readFile ./resources/zshrc;
      envExtra = builtins.readFile ./resources/zshenv;
      plugins = [
        {
          name = "powerlevel10k";
          src = unstable.zsh-powerlevel10k;
          file = "share/zsh-powerlevel10k/powerlevel10k.zsh-theme";
        }
        {
          name = "powerlevel10k-config";
          src = lib.cleanSource ./resources;
          file = "p10k-config";
        }
      ];
    };
  };
}
