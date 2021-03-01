{ config, lib, pkgs, ... }:

let
  fonts.fontconfig.enable = true;
  prefferedFont = "Iosevka Term SS08";
  #emacs_community_overlay = (import (builtins.fetchTarball {
  #  url = "https://github.com/nix-community/emacs-overlay/archive/master.tar.gz";
  #}));
  unstable = import <nixpkgs-unstable> { };
  #unstablenixos = import <nixos-unstable> {overlays = [ emacs_community_overlay ];};

  iosevkass08 = pkgs.iosevka.override {
    privateBuildPlan = {
      design = [ "ss04" ];
      family = "Iosevka Term SS08";
    };
    set = "term";
  };

in {
  home.username = "diegoalvarez";
  home.homeDirectory = "/Users/diegoalvarez";
  home.stateVersion = "20.09";
  nixpkgs.config.allowUnfree = true;

  home.packages = [
    # rm -rf ~/.emacs.d/.local/etc/*spell*
    # same as (pkgs.aspellWithDicts (dicts: with dicts; [en en-computers en-science]))
    (pkgs.aspellWithDicts (d: [ d.en d.es d.en-computers d.en-science ]))
    pkgs.autojump
    pkgs.bat # cannot install unstable, collision between `/nix/store/4igk0yhblxiw9gy9jqflcczyq9da7zbj-bat-0.13.0/bin/.bat-wrapped' and `/nix/store/dr1d3f2p48izs3bc7873bal5np9mcajl-bat-0.15.0/bin/.bat-wrapped'
    pkgs.clipper
    pkgs.curl
    pkgs.direnv
    pkgs.ejson
    pkgs.fzf
    pkgs.gnupg
    pkgs.gtypist
    pkgs.jq
    pkgs.maven
    pkgs.mitmproxy
    pkgs.mpv
    pkgs.nixfmt
    pkgs.pandoc
    pkgs.reattach-to-user-namespace
    pkgs.shellcheck
    pkgs.socat
    pkgs.vscode
    pkgs.watch
    pkgs.zstd
    pkgs.alacritty
    unstable.bash
    unstable.docker-compose
    unstable.exa
    unstable.fd
    unstable.gitAndTools.gh
    unstable.gitAndTools.git-crypt
    unstable.go
    unstable.google-cloud-sdk
    unstable.graphviz
    unstable.htop
    unstable.iosevka-bin
    unstable.kafkacat
    unstable.languagetool
    unstable.ripgrep
    unstable.ruby
    # rust
    unstable.rustc
    unstable.cargo
    unstable.rustfmt

    unstable.tldr
    unstable.wget
  ];

  xdg.configFile."alacritty/alacritty.yml".source = ./resources/alacritty.yml;

  home.file = {
    "bin/ktmux".source = ./resources/bin/ktmux;
    "bin/kc".source = ./resources/bin/kc;
    "bin/ktail".source = ./resources/bin/ktail;
    ".ignore".source = ./resources/ignore;
  };

  programs = {
    # Let Home Manager install and manage itself.
    home-manager.enable = true;

    # NOTE: run `bat cache --build`
    # TODO: run this somehow in some nix after hook
    bat = {
      enable = true;
      config = {
        theme = "base16tomorrownight";
        pager = "less -FR";
        style = "numbers,changes,header";
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

    emacs = {
      enable = true;
      # package = unstablenixos.emacsGcc;
      # package = unstable.emacsGcc;
      package = unstable.emacsMacport;
      extraPackages = epkgs: [ epkgs.vterm ];
    };

    vim = {
      enable = true;
      plugins = [ pkgs.vimPlugins.base16-vim ];
      extraConfig = ''
        " Modify default escape and leader mappings for convenience
        inoremap jk <ESC>
        let mapleader = " "
        " Basic settings
        filetype plugin indent on
        syntax on
        set encoding=utf-8
        set clipboard=unnamed,unnamedplus
        set number
        set background=dark
        colorscheme base16-tomorrow-night
        "nnoremap <leader>y :call system('nc -U ~/.local/share/clipper/clipper.sock', @0)<CR>
        " use system clipboar as default register

        if &term =~ '256color'
          " disable Background Color Erase (BCE) so that color schemes
          " render properly when inside 256-color tmux and GNU screen.
          " see also http://snk.tuxfamily.org/log/vim-256color-bce.html
          set t_ut=
        endif
      '';
    };

    tmux = {
      enable = true;
      package = unstable.tmux;
      shortcut = "a";
      keyMode = "vi";
      plugins = with pkgs.tmuxPlugins; [
        continuum
        fzf-tmux-url # prefix u -> fzf urls
        pain-control # panes with vi like movements hjkl
        resurrect
        sensible
      ];
      extraConfig = (builtins.readFile ./resources/tmux.conf);
    };

    zsh = {
      enable = true;
      enableAutosuggestions = true;
      enableCompletion = true;
      defaultKeymap = "emacs";
      # defaultKeymap = "viins";
      oh-my-zsh = {
        enable = true;
        # theme = "sunaku"; overriden by powerlevel10k
        plugins = [ "fzf" "autojump" "gpg-agent" ];
      };
      initExtraFirst = (builtins.readFile ./resources/zshrc);
      envExtra = (builtins.readFile ./resources/zshenv);
      # from https://git.catgirl.ai/ext0l/nixos-config/src/branch/master/vector.nix
      plugins = [
        {
          name = "powerlevel10k";
          src = pkgs.zsh-powerlevel10k;
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
