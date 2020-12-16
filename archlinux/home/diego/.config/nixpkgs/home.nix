{ config, pkgs, ... }:

let
  prefferedFont = "Iosevka Term SS04";
  emacs_community_overlay = (import (builtins.fetchTarball {
    url =
      "https://github.com/nix-community/emacs-overlay/archive/master.tar.gz";
  }));
  unstable =
    import <nixpkgs-unstable> { overlays = [ emacs_community_overlay ]; };

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
    unstable.alacritty
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
    unstable.kafkacat
    unstable.languagetool
    unstable.tmux
    unstable.ripgrep
    unstable.ruby
    # rust
    unstable.rustc
    unstable.cargo
    unstable.rustfmt

    unstable.tldr
    unstable.wget
    unstable.zsh
  ];

  home.file = {
    ".config/alacritty/alacritty.yml".source = ./resources/alacritty.yml;
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
      # package = unstable.emacsGcc;
      package = unstable.emacsMacport;
      # package = unstable.emacsUnstable;
      # extraPackages = epkgs: [ epkgs.vterm ];
    };

    vim = {
      enable = true;
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
  };
}
