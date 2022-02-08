{ config, lib, pkgs, ... }:

# my channels:
# home-manager https://github.com/nix-community/home-manager/archive/master.tar.gz
# nixos-unstable-small https://nixos.org/channels/nixos-unstable-small

let
  fonts.fontconfig.enable = true;
  prefferedFont = "Iosevka Term SS08";
  emacs_community_overlay = (import (builtins.fetchTarball {
    url =
      "https://github.com/nix-community/emacs-overlay/archive/master.tar.gz";
  }));
  unstable = import <nixos-unstable-small> { overlays = [ emacs_community_overlay ]; };
  # nixos status https://status.nixos.org/, check for nixos-unstable-small channel
  # https://hydra.nixos.org/job/nixos/unstable-small/tested
  # unstable = import (fetchTarball
  #   "https://github.com/NixOS/nixpkgs/archive/5687a345be5806c3ba6fc3ca33b38525f28c905a.tar.gz") {
  #     overlays = [ emacs_community_overlay ];
  #     config = { allowUnfree = true; };
  #   };

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
    pkgs.coreutils # for adding realpath used by vterm ff function
    pkgs.gnused
    unstable.docker-compose
    pkgs.ejson
    # unstable .exa
    unstable.fd
    unstable.gitAndTools.git-crypt
    unstable.gnupg
    unstable.gnuplot
    unstable.go
    unstable.google-cloud-sdk
    pkgs.graphviz
    pkgs.gtypist
    pkgs.htop
    unstable.iosevka-bin
    unstable.kafkacat
    pkgs.languagetool
    pkgs.manpages
    pkgs.mitmproxy
    pkgs.mpv
    pkgs.nixfmt
    pkgs.pandoc
    unstable.ripgrep
    pkgs.shellcheck
    pkgs.shfmt
    unstable.tldr
    #unstable .vscode
    pkgs.watch
  ];

  # example xdg
  # xdg.configFile."git/attributes".source = ./resources/gitattributes;

  programs = {
    home-manager.enable = true; # Let Home Manager install and manage itself.

    tmux = {
      enable = true;
      package = unstable.tmux;
      shortcut = "b";
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

  };
}
