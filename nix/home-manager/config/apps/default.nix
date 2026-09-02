# Application Configuration Module
#
# This file is imported by nix/home-manager/default.nix as "./config/apps"

{
  imports = [
    ./alfred.nix
    ./fish.nix
    ./fzf.nix
    ./gh.nix
    ./git.nix
    ./pi.nix
    ./starship.nix
    ./vim.nix
    ./zsh.nix
  ];
}
