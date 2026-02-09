# Application Configuration Module
#
# This file is imported by nix/home-manager/default.nix as "./config/apps"

{
  imports = [
    ./alfred.nix
    ./fish.nix
    ./fzf.nix
    ./git.nix
    ./starship.nix
    ./vim.nix
    ./zsh.nix
  ];
}
