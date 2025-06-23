# Application Configuration Module
#
# This file is imported by nix/home-manager/default.nix as "./config/apps"

{
  imports = [
    ./fish.nix
    ./fzf.nix
    ./git.nix
    ./sketchybar.nix
    ./starship.nix
    ./vim.nix
    ./wezterm.nix
    ./zsh.nix
  ];
}
