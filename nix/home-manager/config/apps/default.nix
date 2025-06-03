# Application Configuration Module
#
# This file is imported by nix/home-manager/default.nix as "./config/apps"

{
  imports = [
    ./fzf.nix
    ./git.nix
    ./starship.nix
    ./vim.nix
    ./wezterm.nix
    ./zsh.nix
  ];
}
