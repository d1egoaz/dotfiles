{ pkgs, ... }:
{
  imports = [
    ../diego/darwin-common.nix
  ];

  # Set primary user for this configuration
  system.primaryUser = "diego.albeiroalvarezzuluag";

  # The user should already exist, but we need to set this up so Nix knows
  # what our home directory is (https://github.com/LnL7/nix-darwin/issues/423).
  users.users."diego.albeiroalvarezzuluag" = {
    home = "/Users/diego.albeiroalvarezzuluag";
    shell = pkgs.zsh;
  };

  # User-specific homebrew configuration
  homebrew.casks = [
    "slack"
  ];
}
