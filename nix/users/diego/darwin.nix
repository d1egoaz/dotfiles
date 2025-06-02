{ pkgs, user, ... }:
{
  imports = [
    ./darwin-common.nix
  ];

  # Set primary user for this configuration
  system.primaryUser = user;

  # The user should already exist, but we need to set this up so Nix knows
  # what our home directory is (https://github.com/LnL7/nix-darwin/issues/423).
  users.users.${user} = {
    home = "/Users/${user}";
    shell = pkgs.zsh;
  };

  # User-specific homebrew configuration
  homebrew.casks = [
    "discord"
  ];
}
