{
  description = "d1egoaz's Nix System Configuration";

  nixConfig = {
    extra-substituters = [
      "https://nix-community.cachix.org"
    ];
    extra-trusted-public-keys = [
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
    ];
  };

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.05";
    nixpkgs-unstable.url = "github:NixOS/nixpkgs/nixpkgs-unstable";

    home-manager = {
      url = "github:nix-community/home-manager/release-25.05";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    darwin = {
      url = "github:LnL7/nix-darwin/nix-darwin-25.05";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    emacs-overlay = {
      url = "github:nix-community/emacs-overlay";
      inputs.nixpkgs.follows = "nixpkgs-unstable";
    };

    flake-utils.url = "github:numtide/flake-utils";

    emacs-flake = {
      url = "path:/Users/diego.albeiroalvarezzuluag/dotfiles/nix/flakes/emacs";
      inputs.nixpkgs.follows = "nixpkgs-unstable";
      inputs.emacs-overlay.follows = "emacs-overlay";
      inputs.flake-utils.follows = "flake-utils";
    };
  };

  outputs = { nixpkgs, ... }@inputs:
    let
      overlays = [
        # This overlay makes unstable packages available through pkgs.unstable
        (final: prev: {
          unstable = import inputs.nixpkgs-unstable {
            system = prev.system;
            config.allowUnfree = true;
          };

          nh = inputs.nixpkgs-unstable.legacyPackages.${prev.system}.nh;

          # Add emacs from our custom flake
          emacs-custom = inputs.emacs-flake.packages.${prev.system}.default;
        })
      ];

      mkSystem = import ./lib/mksystem.nix {
        inherit overlays nixpkgs inputs;
      };
    in
    {
      # macOS configurations
      darwinConfigurations = {
        # Office MacBook Pro M-chip
        office-mbp = mkSystem "macbook-pro" {
          system = "aarch64-darwin";
          user = "diego.albeiroalvarezzuluag";
          darwin = true;
        };

        # Personal MacBook Pro M-chip
        personal-mbp = mkSystem "macbook-pro" {
          system = "aarch64-darwin";
          user = "diego";
          darwin = true;
        };

        # Personal Mac Mini M-chip
        personal-mini = mkSystem "mac-mini" {
          system = "aarch64-darwin";
          user = "diego";
          darwin = true;
        };
      };

      # Linux configurations
      # Note: Commented out because personal-server runs Ubuntu Server + Nix,
      # not NixOS. System configuration is handled by Ubuntu.
      # Uncomment and configure if migrating to NixOS in the future.
      #
      # nixosConfigurations = {
      #   # Personal Linux server
      #   personal-server = mkSystem "linux-server" {
      #     system = "x86_64-linux";
      #     user = "diego";
      #   };
      # };
    };
}
