{
  description = "d1egoaz's Nix System Configuration";

  # ============================================================================
  # Nix Configuration
  # ============================================================================
  # Extra binary caches and public keys for faster builds
  # https://app.cachix.org/cache/nix-community
  nixConfig = {
    extra-substituters = [
      "https://nix-community.cachix.org"
    ];
    extra-public-keys = [
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
    ];
  };

  # ============================================================================
  # Inputs - External Dependencies
  # ============================================================================
  inputs = {
    # Main nixpkgs (using unstable)
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";

    # Code formatting and linting
    treefmt-nix.url = "github:numtide/treefmt-nix";

    # Utility functions for flakes
    flake-utils.url = "github:numtide/flake-utils";

    # Home Manager for user environment management
    home-manager = {
      url = "github:nix-community/home-manager/master";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # nix-darwin for macOS system management
    darwin = {
      url = "github:LnL7/nix-darwin/master";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # Emacs packages and overlay
    emacs-overlay = {
      url = "github:nix-community/emacs-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # Custom Emacs configuration
    emacs-flake = {
      url = "path:./flakes/emacs";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.emacs-overlay.follows = "emacs-overlay";
      inputs.flake-utils.follows = "flake-utils";
    };

    # Tokyo Night theme
    tokyonight = {
      url = "github:mrjones2014/tokyonight.nix";
    };
  };

  # ============================================================================
  # Outputs - What This Flake Provides
  # ============================================================================
  outputs =
    {
      nixpkgs,
      flake-utils,
      darwin,
      home-manager,
      emacs-flake,
      ...
    }@inputs:
    let
      # ========================================================================
      # System Builder Function
      # ========================================================================

      # macOS system builder
      mkSystem =
        {
          system,
          user,
          modules ? [ ],
        }:
        darwin.lib.darwinSystem {
          inherit system;
          modules = [
            # Apply basic nixpkgs config
            {
              nixpkgs.config.allowUnfree = true;
            }

            # Home Manager integration
            home-manager.darwinModules.home-manager
            {
              home-manager = {
                useGlobalPkgs = true;
                useUserPackages = true;
                users.${user} = import ./users/${user}/home-manager-darwin.nix;
                extraSpecialArgs = { inherit inputs; };
              };
            }
          ] ++ modules;
          # Pass the user parameter to all modules
          specialArgs = { inherit user; };
        };

      # ========================================================================
      # Code Formatting Setup (treefmt-nix)
      # ========================================================================
      treefmtOutputs = flake-utils.lib.eachDefaultSystem (
        system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
          treefmtEval = inputs.treefmt-nix.lib.evalModule pkgs ./treefmt.nix;
        in
        {
          formatter = treefmtEval.config.build.wrapper;
          checks = {
            formatting = treefmtEval.config.build.check inputs.self;
          };
        }
      );
    in
    {
      # ========================================================================
      # macOS System Configurations
      # ========================================================================
      # Apply with: darwin-rebuild switch --flake .#<config-name>
      # or with nh: nh os switch --hostname <config-name>
      darwinConfigurations = {

        office-mbp = mkSystem {
          system = "aarch64-darwin";
          user = "diego.albeiroalvarezzuluag";
          modules = [
            ./machines/macbook-pro.nix
            ./users/diego.albeiroalvarezzuluag/darwin.nix
          ];
        };

        personal-mbp = mkSystem {
          system = "aarch64-darwin";
          user = "diego";
          modules = [
            ./machines/macbook-pro.nix
            ./users/diego/darwin.nix
          ];
        };

        personal-mini = mkSystem {
          system = "aarch64-darwin";
          user = "diegoalvarez";
          modules = [
            ./machines/mac-mini.nix
            ./users/diegoalvarez/darwin.nix
          ];
        };
      };

      # ========================================================================
      # Development Tools
      # ========================================================================
      # Code formatter - formats all code in the repository
      # Usage: nix fmt
      formatter = treefmtOutputs.formatter;

      # Formatting checks - validates code formatting (useful for CI)
      # Usage: nix flake check
      checks = treefmtOutputs.checks;
    };
}
