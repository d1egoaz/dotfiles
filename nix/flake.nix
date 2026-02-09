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
    extra-trusted-public-keys = [
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
    ];
    show-trace = true;
    warn-dirty = false;
  };

  # ============================================================================
  # Inputs - External Dependencies
  # ============================================================================
  inputs = {
    # Flake framework for better organization
    flake-parts.url = "github:hercules-ci/flake-parts";

    # Main nixpkgs (using stable darwin branch)
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-25.11-darwin";

    # Unstable nixpkgs for packages that need latest features
    nixpkgs-unstable.url = "github:NixOS/nixpkgs/nixpkgs-unstable";

    # Code formatting and linting
    treefmt-nix.url = "github:numtide/treefmt-nix";

    # Utility functions for flakes
    flake-utils.url = "github:numtide/flake-utils";

    # Home Manager for user environment management
    home-manager = {
      url = "github:nix-community/home-manager/release-25.11";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # nix-darwin for macOS system management
    darwin = {
      url = "github:LnL7/nix-darwin/nix-darwin-25.11";
      inputs.nixpkgs.follows = "nixpkgs";
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
    inputs:
    inputs.flake-parts.lib.mkFlake { inherit inputs; } {
      # Import our modular configuration
      imports = [
        ./flake-modules/treefmt.nix
        ./flake-modules/darwin.nix
      ];

      # Systems I support
      systems = [
        "aarch64-darwin"
      ];
    };
}
