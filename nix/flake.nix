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
    # Flake framework for better organization
    flake-parts.url = "github:hercules-ci/flake-parts";

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

    # Utility functions for macOS
    mac-app-util = {
      url = "github:hraban/mac-app-util";
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
