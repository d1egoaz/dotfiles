# Helper function to create macOS systems
{ inputs }:
{
  user,
  taps,
  casks,
  brews,
  masApps,
}:
let
  system = "aarch64-darwin";
  pkgs = inputs.nixpkgs.legacyPackages.${system};
in
inputs.darwin.lib.darwinSystem {
  inherit system;
  modules = [
    # Apply basic nixpkgs config with emacs overlay
    {
      nixpkgs.config.allowUnfree = true;
      nixpkgs.overlays = [ inputs.emacs-overlay.overlays.default ];
    }

    # macOS system configuration
    (import ../systems/darwin/default.nix {
      inherit
        pkgs
        user
        taps
        casks
        brews
        masApps
        ;
    })

    # Home Manager integration
    inputs.home-manager.darwinModules.home-manager
    {
      home-manager = {
        useGlobalPkgs = true;
        useUserPackages = true;
        users.${user} = import ../home-manager;
        extraSpecialArgs = { inherit inputs user; };
      };
    }
  ];
  # Pass the user parameter to all modules
  specialArgs = { inherit user; };
}
