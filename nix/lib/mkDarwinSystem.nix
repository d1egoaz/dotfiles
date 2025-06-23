# Helper function to create macOS systems
{ inputs }:
{
  user,
  profile,
  system,
}:
# Ensure that an allowed profile was provided.
assert builtins.elem profile [
  "personal"
  "office"
];

let
  pkgs = inputs.nixpkgs.legacyPackages.${system};
  # Load base & profile configurations
  base = import ../profiles/base.nix { inherit pkgs; };
  profileCfg = import ../profiles/${profile}.nix { inherit pkgs base; };
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
      inherit pkgs user;
      taps = profileCfg.taps;
      casks = profileCfg.casks;
      brews = profileCfg.brews;
      masApps = profileCfg.masApps;
      systemPackages = profileCfg.systemPackages;
    })

    # Mac App Util for better .app handling
    inputs.mac-app-util.darwinModules.default

    # Home Manager integration
    inputs.home-manager.darwinModules.home-manager
    {
      home-manager = {
        useGlobalPkgs = true;
        useUserPackages = true;
        backupFileExtension = "backup";
        users.${user} = import ../home-manager;
        # Pass additional arguments to all Home-Manager modules so they can
        # customize behaviour based on the current work profile.
        extraSpecialArgs = { inherit inputs user profile; };
      };
    }
  ];
}
