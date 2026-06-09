# Helper function to create macOS systems. Receives the shared, pre-imported
# pkgs from flake-modules/darwin.nix so all hosts evaluate one nixpkgs.
{
  inputs,
  pkgs,
  system,
}:
{
  user,
  profile,
}:
# Ensure that an allowed profile was provided.
assert builtins.elem profile [
  "personal"
  "office"
];

let
  inherit (inputs.nixpkgs) lib;

  # Load base & profile configurations. profileCfg is the single source for
  # both system packages (consumed here) and Home Manager packages (consumed
  # by home-manager/packages.nix via extraSpecialArgs).
  base = import ../profiles/base.nix { inherit pkgs; };
  profileCfg = import ../profiles/${profile}.nix { inherit pkgs base; };

  # Machine-specific configuration (1Password, git identity, work paths, LLM)
  # This is NOT secrets - just profile-specific config that differs by machine.
  # See machines.nix header for what each field controls.
  machines = import ../profiles/machines.nix;
  machineConfig = machines.${profile};
in
inputs.darwin.lib.darwinSystem {
  inherit system;
  modules = [
    # Pass pre-configured nixpkgs
    { nixpkgs.pkgs = pkgs; }

    # macOS system configuration
    (import ../systems/darwin/default.nix {
      inherit
        lib
        pkgs
        user
        profile
        ;
      inherit (profileCfg) systemPackages;
    })

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
        extraSpecialArgs = {
          inherit
            inputs
            user
            profile
            profileCfg
            machineConfig
            ;
        };
      };
    }
  ];
}
