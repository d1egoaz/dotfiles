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
  pkgs-unstable = inputs.nixpkgs-unstable.legacyPackages.${system};
  inherit (inputs.nixpkgs) lib;

  # Import nixpkgs with overlays applied upfront
  pkgs = import inputs.nixpkgs {
    inherit system;
    config.allowUnfree = true;
    overlays = [
      # Workaround: use inetutils from unstable (has Clang 17 fix)
      (_final: _prev: {
        inherit (pkgs-unstable) inetutils;
      })
    ];
  };

  # Load base & profile configurations with overlaid pkgs
  base = import ../profiles/base.nix { inherit pkgs; };
  profileCfg = import ../profiles/${profile}.nix { inherit pkgs base; };

  # 1Password account config (gitignored). Falls back to personal defaults
  # so personal machines work without the file; office machines need it.
  # NOTE: Uses absolute path via $HOME because Nix flakes filter gitignored
  # files from the source tree, making relative path literals invisible.
  secretsPath = builtins.toPath (builtins.getEnv "HOME" + "/dotfiles/nix/profiles/secrets.nix");
  opConfig =
    if builtins.pathExists secretsPath then
      (import secretsPath).${profile}
    else
      {
        op_account = "my.1password.com";
        op_vault = "Private";
        work_email = "";
        go_private = "";
        ssh_signing_key = "";
      };
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
            opConfig
            ;
        };
      };
    }
  ];
}
