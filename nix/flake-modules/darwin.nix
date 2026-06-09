{ inputs, ... }:
let
  system = "aarch64-darwin";

  # Import nixpkgs once and share it across every host: the hosts are all
  # aarch64-darwin with identical nixpkgs config, so re-importing per host
  # only multiplies evaluation time.
  pkgs = import inputs.nixpkgs {
    inherit system;
    config.allowUnfree = true;
  };

  mkDarwinSystem = import ../lib/mkDarwinSystem.nix { inherit inputs pkgs system; };
in
{
  flake.darwinConfigurations = {
    # macOS System Configurations
    office-mbp = mkDarwinSystem {
      user = "diego.alvarez";
      profile = "office";
    };

    personal-mbp = mkDarwinSystem {
      user = "diego";
      profile = "personal";
    };

    personal-mini = mkDarwinSystem {
      user = "diegoalvarez";
      profile = "personal";
    };
  };

  perSystem = _: {
    # Evaluate every host during `nix flake check`. Derived from the host
    # attrset so new machines are checked automatically.
    checks = inputs.nixpkgs.lib.mapAttrs' (
      name: cfg: inputs.nixpkgs.lib.nameValuePair "darwin-${name}" cfg.system
    ) inputs.self.darwinConfigurations;
  };
}
