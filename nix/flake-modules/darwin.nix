{ inputs, ... }:
{
  flake.darwinConfigurations =
    let
      mkDarwinSystem = import ../lib/mkDarwinSystem.nix { inherit inputs; };
      system = "aarch64-darwin";
    in
    {
      # macOS System Configurations
      office-mbp = mkDarwinSystem {
        user = "diego.albeiroalvarezzuluag";
        profile = "office";
        inherit system;
      };

      personal-mbp = mkDarwinSystem {
        user = "diego";
        profile = "personal";
        inherit system;
      };

      personal-mini = mkDarwinSystem {
        user = "diegoalvarez";
        profile = "personal";
        inherit system;
      };
    };
}
