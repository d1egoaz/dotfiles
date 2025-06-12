{ self, inputs, ... }:
{
  flake.darwinConfigurations =
    let
      mkDarwinSystem = import ../lib/mkDarwinSystem.nix { inherit inputs; };
    in
    {
      # macOS System Configurations
      office-mbp = mkDarwinSystem {
        user = "diego.albeiroalvarezzuluag";
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
}
