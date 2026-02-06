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
        user = "diego.alvarez";
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

  perSystem = _: {
    # Ensure evaluation during `nix flake check`
    checks = {
      darwin-office = inputs.self.darwinConfigurations.office-mbp.system;
      darwin-personal-mbp = inputs.self.darwinConfigurations.personal-mbp.system;
      darwin-personal-mini = inputs.self.darwinConfigurations.personal-mini.system;
    };
  };
}
