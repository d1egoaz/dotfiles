{ self, inputs, ... }:
{
  flake.darwinConfigurations =
    let
      common = import ../systems/darwin/common.nix;
      mkDarwinSystem = import ../lib/mkDarwinSystem.nix { inherit inputs; };
    in
    {
      # macOS System Configurations
      office-mbp = mkDarwinSystem {
        user = "diego.albeiroalvarezzuluag";
        hostname = "office-mbp";
        taps = common.officeTaps;
        casks = common.officeCasks;
        masApps = common.officeMasApps;
      };

      personal-mbp = mkDarwinSystem {
        user = "diego";
        hostname = "personal-mbp";
        taps = common.personalTaps;
        casks = common.personalCasks;
        masApps = common.personalMasApps;
      };

      personal-mini = mkDarwinSystem {
        user = "diegoalvarez";
        hostname = "personal-mini";
        taps = common.personalTaps;
        casks = common.personalCasks;
        masApps = common.personalMasApps;
      };
    };
}
