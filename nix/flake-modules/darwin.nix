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
        taps = common.officeTaps;
        casks = common.officeCasks;
        brews = common.officeBrews;
        masApps = common.officeMasApps;
      };

      personal-mbp = mkDarwinSystem {
        user = "diego";
        taps = common.personalTaps;
        casks = common.personalCasks;
        brews = common.personalBrews;
        masApps = common.personalMasApps;
      };

      personal-mini = mkDarwinSystem {
        user = "diegoalvarez";
        taps = common.personalTaps;
        casks = common.personalCasks;
        brews = common.personalBrews;
        masApps = common.personalMasApps;
      };
    };
}
