{
  config,
  lib,
  ...
}:

let
  cfg = config.macos.controlCenter;
  menuBar = import ../../lib/macosMenuBar.nix { inherit lib; };

  placementOption =
    name: mapping:
    lib.mkOption {
      type = lib.types.nullOr (lib.types.enum mapping.states);
      default = null;
      example = builtins.head mapping.states;
      description = ''
        Semantic menu bar state for ${name}. A null value leaves the setting
        unmanaged. Only states measured on the supported macOS release are
        accepted.
      '';
    };

  currentHostDefaults = menuBar.currentHostDefaultsFor cfg;
in
{
  options.macos.controlCenter =
    lib.mapAttrs (name: mapping: placementOption name mapping) menuBar.itemMappings
    // {
      showBatteryPercentage = lib.mkOption {
        type = lib.types.nullOr lib.types.bool;
        default = null;
        description = ''
          Whether to show the battery percentage. A null value leaves the
          setting unmanaged.
        '';
      };
    };

  config = lib.mkIf (currentHostDefaults != { }) {
    targets.darwin.currentHostDefaults."com.apple.controlcenter" = currentHostDefaults;
  };
}
