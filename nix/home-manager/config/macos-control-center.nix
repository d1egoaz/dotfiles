{
  config,
  lib,
  ...
}:

let
  cfg = config.macos.controlCenter;

  # Apple does not document these ByHost encodings, and the values vary by
  # module and macOS release. Keep measured values at this adapter boundary and
  # expose only semantic states to machine configuration.
  placementMappings = {
    battery = {
      preference = "Battery";
      states.always = 18;
    };
    nowPlaying = {
      preference = "NowPlaying";
      states."when-active" = 8;
    };
    screenMirroring = {
      preference = "ScreenMirroring";
      states."when-active" = 8;
    };
    sound = {
      preference = "Sound";
      states.always = 16;
    };
    spotlight = {
      preference = "Spotlight";
      states.always = 2;
    };
    weather = {
      preference = "Weather";
      states.always = 2;
    };
  };

  placementOption =
    name: mapping:
    lib.mkOption {
      type = lib.types.nullOr (lib.types.enum (builtins.attrNames mapping.states));
      default = null;
      example = builtins.head (builtins.attrNames mapping.states);
      description = ''
        Semantic menu bar state for ${name}. A null value leaves the setting
        unmanaged. Only states measured on the supported macOS release are
        accepted.
      '';
    };

  placementDefaults = lib.mapAttrs' (
    name: mapping:
    lib.nameValuePair mapping.preference (
      if cfg.${name} == null then null else mapping.states.${cfg.${name}}
    )
  ) placementMappings;

  currentHostDefaults =
    lib.filterAttrs (_: value: value != null) placementDefaults
    // lib.optionalAttrs (cfg.showBatteryPercentage != null) {
      BatteryShowPercentage = cfg.showBatteryPercentage;
    };
in
{
  options.macos.controlCenter =
    lib.mapAttrs (name: mapping: placementOption name mapping) placementMappings
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
