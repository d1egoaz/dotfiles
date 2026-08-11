{ lib }:

let
  # This is the shared, semantic policy for every managed Mac. Private macOS
  # encodings stay in the adapters below instead of leaking into host config.
  policy = {
    autoHide = "in-full-screen-only";
    layout = "office";

    clock = {
      flashDateSeparators = false;
      isAnalog = false;
      show24Hour = false;
      showAMPM = true;
      showDate = "always";
      showDayOfWeek = true;
      showSeconds = false;
      timeAnnouncementsEnabled = false;
    };

    controlCenter = {
      battery = "always";
      bluetooth = "hidden";
      clock = "always";
      controlCenter = "always";
      display = "hidden";
      focus = "hidden";
      nowPlaying = "when-active";
      screenMirroring = "when-active";
      sound = "always";
      spotlight = "always";
      timeMachine = "hidden";
      userSwitcher = "hidden";
      vpn = "hidden";
      weather = "always";
      wifi = "always";
      showBatteryPercentage = true;
    };
  };

  # Apple does not document these ByHost encodings, and observed values differ
  # between macOS releases and modules. Only states measured on these macOS
  # 26.5 machines are mapped here. A hidden state uses the standard visibility
  # flag only because no reliable ByHost encoding has been measured for it.
  itemMappings = {
    battery = {
      statusItem = "Battery";
      preference = "Battery";
      states = [
        "always"
        "hidden"
      ];
      rawStates.always = 18;
    };
    bluetooth = {
      statusItem = "Bluetooth";
      states = [ "hidden" ];
    };
    clock = {
      statusItem = "Clock";
      states = [
        "always"
        "hidden"
      ];
    };
    controlCenter = {
      statusItem = "BentoBox-0";
      states = [ "always" ];
    };
    display = {
      statusItem = "Display";
      states = [ "hidden" ];
    };
    focus = {
      statusItem = "FocusModes";
      states = [ "hidden" ];
    };
    nowPlaying = {
      statusItem = "NowPlaying";
      preference = "NowPlaying";
      states = [
        "when-active"
        "hidden"
      ];
      rawStates."when-active" = 8;
    };
    screenMirroring = {
      statusItem = "ScreenMirroring";
      preference = "ScreenMirroring";
      states = [
        "when-active"
        "hidden"
      ];
      rawStates."when-active" = 8;
    };
    sound = {
      statusItem = "Sound";
      preference = "Sound";
      states = [
        "always"
        "hidden"
      ];
      rawStates.always = 16;
    };
    spotlight = {
      statusItem = "Spotlight";
      preference = "Spotlight";
      states = [
        "always"
        "hidden"
      ];
      rawStates.always = 2;
    };
    timeMachine = {
      statusItem = "TimeMachine";
      states = [ "hidden" ];
    };
    userSwitcher = {
      statusItem = "UserSwitcher";
      states = [ "hidden" ];
    };
    vpn = {
      statusItem = "VPN";
      states = [ "hidden" ];
    };
    weather = {
      statusItem = "Weather";
      preference = "Weather";
      states = [
        "always"
        "hidden"
      ];
      rawStates.always = 2;
    };
    wifi = {
      statusItem = "WiFi";
      states = [
        "always"
        "hidden"
      ];
    };
  };

  autoHideMappings = {
    always = {
      controlCenter = 0;
      hideMenuBar = true;
      visibleInFullScreen = false;
    };
    "on-desktop-only" = {
      controlCenter = 1;
      hideMenuBar = true;
      visibleInFullScreen = true;
    };
    "in-full-screen-only" = {
      controlCenter = 2;
      hideMenuBar = false;
      visibleInFullScreen = false;
    };
    never = {
      controlCenter = 3;
      hideMenuBar = false;
      visibleInFullScreen = true;
    };
  };

  showDateMappings = {
    "when-space-allows" = 0;
    always = 1;
    never = 2;
  };

  layouts.office = {
    "NSStatusItem Preferred Position Battery" = 173;
    "NSStatusItem Preferred Position BentoBox-0" = 131;
    "NSStatusItem Preferred Position Sound" = 249;
    "NSStatusItem Preferred Position WiFi" = 227;
  };

  currentHostDefaultsFor =
    controlCenter:
    let
      rawEntries = lib.filter (entry: entry != null) (
        lib.mapAttrsToList (
          name: mapping:
          let
            state = controlCenter.${name};
          in
          if state != null && mapping ? rawStates && builtins.hasAttr state mapping.rawStates then
            lib.nameValuePair mapping.preference mapping.rawStates.${state}
          else
            null
        ) itemMappings
      );
    in
    builtins.listToAttrs rawEntries
    // lib.optionalAttrs (controlCenter.showBatteryPercentage != null) {
      BatteryShowPercentage = controlCenter.showBatteryPercentage;
    };

  visibilityDefaultsFor =
    controlCenter:
    lib.mapAttrs' (
      name: mapping:
      lib.nameValuePair "NSStatusItem VisibleCC ${mapping.statusItem}" (controlCenter.${name} != "hidden")
    ) itemMappings;

  autoHide = autoHideMappings.${policy.autoHide};
in
{
  inherit
    currentHostDefaultsFor
    itemMappings
    policy
    visibilityDefaultsFor
    ;

  systemDefaults = {
    global = {
      "_HIHideMenuBar" = autoHide.hideMenuBar;
    };

    globalCustom = {
      AppleMenuBarVisibleInFullscreen = autoHide.visibleInFullScreen;
    };

    clock = {
      FlashDateSeparators = policy.clock.flashDateSeparators;
      IsAnalog = policy.clock.isAnalog;
      Show24Hour = policy.clock.show24Hour;
      ShowAMPM = policy.clock.showAMPM;
      ShowDate = showDateMappings.${policy.clock.showDate};
      ShowDayOfWeek = policy.clock.showDayOfWeek;
      ShowSeconds = policy.clock.showSeconds;
      TimeAnnouncementsEnabled = policy.clock.timeAnnouncementsEnabled;
    };

    controlCenter =
      visibilityDefaultsFor policy.controlCenter
      // layouts.${policy.layout}
      // {
        AutoHideMenuBarOption = autoHide.controlCenter;
      };
  };
}
