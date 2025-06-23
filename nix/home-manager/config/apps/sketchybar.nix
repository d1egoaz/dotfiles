{ ... }:

{
  # ============================================================================
  # SketchyBar Configuration
  # ============================================================================

  xdg.configFile = {
    # Main configuration
    "sketchybar/sketchybarrc" = {
      source = ./sketchybar/sketchybarrc;
      executable = true;
    };

    # Colors and icons
    "sketchybar/colours.sh" = {
      source = ./sketchybar/colours.sh;
      executable = true;
    };
    "sketchybar/icons.sh" = {
      source = ./sketchybar/icons.sh;
      executable = true;
    };

    # Items
    "sketchybar/items/aerospaces.sh" = {
      source = ./sketchybar/items/aerospaces.sh;
      executable = true;
    };
    "sketchybar/items/apple.sh" = {
      source = ./sketchybar/items/apple.sh;
      executable = true;
    };
    "sketchybar/items/battery.sh" = {
      source = ./sketchybar/items/battery.sh;
      executable = true;
    };
    "sketchybar/items/chym.sh" = {
      source = ./sketchybar/items/chym.sh;
      executable = true;
    };
    "sketchybar/items/clock.sh" = {
      source = ./sketchybar/items/clock.sh;
      executable = true;
    };
    "sketchybar/items/front_app.sh" = {
      source = ./sketchybar/items/front_app.sh;
      executable = true;
    };
    "sketchybar/items/ip.sh" = {
      source = ./sketchybar/items/ip.sh;
      executable = true;
    };
    "sketchybar/items/volume.sh" = {
      source = ./sketchybar/items/volume.sh;
      executable = true;
    };

    # Plugins
    "sketchybar/plugins/aerospace.sh" = {
      source = ./sketchybar/plugins/aerospace.sh;
      executable = true;
    };
    "sketchybar/plugins/battery.sh" = {
      source = ./sketchybar/plugins/battery.sh;
      executable = true;
    };
    "sketchybar/plugins/chym.sh" = {
      source = ./sketchybar/plugins/chym.sh;
      executable = true;
    };
    "sketchybar/plugins/clock.sh" = {
      source = ./sketchybar/plugins/clock.sh;
      executable = true;
    };
    "sketchybar/plugins/front_app.sh" = {
      source = ./sketchybar/plugins/front_app.sh;
      executable = true;
    };
    "sketchybar/plugins/ip_address.sh" = {
      source = ./sketchybar/plugins/ip_address.sh;
      executable = true;
    };
    "sketchybar/plugins/volume.sh" = {
      source = ./sketchybar/plugins/volume.sh;
      executable = true;
    };
  };
}
