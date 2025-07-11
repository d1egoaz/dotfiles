#!/bin/bash

# shellcheck source=/dev/null
source "${CONFIG_DIR}/colours.sh"

sketchybar --add item front_app left \
  --set front_app \
  label.color="$ACTIVE_APP_FG_COLOUR" \
  script="$PLUGIN_DIR/front_app.sh" \
  --subscribe front_app front_app_switched
