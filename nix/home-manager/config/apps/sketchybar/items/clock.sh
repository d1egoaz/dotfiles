#!/bin/bash

# shellcheck source=/dev/null
source "${CONFIG_DIR}/colours.sh"

# label.font="${FONT}:Regular:14.0" \
sketchybar --add item clock right \
  --set clock update_freq=10 \
  label.color="$ORANGE" \
  icon=􀧞 \
  icon.color="$ORANGE" \
  script="$CONFIG_DIR/plugins/clock.sh"
