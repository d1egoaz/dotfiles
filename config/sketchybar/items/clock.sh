#!/usr/bin/env bash

# shellcheck source=/dev/null
source "${CONFIG_DIR}/colours.sh"

sketchybar --add item clock right \
  --set clock update_freq=10 \
  label.color="$ORANGE" \
  icon.color="$ORANGE" \
  script="$CONFIG_DIR/plugins/clock.sh"
