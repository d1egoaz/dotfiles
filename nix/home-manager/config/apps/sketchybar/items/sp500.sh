#!/bin/bash

# shellcheck source=/dev/null
source "${CONFIG_DIR}/colours.sh"

sketchybar --add item sp500 right \
  --set sp500 update_freq=300 \
  script="$CONFIG_DIR/plugins/chym.sh ^SPX" \
  label.color="$GREEN" \
  label.font="${FONT}:Regular:11.0" \
  icon=􀈼 \
  icon.color="$GREEN" \
  padding_left=4 \
  padding_right=4