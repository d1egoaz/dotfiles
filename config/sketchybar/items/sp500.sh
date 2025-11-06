#!/usr/bin/env bash

# shellcheck source=/dev/null
source "${CONFIG_DIR}/colours.sh"

sketchybar --add item sp500 right \
  --set sp500 update_freq=300 \
  script="$CONFIG_DIR/plugins/chym.sh VOO" \
  click_script="$CONFIG_DIR/plugins/sp500.sh" \
  icon="" \
  padding_left=4 \
  padding_right=4
