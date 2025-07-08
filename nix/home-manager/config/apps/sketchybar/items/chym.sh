#!/bin/bash

# shellcheck source=/dev/null
source "${CONFIG_DIR}/colours.sh"

sketchybar --add item chym right \
  --set chym update_freq=300 \
  script="$CONFIG_DIR/plugins/chym.sh" \
  click_script="$CONFIG_DIR/plugins/chym.sh" \
  icon="ⓒ" \
  padding_left=4 \
  padding_right=4
