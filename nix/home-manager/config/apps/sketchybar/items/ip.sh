#!/bin/bash

# shellcheck source=/dev/null
source "${CONFIG_DIR}/colours.sh"

sketchybar --add item ip_address right \
  --set ip_address script="$CONFIG_DIR/plugins/ip_address.sh" \
  label.font="${FONT}:Regular:11.0" \
  update_freq=30 \
  padding_left=2 \
  padding_right=8 \
  --subscribe ip_address wifi_change
