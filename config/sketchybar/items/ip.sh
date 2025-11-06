#!/usr/bin/env bash

# shellcheck source=/dev/null
source "${CONFIG_DIR}/colours.sh"

sketchybar --add item ip_address right \
    --set ip_address script="$CONFIG_DIR/plugins/ip_address.sh" \
    update_freq=30 \
    --subscribe ip_address wifi_change
