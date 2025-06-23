#!/bin/bash

source colours.sh
source icons.sh

sketchybar --add item chym right \
    --set chym update_freq=300 \
    script="$CONFIG_DIR/plugins/chym.sh" \
    label.color=$GREEN \
    label.font="${FONT}:Regular:11.0" \
    icon=􀂘 \
    icon.color=$GREEN \
    padding_left=4 \
    padding_right=4
