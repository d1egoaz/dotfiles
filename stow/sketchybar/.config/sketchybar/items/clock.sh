#!/bin/bash

source colours.sh
source icons.sh

sketchybar --add item clock right \
    --set clock update_freq=10 \
    label.color=$ORANGE \
    label.font="${FONT}:Bold:16.0" \
    icon=􀧞 \
    icon.color=$ORANGE \
    script="$CONFIG_DIR/plugins/clock.sh"
