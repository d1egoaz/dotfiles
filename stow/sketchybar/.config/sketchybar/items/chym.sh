#!/bin/bash

source colours.sh
source icons.sh

sketchybar --add item chym right \
    --set chym update_freq=60 \
    script="plugins/chym.sh" \
    label.color=$RED \
    label.font="${FONT}:Regular:11.0" \
    icon=􀂘 \
    icon.color=$RED \
    padding_left=4 \
    padding_right=4
