#!/bin/bash

# shellcheck source=/dev/null
source "${CONFIG_DIR}/colours.sh"

sketchybar --add item volume right \
    --set volume script="$CONFIG_DIR/plugins/volume.sh" \
    label.font="${FONT}:Regular:11.0" \
    --subscribe volume volume_change
