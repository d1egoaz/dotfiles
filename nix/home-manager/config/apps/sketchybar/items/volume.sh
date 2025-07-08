#!/bin/bash

# shellcheck source=/dev/null
source "${CONFIG_DIR}/colours.sh"

sketchybar --add item volume right \
    --set volume script="$CONFIG_DIR/plugins/volume.sh" \
    --subscribe volume volume_change
