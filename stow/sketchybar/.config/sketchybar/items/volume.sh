#!/bin/bash

source colours.sh
source icons.sh

sketchybar --add item volume right \
    --set volume script="$CONFIG_DIR/plugins/volume.sh" \
    --subscribe volume volume_change
