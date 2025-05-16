#!/bin/bash

source colors.sh
source icons.sh

sketchybar --add item front_app left \
    --set front_app \
    label.font="${FONT}:Bold:16.0" \
    script="$PLUGIN_DIR/front_app.sh" \
    --subscribe front_app front_app_switched
