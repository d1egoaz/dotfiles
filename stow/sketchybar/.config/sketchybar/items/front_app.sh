#!/bin/bash

source colours.sh
source icons.sh

sketchybar --add item front_app left \
    --set front_app \
    label.font="${FONT}:Bold:16.0" \
    label.color=$ACTIVE_APP_FG_COLOUR \
    script="$PLUGIN_DIR/front_app.sh" \
    --subscribe front_app front_app_switched
