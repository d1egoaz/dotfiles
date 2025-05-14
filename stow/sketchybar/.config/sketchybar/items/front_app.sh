#!/bin/bash

source colors.sh
source icons.sh

sketchybar --add item chevron left \
    --set chevron icon=" " label.drawing=off \
    padding_right=0 \
    --add item front_app left \
    --set front_app icon.drawing=off script="$PLUGIN_DIR/front_app.sh" \
    label.font="${FONT}:Bold:15.0" \
    --subscribe front_app front_app_switched \
    --set chevron background.color=0xff1e1e1e label.color=$ACTIVE_SPACE icon.color=$ACTIVE_SPACE \
    --set front_app background.color=0xff1e1e1e label.color=$ACTIVE_SPACE icon.color=$ACTIVE_SPACE \
    padding_left=0 \
    padding_right=0

