#!/usr/bin/env bash

# make sure it's executable with:
# chmod +x ~/.config/sketchybar/plugins/aerospace.sh

source colors.sh

if [ "$1" = "$FOCUSED_WORKSPACE" ]; then
    sketchybar --set $NAME background.drawing=on label.color=$ACTIVE_SPACE
else
    sketchybar --set $NAME background.drawing=off label.color=$WHITE
fi
