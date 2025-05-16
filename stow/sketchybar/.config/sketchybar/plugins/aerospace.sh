#!/usr/bin/env bash

# make sure it's executable with:
# chmod +x ~/.config/sketchybar/plugins/aerospace.sh

source colours.sh

if [ "$1" = "$FOCUSED_WORKSPACE" ]; then
    sketchybar --set $NAME background.drawing=on \
        background.color=$ACTIVE_SPACE_BG_COLOUR \
        icon.color=$BAR_COLOUR
else
    sketchybar --set $NAME background.drawing=off \
        label.color=$INACTIVE_SPACE_ICON_COLOUR \
        icon.color=$INACTIVE_SPACE_ICON_COLOUR
fi
