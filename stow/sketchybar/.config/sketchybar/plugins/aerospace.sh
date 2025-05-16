#!/usr/bin/env bash

# make sure it's executable with:
# chmod +x ~/.config/sketchybar/plugins/aerospace.sh

source colors.sh

if [ "$1" = "$FOCUSED_WORKSPACE" ]; then
    sketchybar --set $NAME background.drawing=on \
        background.color=$ACTIVE_SPACE_BG_COLOR \
        icon.color=$BAR_COLOR
else
    sketchybar --set $NAME background.drawing=off \
        label.color=$INACTIVE_SPACE_ICON_COLOR \
        icon.color=$INACTIVE_SPACE_ICON_COLOR
fi
