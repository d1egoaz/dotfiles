#!/usr/bin/env bash

# make sure it's executable with:
# chmod +x ~/.config/sketchybar/plugins/aerospace.sh

source colors.sh

if [ "$1" = "$FOCUSED_WORKSPACE" ]; then
    sketchybar --set $NAME background.drawing=on \
        background.color=$ACCENT_COLOR2 \
        label.color=$BAR_COLOR2 \
        icon.color=$BAR_COLOR2
else
    sketchybar --set $NAME background.drawing=off \
        label.color=$ACCENT_COLOR \
        icon.color=$ACCENT_COLOR
fi
