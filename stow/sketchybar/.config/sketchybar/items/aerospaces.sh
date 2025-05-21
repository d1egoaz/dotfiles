#!/bin/bash

source colours.sh
source icons.sh

#############
# Aerospace #
#############
sketchybar --add event aerospace_workspace_change
for sid in $(aerospace list-workspaces --all); do
    ICON="$sid"
    case "$sid" in
    # https://www.nerdfonts.com/cheat-sheet
    "AI") ICON="" ;;
    "Chrome") ICON="" ;;
    "Emacs") ICON="" ;;
    "Notion") ICON="" ;;
    "Slack") ICON="" ;;
    "Terminal") ICON="" ;;
    "IDEs") ICON="" ;;
    "Zoom") ICON="󰰷" ;;
    esac

    # label="${ICON} ${sid}" \
    sketchybar --add item space.$sid left \
        --subscribe space.$sid aerospace_workspace_change \
        --set space.$sid \
        background.color=0x44ffffff \
        background.corner_radius=5 \
        background.drawing=on \
        icon="${ICON}" \
        click_script="aerospace workspace $sid" \
        script="$CONFIG_DIR/plugins/aerospace.sh $sid"
done
