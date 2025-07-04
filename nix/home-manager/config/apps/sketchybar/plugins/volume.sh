#!/bin/sh

# The volume_change event supplies a $INFO variable in which the current volume
# percentage is passed to the script.

if [ "$SENDER" = "volume_change" ]; then
    VOLUME="$INFO"

    if [ "$VOLUME" -ge 60 ]; then
        ICON=""
    elif [ "$VOLUME" -ge 30 ]; then
        ICON="󰕾"
    elif [ "$VOLUME" -ge 1 ]; then
        ICON=""
    else
        ICON="󰖁"
    fi

    sketchybar --set "$NAME" icon="$ICON" label="${VOLUME}%"
fi
