#!/bin/sh

# The $NAME variable is passed from sketchybar and holds the name of
# the item invoking this script:
# https://felixkratz.github.io/SketchyBar/config/events#events-and-scripting

local_time="$(date '+%a, %B %d, %-I:%M%p')"
utc_time="$(TZ=UTC date '+%H:%M')"

sketchybar --set "$NAME" icon="􀧞" label="(UTC ${utc_time}) ${local_time}"
