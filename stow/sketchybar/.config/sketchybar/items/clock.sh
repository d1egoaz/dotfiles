#!/bin/bash

source colours.sh
source icons.sh

sketchybar --add item clock right \
    --set clock update_freq=10 \
    icon=􀧞 \
    script="$CONFIG_DIR/plugins/clock.sh"
