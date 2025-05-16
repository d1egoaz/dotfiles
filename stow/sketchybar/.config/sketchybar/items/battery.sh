#!/bin/bash

source colors.sh
source icons.sh

sketchybar --add item battery right \
    --set battery update_freq=120 \
    script="plugins/battery.sh" \
    --subscribe battery system_woke power_source_change
