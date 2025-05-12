#!/bin/bash

source colors.sh
source icons.sh

sketchybar --add item ip_address right \
    --set ip_address script="$CONFIG_DIR/plugins/ip_address.sh" \
    update_freq=30 \
    padding_left=2 \
    padding_right=8 \
    --subscribe ip_address wifi_change
