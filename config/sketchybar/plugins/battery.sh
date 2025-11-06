#!/usr/bin/env bash

PERCENTAGE="$(pmset -g batt | grep -Eo "\d+%" | cut -d% -f1)"
CHARGING="$(pmset -g batt | grep 'AC Power')"

if [ "$PERCENTAGE" = "" ]; then
  exit 0
fi

if [[ $CHARGING != "" ]]; then
  if [ "$PERCENTAGE" -ge 100 ]; then
    ICON="󱊦"
  elif [ "$PERCENTAGE" -ge 90 ]; then
    ICON="󰂋"
  elif [ "$PERCENTAGE" -ge 80 ]; then
    ICON="󰂊"
  elif [ "$PERCENTAGE" -ge 60 ]; then
    ICON="󰂉"
  elif [ "$PERCENTAGE" -ge 40 ]; then
    ICON="󰂈"
  elif [ "$PERCENTAGE" -ge 30 ]; then
    ICON="󰂇"
  elif [ "$PERCENTAGE" -ge 20 ]; then
    ICON="󰂆"
  else
    ICON="󰢟"
  fi
else
  if [ "$PERCENTAGE" -ge 100 ]; then
    ICON="󱊣"
  elif [ "$PERCENTAGE" -ge 90 ]; then
    ICON="󰂂"
  elif [ "$PERCENTAGE" -ge 80 ]; then
    ICON="󰂁"
  elif [ "$PERCENTAGE" -ge 70 ]; then
    ICON="󰂀"
  elif [ "$PERCENTAGE" -ge 60 ]; then
    ICON="󰁿"
  elif [ "$PERCENTAGE" -ge 50 ]; then
    ICON="󰁾"
  elif [ "$PERCENTAGE" -ge 40 ]; then
    ICON="󰁽"
  elif [ "$PERCENTAGE" -ge 30 ]; then
    ICON="󰁼"
  elif [ "$PERCENTAGE" -ge 20 ]; then
    ICON="󰁻"
  elif [ "$PERCENTAGE" -ge 10 ]; then
    ICON="󰁺"
  else
    ICON="󰂃"
  fi
fi

sketchybar --set "$NAME" icon="${ICON}" label="${PERCENTAGE}%"
