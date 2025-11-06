#!/usr/bin/env bash
set -euo pipefail

# Source colors
# shellcheck source=/dev/null
source "${CONFIG_DIR}/colours.sh"

# Get stock symbol from parameter or default to CHYM
SYMBOL=${1:-CHYM}
API_URL="https://query1.finance.yahoo.com/v8/finance/chart/${SYMBOL}"

# Fetch data from Yahoo Finance API
API_DATA=$(curl -s \
  -H "User-Agent: Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36" \
  "$API_URL")

# Extract price and previous close
PRICE=$(echo "$API_DATA" | jq -r '.chart.result[0].meta.regularMarketPrice // empty' 2>/dev/null)
PREV_CLOSE=$(echo "$API_DATA" | jq -r '.chart.result[0].meta.previousClose // empty' 2>/dev/null)

# Check if we got valid data
if [[ -z $PRICE || -z $PREV_CLOSE || $PRICE == "null" || $PREV_CLOSE == "null" ]]; then
  sketchybar --set "$NAME" label="Error: No data" label.color="$RED"
  exit 1
fi

# Calculate change and percentage using awk with proper variable passing
CHANGE=$(awk -v price="$PRICE" -v prev="$PREV_CLOSE" 'BEGIN{print price - prev}')
PCT=$(awk -v price="$PRICE" -v prev="$PREV_CLOSE" 'BEGIN{print ((price - prev) / prev) * 100}')

# Determine sign, color, and get absolute values
if (($(awk -v change="$CHANGE" 'BEGIN{print (change<0)}'))); then
  sign="−"
  color="$RED"
  abschange=$(awk -v change="$CHANGE" 'BEGIN{print -change}')
  abspct=$(awk -v pct="$PCT" 'BEGIN{print -pct}')
else
  sign="+"
  color="$GREEN"
  abschange="$CHANGE"
  abspct="$PCT"
fi

# Format the label for SketchyBar
LABEL=$(printf "%s $%.2f %s%.2f (%s%.2f%%)" "$SYMBOL" "$PRICE" "$sign" "$abschange" "$sign" "$abspct")

# Set the SketchyBar item with color
sketchybar --set "$NAME" label="$LABEL" label.color="$color" icon.color="$color"
