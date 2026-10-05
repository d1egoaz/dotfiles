#!/usr/bin/env zsh

payload="${1:-{}}"
event_type="$(printf '%s' "$payload" | jq -r '.type // empty' 2>/dev/null)"

[[ "$event_type" == "agent-turn-complete" ]] || exit 0

/usr/bin/afplay -v 15 "/System/Library/Sounds/Bottle.aiff" >/dev/null 2>&1 || true
