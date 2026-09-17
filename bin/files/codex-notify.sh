#!/usr/bin/env zsh

payload="${1:-{}}"
event_type="$(printf '%s' "$payload" | jq -r '.type // empty' 2>/dev/null)"

[[ "$event_type" == "agent-turn-complete" ]] || exit 0

/usr/bin/afplay -v 15 "/System/Library/Sounds/Bottle.aiff" >/dev/null 2>&1 || true

# Keep the relay's locked-Computer-Use trust fresh whenever the app is in use.
# It exits immediately when the relay is already trusted or when the caller is
# relay-hosted, so this is a no-op on ordinary turn ends.
"/Users/diegoalvarez/code/codex-relay/tools/relay-trusted/relay-trusted-ensure.sh" -t notify >/dev/null 2>&1 &
