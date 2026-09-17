#!/usr/bin/env zsh

payload="${1:-{}}"
event_type="$(printf '%s' "$payload" | jq -r '.type // empty' 2>/dev/null)"

[[ "$event_type" == "agent-turn-complete" ]] || exit 0

/usr/bin/afplay -v 15 "/System/Library/Sounds/Bottle.aiff" >/dev/null 2>&1 || true

# Keep the relay's locked-Computer-Use trust fresh whenever the app is in use.
# These run in the FOREGROUND on purpose. Backgrounding them makes this script
# exit immediately, the child is reparented to launchd, and its ancestry walk
# then sees a chain of one process, so trust can never be established. Measured:
# backgrounded gives "chain=NNNNN:sh" and no ancestry, foreground gives the full
# app chain. They exit immediately when the relay is already trusted, so the
# cost on an ordinary turn end is tens of milliseconds.
"/Users/diegoalvarez/code/codex-relay/tools/relay-trusted/relay-trusted-ensure.sh" -t notify >/dev/null 2>&1 || true

# Hooks need per-definition trust and this path does not, so it is the trigger
# that always fires.
"/Users/diegoalvarez/code/codex-relay/tools/relay-trusted/relay-trust-watch-ensure.sh" >/dev/null 2>&1 || true
