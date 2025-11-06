#!/usr/bin/env zsh

# Strip trailing newline/whitespace
url="${1%$'\n'}"
# url="${url%$'\r'}"

# Debug:
osascript -e "display notification \"$url\" with title \"Approving PR:\""

if [[ "$url" == https://github.com* ]]; then
    gh pr review --approve "$url"
else
    osascript -e 'display notification "URL is not a GitHub PR" with title "PR Review Failed"'
fi
