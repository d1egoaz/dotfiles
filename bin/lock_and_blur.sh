#!/usr/bin/env bash
#set -x

# set the icon and a temporary location for the screenshot to be stored
tmpbg="$HOME/.cache/screenlock.png"

# take a screenshot
scrot "$tmpbg"

# blur the screenshot by resizing and scaling back up
#convert "$tmpbg" -filter Gaussian -blur 0x1 -thumbnail 20% -sample 500% "$tmpbg"
convert "$tmpbg" -filter Gaussian -blur 0x8 "$tmpbg"

# lock the screen with the blurred screenshot
i3lock -b -i "$tmpbg"
