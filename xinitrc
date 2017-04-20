#!/bin/sh

# reset kbd, xcape
# setxkbmap -layout us
# pkill xcape

# 1.ubuntu tweak, caps lock as control

# 2. caps as ctrol + escape
xcape -e 'Control_L=Escape'

# palm detection
synclient PalmDetect=1
#syndaemon -i 0.5 -t -K -R -d # t enable mouse move
syndaemon -i 0.5 -K -R -d

