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

xinput set-prop "TPPS/2 IBM TrackPoint" "libinput Natural Scrolling Enabled" 0

# echo 200 | sudo tee /sys/devices/platform/i8042/serio1/serio2/speed                                                                  <<<
# echo 255 | sudo tee /sys/devices/platform/i8042/serio1/serio2/sensitivity

