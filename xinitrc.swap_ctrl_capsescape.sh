#!/bin/sh

# reset kbd, xcape
setxkbmap -option ''
pkill xcape

# palm detection
#synclient PalmDetect=1
#syndaemon -i 0.5 -K -R -d

setxkbmap -layout us -option 'caps:swapescape' -option 'altwin:ctrl_alt_win'

# PrtSc key ThinkPad:
xmodmap -e "remove Mod1 = Alt_R"
xmodmap -e "keycode 107 = Alt_R"
xmodmap -e "add Mod1 = Alt_R"

# after modding alt above
xcape -e 'Alt_L=Control_L|Shift_L|c'
xcape -e 'Alt_R=Control_L|Shift_L|v'

#xinput set-prop "TPPS/2 IBM TrackPoint" "libinput Natural Scrolling Enabled" 0

# echo 200 | sudo tee /sys/devices/platform/i8042/serio1/serio2/speed                                                                  <<<
# echo 255 | sudo tee /sys/devices/platform/i8042/serio1/serio2/sensitivity
DISPLAY=:0.0 notify-send "Keyboard configuration loaded d1egoaz"