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

xinput set-prop "SynPS/2 Synaptics TouchPad" "libinput Natural Scrolling Enabled" 1
xinput set-prop "SynPS/2 Synaptics TouchPad" "libinput Tapping Enabled" 1
xinput set-prop "SynPS/2 Synaptics TouchPad" "libinput Disable While Typing Enabled" 1

xrdb -merge ~/.Xresources
exec i3
#exec startkde
