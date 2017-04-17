#!/bin/sh

# reset kbd, xcape
#setxkbmap -layout us
#pkill xcape

# 1.ubuntu tweak, caps lock as control

# 2. caps as ctrol + escape
xcape -e 'Control_L=Escape'

