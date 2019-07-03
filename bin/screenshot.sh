#!/bin/bash
scrot -s -e 'xclip -selection clipboard -t "image/png" < $f && echo "Screenshot saved at $f"'
