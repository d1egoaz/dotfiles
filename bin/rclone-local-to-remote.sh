#!/bin/bash
set -x

if [ $# -eq 1 ]; then # when called with arguments it'll sync
    rclone sync ~/gdrive/deft remote:deft --exclude ".git/**" -v
else
    rclone sync ~/gdrive/deft remote:deft --exclude ".git/**" --dry-run -v
fi
