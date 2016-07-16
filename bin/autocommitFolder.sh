#!/bin/sh
NOW=$(date +"%Y-%m-%d %k:%m")
cd $1
git add .
git ca -m "backup of $NOW" 
