#!/bin/sh
dir=~/dotfiles/dnsmasq
ln -sf $dir/dnsmasq.conf /usr/local/etc/dnsmasq.conf

# To have launchd start dnsmasq at startup
sudo cp -fv /usr/local/opt/dnsmasq/*.plist /Library/LaunchDaemons
sudo chown root /Library/LaunchDaemons/homebrew.mxcl.dnsmasq.plist

# Then to load dnsmasq now
sudo launchctl load /Library/LaunchDaemons/homebrew.mxcl.dnsmasq.plist

# sudo launchctl stop homebrew.mxcl.dnsmasq
# sudo launchctl start homebrew.mxcl.dnsmasq

# Go to *System Preferences > Network*
# Click *Advanced...*
# Click the *DNS* tab
# add 127.0.0.1

# crontab
# 0 15 * * 2 ./Users/d1egoaz/dotfiles/dnsmasq/latest_ads.sh
