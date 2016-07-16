#!/bin/bash

# Saved in ~/online-check.sh and in a cron job as:
# * * * * * ~/bin/online-check.sh

offline=`dig 8.8.8.8 +time=1 +short google.com A | grep -c "no servers could be reached"`
if [[ "$offline" == "0" ]]; then
  test -e ~/.offline && rm ~/.offline
else
  touch ~/.offline
fi
