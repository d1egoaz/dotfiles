#!/bin/sh

get_wifi_ssid() {
    for i in $(ifconfig -lX "en[0-9]"); do
        SSID=$(ipconfig getsummary "$i" 2>/dev/null | awk '/ SSID/ {print $NF}')
        [ -n "$SSID" ] && echo "$SSID" && return
    done
}

IP_ADDRESS=$(scutil --nwi | grep address | sed 's/.*://' | tr -d ' ' | head -1)
IS_VPN=$(scutil --nwi | grep -m1 'utun' | awk '{ print $1 }')
WIFI_SSID=$(get_wifi_ssid)

if [[ $IS_VPN != "" ]]; then
    # COLOR=$ACCENT_COLOR
    ICON=
    LABEL="VPN/$WIFI_SSID"
elif [[ $IP_ADDRESS != "" ]]; then
    COLOR=$BLUE
    ICON=
    LABEL=$WIFI_SSID
else
    COLOR=$RED
    ICON=
    LABEL="Not Connected"
fi

sketchybar --set $NAME \
    icon=$ICON \
    label="$LABEL" \
    background.color=$COLOR
