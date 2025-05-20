#!/usr/bin/env bash
source colours.sh

get_wifi_ssid() {
    for i in $(ifconfig -lX "en[0-9]"); do
        SSID=$(ipconfig getsummary "$i" 2>/dev/null | awk '/ SSID/ {print $NF}')
        [ -n "$SSID" ] && echo "$SSID" && return
    done
}

format_lan_ip() {
    if [[ $1 == 192.168.100.* ]]; then
        echo "􀤆LAN ISOLATED"
    else
        echo "􀤆${1%.*}.x"
    fi
}

WIFI_SSID="􀙇$(get_wifi_ssid)"

#–– find IPs on all en* ––
for iface in $(ifconfig -l | tr ' ' '\n' | grep -E '^en[0-9]+$'); do
    ip=$(ipconfig getifaddr "$iface" 2>/dev/null) || continue
    if [[ $iface == $WIFI_IFACE ]]; then
        WIFI_IP=$ip
    else
        LAN_IP=$ip
    fi
done

IS_VPN=$(scutil --nwi | awk '/utun[0-9]+/ {print; exit}')

if [[ -n $IS_VPN ]]; then
    COLOUR=$ORANGE
    LABEL="􀎡VPN"
    [[ -n $WIFI_SSID ]] && LABEL+=" $WIFI_SSID"
    [[ -n $LAN_IP ]] && LABEL+=" $(format_lan_ip $LAN_IP)"

elif [[ -n $WIFI_SSID || -n $LAN_IP ]]; then
    COLOUR=$WHITE
    LABEL=""
    [[ -n $WIFI_SSID ]] && LABEL+="$WIFI_SSID"
    if [[ -n $LAN_IP ]]; then
        [[ -n $LABEL ]] && LABEL+=" "
        LABEL+="LAN:$(format_lan_ip $LAN_IP)"
    fi

else
    COLOUR=$RED
    ICON=􀙥
    LABEL="􀙥Not Connected"
fi

sketchybar --set "$NAME" \
    label="$LABEL" \
    label.color="$COLOUR"
