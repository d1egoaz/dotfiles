#!/bin/bash

# shellcheck source=/dev/null
source "${CONFIG_DIR}/colours.sh"

# Get Wi-Fi interface details and SSID
get_wifi_details() {
    local wifi_service
    wifi_service=$(networksetup -listallhardwareports | awk '/Wi-Fi|AirPort/{getline; print $2}' | head -1)

    if [[ -n $wifi_service ]]; then
        local ssid
        ssid=$(networksetup -getairportnetwork "$wifi_service" 2>/dev/null | cut -d' ' -f4-)
        if [[ $ssid != "You are not associated with an AirPort network." && -n $ssid ]]; then
            wifi_iface="$wifi_service"
            wifi_ssid="􀙇$ssid"
            return 0
        fi
    fi
    return 1
}

# Format LAN IP address for display
format_lan_ip() {
    if [[ $1 == 192.168.100.* ]]; then
        echo "􀤆LAN ISOLATED"
    else
        echo "􀤆${1%.*}.x"
    fi
}

get_wifi_details
lan_ip=""

# Find LAN IP addresses on ethernet interfaces
for iface in $(ifconfig -l | tr ' ' '\n' | grep -E '^en[0-9]+$'); do
    ip=$(ipconfig getifaddr "$iface" 2>/dev/null) || continue
    if [[ $iface == "$wifi_iface" ]]; then
        continue
    fi
    if [[ $ip != 169.254.* && $ip != 127.0.0.1 ]]; then
        lan_ip="$ip"
        break # Use first valid LAN IP found
    fi
done

# Better VPN detection
is_vpn=""
if scutil --nwi | grep -q 'utun[0-9]\+\|ipsec[0-9]\+\|ppp[0-9]\+'; then
    is_vpn="true"
fi

colour=$PURPLE

if [[ -n $is_vpn ]]; then
    label="􀎡VPN"
    [[ -n $wifi_ssid ]] && label+=" $wifi_ssid"
    [[ -n $lan_ip ]] && label+=" $(format_lan_ip "$lan_ip")"

elif [[ -n $wifi_ssid || -n $lan_ip ]]; then
    label=""
    [[ -n $wifi_ssid ]] && label+="$wifi_ssid"
    if [[ -n $lan_ip ]]; then
        [[ -n $label ]] && label+=" "
        label+="$(format_lan_ip "$lan_ip")"
    fi
else
    colour=$RED
    label="􀙥Not Connected"
fi

sketchybar --set "$NAME" \
    label="$label" \
    label.color="$colour"
