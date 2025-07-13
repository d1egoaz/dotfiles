#!/bin/bash

# shellcheck source=/dev/null
source "${CONFIG_DIR}/colours.sh"

get_wifi_details() {
  for i in $(ifconfig -lX "en[0-9]"); do
    SSID=$(ipconfig getsummary "$i" 2>/dev/null |
      awk '/ SSID/ {print $NF}')
    if [ -n "$SSID" ]; then
      WIFI_IFACE="$i"
      WIFI_SSID="󰖩$SSID"
      return
    fi
  done
}

format_lan_ip() {
  if [[ $1 == 192.168.100.* ]]; then
    echo "󰌗LAN ISOLATED"
  else
    echo "󰌗${1%.*}.x"
  fi
}

get_wifi_details

#–– find IPs on all en* ––
for iface in $(ifconfig -l | tr ' ' '\n' | grep -E '^en[0-9]+'); do
  ip=$(ipconfig getifaddr "$iface" 2>/dev/null) || continue
  if [[ $iface == "$WIFI_IFACE" ]]; then
    #WIFI_IP=$ip
    continue
  else
    if [[ $ip != 169.254.* && $ip != 127.0.0.1 ]]; then
      LAN_IP=$ip
    fi
  fi
done

IS_VPN=$(scutil --nwi | awk '/utun[0-9]+/ {print; exit}')
COLOUR=$PURPLE

if [[ -n $IS_VPN ]]; then
  LABEL="󰒄VPN"
  [[ -n $WIFI_SSID ]] && LABEL+=" $WIFI_SSID"
  [[ -n $LAN_IP ]] && LABEL+=" $(format_lan_ip "$LAN_IP")"

elif [[ -n $WIFI_SSID || -n $LAN_IP ]]; then
  LABEL=" "
  [[ -n $WIFI_SSID ]] && LABEL+="$WIFI_SSID"
  # LABEL+="$WIFI_SSID"
  if [[ -n $LAN_IP ]]; then
    [[ -n $LABEL ]] && LABEL+=" "
    LABEL+=" $(format_lan_ip "${LAN_IP}")"
  fi
else
  COLOUR=$RED
  LABEL="󰌙Not Connected"
fi

sketchybar --set "$NAME" label="$LABEL" label.color="$COLOUR"
