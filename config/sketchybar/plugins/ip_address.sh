#!/usr/bin/env bash

# shellcheck source=/dev/null
source "${CONFIG_DIR}/colours.sh"

# ——— Constants ———
ICON_VPN_ON="󰒄"
ICON_VPN_OFF="󰅛"
ICON_WIFI_ON="󰖩"
ICON_WIFI_OFF="󰖪"
ICON_LAN_ON="󰌗"
ICON_LAN_OFF="󰌙" # Icons can be swapped to match your font set

# ——— Helpers ———
format_lan_ip() {
  local ip_address="$1" # Using a local variable is good practice
  [[ "$ip_address" == 192.168.100.* ]] && echo "LAN ISOLATED" || echo "${ip_address%.*}.x"
}

build_segment() {
  local icon_on="$1"
  local icon_off="$2"
  local name="$3"
  local value="$4"
  local is_on="$5"
  local icon label

  if (( is_on )); then
    icon="$icon_on"
    if [[ -n "$value" ]]; then
      label="$name $value"
    else
      label="$name ON"
    fi
  else
    icon="$icon_off"
    label="$name OFF"
  fi

  printf "%s %s" "$icon" "$label"
}

# ——— Collect State ———
WIFI_SSID=""
WIFI_IFACE=""
LAN_IP=""
IS_VPN=0
declare -A INTERFACE_IPS

for iface in $(ifconfig -l | tr ' ' '\n' | grep -E '^en[0-9]+$'); do
    ip=$(ipconfig getifaddr "$iface" 2>/dev/null)
    [[ -n "$ip" ]] && INTERFACE_IPS["$iface"]="$ip"

    if [[ -z "$WIFI_IFACE" ]]; then
        SSID=$(ipconfig getsummary "$iface" 2>/dev/null | awk '/^[[:space:]]*SSID[[:space:]]*:/ { sub(/^[^:]*:[[:space:]]*/, ""); print; exit }')
        if [[ -n "$SSID" ]]; then
            WIFI_SSID="$SSID"
            WIFI_IFACE="$iface"
        fi
    fi
done

for iface in "${!INTERFACE_IPS[@]}"; do
    [[ "$iface" == "$WIFI_IFACE" ]] && continue
    ip="${INTERFACE_IPS[$iface]}"
    [[ $ip == 169.254.* || $ip == 127.* ]] && continue
    LAN_IP=$ip
    break
done

if scutil --nwi | grep -qE 'utun[0-9]+'; then
    IS_VPN=1
fi

# ——— Render ———
VPN_SEGMENT="$(build_segment "$ICON_VPN_ON" "$ICON_VPN_OFF" "VPN" "" "$IS_VPN")"
wifi_active=0
if [[ -n "$WIFI_SSID" ]]; then
    wifi_active=1
fi
WIFI_SEGMENT="$(build_segment "$ICON_WIFI_ON" "$ICON_WIFI_OFF" "Wi-Fi" "$WIFI_SSID" "$wifi_active")"
lan_active=0
lan_label=""
if [[ -n "$LAN_IP" ]]; then
    lan_active=1
    lan_label="$(format_lan_ip "$LAN_IP")"
fi
LAN_SEGMENT="$(build_segment "$ICON_LAN_ON" "$ICON_LAN_OFF" "LAN" "$lan_label" "$lan_active")"

LABEL=$(printf "%s | %s | %s" "$VPN_SEGMENT" "$WIFI_SEGMENT" "$LAN_SEGMENT")

if (( IS_VPN || wifi_active || lan_active )); then
    COLOR=$PURPLE
else
    COLOR=$RED
fi

sketchybar --set "$NAME" \
    icon.color="$COLOR" \
    label="$LABEL" \
    label.color="$COLOR"
