#!/usr/bin/env sh
# based on https://raw.githubusercontent.com/matthewbauer/macNixOS/master/link-apps.sh

APP_DIR=/Applications
NIX_PROFILE_APPS="$HOME"/.nix-profile/Applications
LATEST_NIX_PROFILE_APPS=$(readlink "$NIX_PROFILE_APPS")

# remove broken links
for f in "$APP_DIR"/*; do
	if [ -L "$f" ] && [ ! -e "$f" ]; then
		rm "$f"
	fi
done

# link new ones
# for f in "$LATEST_NIX_PROFILE_APPS"/*; do
for f in "$NIX_PROFILE_APPS"/*; do
	echo $f
	app_name="$(basename "$f")"
	echo $app_name
	echo "new link for $app_name"
	ln -sf "$f" "$APP_DIR"/
done
