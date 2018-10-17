yay -Sy socat
cd ~/go/src/github.com/wincent/clipper

sudo cp clipper /usr/local/bin
cp contrib/linux/systemd-service/clipper.service ~/.config/systemd/user
systemctl --user daemon-reload
systemctl --user enable clipper.service
systemctl --user start clipper.service
