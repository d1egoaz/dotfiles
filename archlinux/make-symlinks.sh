#!/bin/bash
# set -x

DOT_USER_HOME=~/dotfiles/archlinux/home/diego

for f in .aliases .emacs.d .spacemacs .tmux.conf .vim .vimrc .xinitrc .Xresources .zprofile .zshenv .zshrc .vim .agignore .gitconfig .gitconfig-personal .gitconfig-work .gitignore_global; do ln -sf $DOT_USER_HOME/$f ~/$f; done

rm -rf ~/.config/alacritty
mkdir -p ~/.config
for f in alacritty i3 i3status systemd polybar clipper karabiner; do ln -sf $DOT_USER_HOME/.config/$f ~/.config/; done

mkdir -p ~/.oh-my-zsh/themes
ln -sf $DOT_USER_HOME/.oh-my-zsh/themes/in-fino-veritas.zsh-theme ~/.oh-my-zsh/themes/in-fino-veritas.zsh-theme 


#systemctl --user enable --now ssh-agent
#sudo systemctl enable i3lock.service
#sudo systemctl enable wpa_supplicant.service
#sudo systemctl start wpa_supplicant.service

mkdir -p ~/.local/share
ln -sf $DOT_USER_HOME/.local/share/fonts ~/.local/share
