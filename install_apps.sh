#!/bin/bash

dir=~/data/dotfiles

install_zsh () {
# Test to see if zshell is installed.  If it is:
if [ -f /bin/zsh -o -f /usr/bin/zsh ]; then
    # Clone my oh-my-zsh repository from GitHub only if it isn't already present
    if [[ ! -d ~/.oh-my-zsh/ ]]; then
	echo "installing oh-my-zsh"
	git clone git://github.com/robbyrussell/oh-my-zsh.git ~/.oh-my-zsh
	
ln -s $dir/in-fino-veritas.zsh-theme ~/.oh-my-zsh/themes/in-fino-veritas.zsh-theme
	
    fi
    # Set the default shell to zsh if it isn't currently set to zsh
    if [[ ! $(echo $SHELL) == $(which zsh) ]]; then
        chsh -s $(which zsh)
    fi
else
    # If zsh isn't installed, get the platform of the current machine
    platform=$(uname);
    # If the platform is Linux, try an apt to install zsh and then recurse
    if [[ $platform == 'Linux' ]]; then
	echo "installing zsh"
        sudo apt install zsh -y
        install_zsh
    # If the platform is OS X, tell the user to install zsh :)
    elif [[ $platform == 'Darwin' ]]; then
        echo "Please install zsh, then re-run this script!"
        exit
    fi
fi
}

install_zsh


###################################################

sudo apt install vim tree autojump cowsay httpie httpry maven nmap speedtest-cli unrar youtube-dl rbenv ruby-build -y
sudo apt install gstreamer0.10-plugins-ugly gxine libdvdread4 totem-mozilla icedax tagtool easytag id3tool lame nautilus-script-audio-convert libmad0 mpg321 gstreamer1.0-libav -y
sudo apt install ubuntu-restricted-extras -y

# archive management apps
sudo apt install unace unrar zip unzip p7zip-full p7zip-rar sharutils rar uudeview mpack arj cabextract file-roller -y

# clipboard management
sudo apt install xclip -y

# Jpn
sudo apt install network-manager-vpnc network-manager-vpnc-gnome -y

# clipmboard manager
sudo apt install clipit -y

# chrome
wget -q -O - https://dl-ssl.google.com/linux/linux_signing_key.pub | sudo apt-key add - 
sudo sh -c 'echo "deb http://dl.google.com/linux/chrome/deb/ stable main" >> /etc/apt/sources.list.d/google-chrome.list'
sudo apt update
sudo apt install google-chrome-stable -y

#sublime text 3
sudo add-apt-repository ppa:webupd8team/sublime-text-3 -y
sudo apt update
sudo apt install sublime-text-installer -y

# redshift
sudo apt install redshift-gtk -y

# uniti tweak tool
sudo apt install unity-tweak-tool -y

# merge pdf
sudo apt install pdfshuffler -y

sudo apt install indicator-cpufreq indicator-multiload -y

# classic menu
sudo apt-add-repository ppa:diesch/testing -y
sudo apt update && sudo apt-get install classicmenu-indicator -y

# ubuntu after install, http://www.unixmen.com/ubuntu-install-automate-installation-popular-softwares-ubuntu-14-0413-1013-0412-1012-04/
sudo add-apt-repository ppa:thefanclub/ubuntu-after-install
sudo apt update
sudo apt install ubuntu-after-install
