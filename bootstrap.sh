#!/bin/bash

echo "Installing brew..."
#/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
brew update
brew upgrade
sleep 1
echo "Success! Brew is installed."


echo "Installing latest zsh and friends"
brew install zsh
brew install starship
sleep 1
echo "Success! zsh and friends are installed."


echo "Installing other tools..."
brew install git-crypt
brew install gpg
brew install pass
brew install pinentry-mac
brew install tree
brew install stow
brew install dua-cli # disk usage analyzer

echo "Installing fonts..."
brew tap homebrew/cask-fonts
# TODO: install sf pro and sf mono from https://developer.apple.com/fonts/

echo "Installing terminal helpers..."
brew install coreutils
brew install bash # needed by one of the tmux plugins
brew install tmux
brew install fzf
brew install autojump
brew install eza
brew install bat
brew install progress

echo "Installing dev tools ..."
brew install go
go install golang.org/x/tools/cmd/goimports@latest
go install golang.org/x/tools/gopls@latest
brew install rust
brew install jq
brew install stern
brew install ripgrep
brew install fd
brew install prettier
brew install mermaid-cli
brew install gum

# k8s
brew install kind

echo "Install only tool that I'm going to use: Emacs ❤️ ..."

# for emacs
brew install cmake
brew install aspell --language en,es
brew install languagetool
brew install vale
brew install proselint
brew install pandoc
brew install shellcheck
brew install shfmt
brew install universal-ctags
brew install libvterm

echo "Installing lsp servers ..."
brew install yaml-language-server
brew install bash-language-server
# brew install rust-analyzer
brew install gopls
npm install -g typescript-language-server typescript


echo "chime"
brew install rbenv
brew install helm
brew install argocd
brew install tfenv
brew install cdktf
brew install yarn
