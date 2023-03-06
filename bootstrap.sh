#!/bin/bash

echo "Installing brew..."
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
brew update
brew upgrade
sleep 1
echo "Success! Brew is installed."


echo "Installing latest zsh and oh-my-zsh"
brew install zsh
sh -c "$(curl -fsSL https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/tools/install.sh)"
brew install romkatv/powerlevel10k/powerlevel10k
sleep 1
echo "Success! zsh and friends are installed."


echo "Installing other tools..."
brew install git-crypt
brew install gpg
brew install pinentry-mac
brew install tree
brew install stow

echo "Installing fonts..."
brew tap homebrew/cask-fonts
brew install font-iosevka-ss08
# TODO: install sf pro and sf mono from https://developer.apple.com/fonts/

echo "Installing terminal helpers..."
brew install coreutils
brew install bash # needed by one of the tmux plugins
brew install tmux
brew install fzf
brew install autojump
brew install exa
brew install bat

echo "Installing dev tools ..."
brew install go
brew install jq
brew install stern
brew install ripgrep
brew install fd
brew install prettier
brew install mermaid-cli
brew install gum

echo "Install only tool that I'm going to use: Emacs ❤️ ..."
brew tap d12frosted/emacs-plus
brew update

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

echo "Installing lsp servers ..."
brew install yaml-language-server
brew install bash-language-server
brew install rust-analyzer
brew install gopls
npm install -g typescript-language-server typescript
