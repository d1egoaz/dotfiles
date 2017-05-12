# Path to your oh-my-zsh configuration.
ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
ZSH_THEME="in-fino-veritas"

# Uncomment to change how often before auto-updates occur? (in days)
export UPDATE_ZSH_DAYS=7

if [ $EMACS ]; then
  unsetopt zle # disables zsh line editor
  plugins=(git autojump history-substring-search)
else
  plugins=(archlinux git scala autojump urltools vi-mode history-substring-search zsh-syntax-highlighting)
fi

source $ZSH/oh-my-zsh.sh

bindkey '^k' history-substring-search-up
bindkey '^j' history-substring-search-down

# Customize to your needs...
DIRSTACKDIR="$HOME/.cache/zsh"
DIRSTACKFILE="$DIRSTACKDIR/dirs"
if [ ! -d "$DIRECTORY" ]; then
  mkdir -p $DIRSTACKDIR #Se crea el dir y archivo si no existe
  touch $DIRSTACKFILE
fi
if [[ -f $DIRSTACKFILE ]] && [[ $#dirstack -eq 0 ]]; then
     dirstack=( ${(f)"$(< $DIRSTACKFILE)"} )
      [[ -d $dirstack[1] ]] && cd $dirstack[1]
  fi
  chpwd() {
    print -l $PWD ${(u)dirstack} >$DIRSTACKFILE
}

DIRSTACKSIZE=20

setopt autopushd pushdsilent pushdtohome

## Remove duplicate entries
setopt pushdignoredups

### This reverts the +/- operators.
setopt pushdminus

# disable history expansion for commands
unsetopt histverify

# load aliases
test -e ~/.aliases && source ~/.aliases
test -e ~/.aliases_hs && source ~/.aliases_hs

# RBENV
#eval "$(rbenv init -)"
#source ~/.nvm/nvm.sh
#nvm use 0.12.9
# echo ">>>"
# echo "nvm is disabled!"
