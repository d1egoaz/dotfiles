if [[ "$TERM" == "dumb" ]]
then
  unsetopt zle
  unsetopt prompt_cr
  unsetopt prompt_subst
  unfunction precmd
  unfunction preexec
  PS1='$ '
fi
[[ $TERM == "dumb" ]] && unsetopt zle && PS1='$ ' && return

# Path to your oh-my-zsh configuration.
ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
ZSH_THEME="in-fino-veritas"

# Set to this to use case-sensitive completion
# CASE_SENSITIVE="true"

# Comment this out to disable bi-weekly auto-update checks
# DISABLE_AUTO_UPDATE="true"

# Uncomment to change how often before auto-updates occur? (in days)
export UPDATE_ZSH_DAYS=7

# Uncomment following line if you want red dots to be displayed while waiting for completion
COMPLETION_WAITING_DOTS="true"

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
plugins=(git ruby brew scala sublime gitignore jruby autojump urltools git-flow docker boot2docker battery)

source $ZSH/oh-my-zsh.sh

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

# load exports
test -e ~/.exports && source ~/.exports

# load aliases
test -e ~/.aliases && source ~/.aliases

# RBENV
#eval "$(rbenv init -)"

#source ~/.nvm/nvm.sh
#nvm use 0.12.9
     echo
echo ">>>"
echo "nvm is disabled!"
