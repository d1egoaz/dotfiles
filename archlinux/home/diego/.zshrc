ZSH=$HOME/.oh-my-zsh

# Uncomment to change how often before auto-updates occur? (in days)
export UPDATE_ZSH_DAYS=7

plugins=(git autojump urltools vi-mode history-substring-search)
if [ $EMACS ] || [ $INSIDE_EMACS ]; then
  ZSH_THEME="simple"
else
  ZSH_THEME="in-fino-veritas"
fi

if [[ "$TERM" == "dumb" ]]; then
  unset zle_bracketed_paste
  unset zle
  PS1='$ '
  return
fi

source $ZSH/oh-my-zsh.sh
source ~/.config/fzf/base16-tomorrow-night.config
test -e ~/.fzf.zsh && source ~/.fzf.zsh

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

if [[ "$OSTYPE" =~ ^linux-gnu ]]; then
  if ! pgrep -u "$USER" ssh-agent > /dev/null; then
      ssh-agent > ~/.cache/.ssh-agent-thing
  fi
  if [[ "$SSH_AGENT_PID" == "" ]]; then
      eval "$(<~/.cache/.ssh-agent-thing)"
  fi
fi

function ediff() {
  if [ "X${2}" = "X" ]; then
    echo "USAGE: ediff <FILE 1> <FILE 2>"
  else
    # The --eval flag takes lisp code and evaluates it with EMACS
    emacs --eval "(ediff-files \"$1\" \"$2\")"
  fi
}

[ -f /opt/dev/dev.sh ] && source /opt/dev/dev.sh

[ -f /usr/local/bin/kubectl ] && source <(kubectl completion zsh)

[ -f ~/.local/share/lscolors.sh ] && source ~/.local/share/lscolors.sh

eval "$(ssh-agent)"

# similar to https://github.com/edenhill/kafkacat/issues/209 to make kafkacat happy
ulimit -n 2048
