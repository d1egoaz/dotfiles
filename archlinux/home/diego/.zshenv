# Prefer US English and use UTF-8
export LANG="en_US.UTF-8"
export LC_ALL="en_US.UTF-8"

# JAVA
if [[ "$OSTYPE" =~ ^linux-gnu ]]; then
  JAVA_HOME="/usr/lib/jvm/default"
else
  JAVA_HOME="/Library/Java/JavaVirtualMachines/jdk1.8/Contents/Home"
fi
export JAVA_HOME
export JAVA_OPTS="-Dfile.encoding=UTF8 -Dscala.color"

# PATH
PATH="$PATH:/usr/bin:/bin:/usr/sbin:/sbin:/usr/local/bin:/usr/local/sbin"
PATH="/usr/local/bin:$PATH"
PATH="$PATH:$JAVA_HOME/bin"

# go
export GOPATH="$HOME"
export GOROOT="$HOME"
PATH="$PATH:$GOPATH/bin"

# ruby
PATH="$HOME/.nvm/versions/node/v8.9.4/bin:$HOME/.gem/ruby/2.4.4/bin:/opt/rubies/2.4.4/lib/ruby/gems/2.4.0/bin:/opt/rubies/2.4.4/bin:$PATH"

if [[ "$OSTYPE" =~ ^linux-gnu ]]; then
    PATH="/home/diego/.local/bin:$PATH"
fi
export PATH

export EDITOR="emacsclient -t"                  # $EDITOR opens in terminal
export VISUAL="emacsclient -c -a emacs"         # $VISUAL opens in GUI mode

export EA_EDITOR='/Applications/Emacs.app/Contents/MacOS/bin/emacsclient -a "" -c'

export SSH_AUTH_SOCK="$XDG_RUNTIME_DIR/ssh-agent.socket"

export TERMINAL=urxvt256c
