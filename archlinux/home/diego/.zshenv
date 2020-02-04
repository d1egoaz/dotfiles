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
PATH="/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:/usr/local/sbin:$PATH"

# go
export GOROOT=$(go env GOROOT)
export GOPATH="$HOME/go:$HOME:$HOME/code/go"
export GOPRIVATE="github.com/Shopify/*"
PATH="$PATH:$HOME/go/bin"

# ruby
PATH="$HOME/.nvm/versions/node/v8.9.4/bin:$HOME/.gem/ruby/2.4.4/bin:/opt/rubies/2.4.4/lib/ruby/gems/2.4.0/bin:/opt/rubies/2.4.4/bin:$PATH"

export PATH

export SSH_AUTH_SOCK="$XDG_RUNTIME_DIR/ssh-agent.socket"

export EDITOR="emacsclient -t"                  # $EDITOR opens in terminal
export VISUAL="emacsclient -c -a emacs"         # $VISUAL opens in GUI mode
export TERMINAL=rxvt
export EA_EDITOR=$VISUAL
export NIX_SSL_CERT_FILE="$HOME/.nix-profile/etc/ssl/certs/ca-bundle.crt"
