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
export GOPATH="$HOME/go"
PATH="$PATH:$GOPATH/bin"

# ruby
PATH="$HOME/.nvm/versions/node/v8.9.4/bin:$HOME/.gem/ruby/2.4.4/bin:/opt/rubies/2.4.4/lib/ruby/gems/2.4.0/bin:/opt/rubies/2.4.4/bin:$PATH"

if [[ "$OSTYPE" =~ ^linux-gnu ]]; then
    PATH="/home/diego/.local/bin:$PATH"
fi
export PATH

# ansible
export ANSIBLE_TRANSPORT="ssh"
export ANSIBLE_SSH_ARGS="-o ControlMaster=auto -o ControlPersist=60s -o ControlPath=/tmp/a-%h-%p-%r"
export ANSIBLE_NOCOWS=1

export GTAGSLABEL="ctags"

export EDITOR="em"
export VISUAL="em"

export SSH_AUTH_SOCK="$XDG_RUNTIME_DIR/ssh-agent.socket"
