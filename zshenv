export EDITOR="vim"

# Prefer US English and use UTF-8
export LANG="en_US.UTF-8"
export LC_ALL="en_US.UTF-8"

# JAVA
if [[ "$OSTYPE" =~ ^linux-gnu ]]; then
  export JAVA_HOME="/usr/lib/jvm/default"
else
  export JAVA_HOME="/Library/Java/JavaVirtualMachines/jdk1.8/Contents/Home"
fi
export JAVA_OPTS="-Dfile.encoding=UTF8 -Dscala.color"

# PATH
PATH=$PATH:/usr/bin:/bin:/usr/sbin:/sbin:/usr/local/bin:/usr/local/sbin
PATH="/usr/local/bin:$PATH"
PATH="$PATH:$JAVA_HOME/bin"

# go
export PATH=$PATH:/usr/local/go/bin
export GOPATH=$HOME/go
export PATH=$PATH:$GOPATH/bin

if [[ "$OSTYPE" =~ ^linux-gnu ]]; then
    PATH="/home/diego/.local/bin:$PATH"
fi
export PATH

# ansible
export ANSIBLE_TRANSPORT=ssh
export ANSIBLE_SSH_ARGS="-o ControlMaster=auto -o ControlPersist=60s -o ControlPath=/tmp/a-%h-%p-%r"
export ANSIBLE_NOCOWS=1
