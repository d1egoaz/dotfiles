#!/usr/bin/env zsh

# in fino veritas
 
# Borrowing shamelessly from these oh-my-zsh themes:
#   fino-time
#   pure
#   https://gist.github.com/smileart/3750104

# Set required options
#
setopt prompt_subst

# Load required modules
#
autoload -Uz vcs_info

# Set vcs_info parameters
#
zstyle ':vcs_info:*' enable hg bzr git
zstyle ':vcs_info:*:*' unstagedstr '!'
zstyle ':vcs_info:*:*' stagedstr '+'
zstyle ':vcs_info:*' check-for-changes true


# List of vcs_info format strings:
#
# %b => current branch
# %a => current action (rebase/merge)
# %s => current version control system
# %r => name of the root directory of the repository
# %S => current path relative to the repository root directory
# %m => in case of Git, show information about stashes
# %u => show unstaged changes in the repository
# %c => show staged changes in the repository

zstyle ':vcs_info:*:*' formats "%b"
zstyle ':vcs_info:*:*' actionformats "%b"
zstyle ':vcs_info:*:*' nvcsformats ""

# Fastest possible way to check if repo is dirty
#
git_dirty() {
    # Check if we're in a git repo
    command git rev-parse --is-inside-work-tree &>/dev/null || return
    # Check if it's dirty
    command git diff --quiet --ignore-submodules HEAD &>/dev/null; [ $? -eq 1 ] && echo "true" && return
    # We're in a git repo but we're clean
    echo "false"
}

function pluralize() {
  if [[ "$1" == "1" ]]; then
    echo "$2"
  else
    echo "$2s"
  fi
}

# Display information about the current repository
#
repo_info() {
    GST=""
    if [[ "$1" == "verbose" ]]; then
      local git_is_dirty=$(git_dirty)

      if [[ "$git_is_dirty" != "" ]]; then
         INDEX=$(command git status --porcelain -b 2> /dev/null)

         local untracked="$(command echo "$INDEX" | grep -E '^\?\? ' | wc -l | sed 's/^ *//' 2> /dev/null)"
         if [[ "$untracked" != "0" ]]; then
           GST="%{$FG[239]%}, $untracked $(pluralize $untracked 'file') untracked$GST"
         fi

         if [[ "$git_is_dirty" == "false" ]]; then
            GST="%{$FG[040]%}✔$GST"
         elif [[ "$git_is_dirty" == "true" ]]; then

            local changed="$(command git diff --shortstat 2> /dev/null)"

            if [[ "$changed" != "" ]]; then
              GST="%{$FG[239]%},$changed$GST"
            fi

            local added="$(command echo "$INDEX" | grep '^A  ' | wc -l | sed 's/^ *//' 2> /dev/null)"
            if [[ "$added" != "0" ]]; then
               GST="%{$FG[239]%}, $added $(pluralize $added 'file') added$GST"
            fi

            GST="%{%F{red}%}✘%{$FG[239]%}$GST"
         fi

         if [[ "$GST" != "" ]]; then
           GST=": %{$FG[239]%}$GST%f"
         fi
      fi
    fi

    [[ "$vcs_info_msg_0_" != "" ]] && echo "\n( %F{blue}${vcs_info_msg_0_%%/.}%f$GST"
}

# In case you need to do something
#
preexec() {
}

# Output additional information about paths, repos etc
#
precmd() {
    vcs_info # Get version control info before we start outputting stuff
    #if you remove "verbose" from the repo_info call it will not show detailed info on changes
    print -P "\n╭─[%{$FG[039]%}%*%f] $(user_info)%{$terminfo[bold]%}${PWD/#$HOME/ ~}$(repo_info verbose)%f"
}

user_info() {
    #Uncomment if you only want user & host to be shown when using SSH
    if [[ -n $SSH_TTY ]]; then
        echo "%{$FG[033]%}%n%f%{$FG[239]%}@%f%{$FG[033]%}%m%f%{$FG[239]%}/%f"
    fi
}

# List of prompt format strings:
#
# prompt:
# %F => color dict
# %f => reset color
# %~ => current path
# %* => time
# %n => username
# %m => shortname host
# %(?..) => prompt conditional - %(condition.true.false)

# Define prompts

PROMPT="╰─%(?.%{%F{green}%}○.%{%F{red}%}✘)%f "

# ------------------------------------------------------------------------------
