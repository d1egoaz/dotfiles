{ pkgs, ... }:

{
  programs.zsh = {
    enable = true;

    initExtra = ''
      # if [[ -z "$FISH_VERSION" ]]; then
      #   exec ${pkgs.fish}/bin/fish
      # fi
    '';

    # Shell options and initialization
    initContent = ''
      # Handle dumb/tramp terminals gracefully
      if [[ $TERM == "dumb" ]]; then
        unsetopt zle
        PS1='$ '
        return
      fi
      if [[ $TERM == "tramp" ]]; then
        unsetopt zle
        PS1='[\u@\h \w]$ '
        return
      fi

      # Increase file descriptor limit (helpful for some tools)
      ulimit -n 2048

      # Use Emacs keybindings in zsh
      bindkey -e

      # Alt-key bindings for word navigation
      bindkey '^[[1;3D' backward-word
      bindkey '^[[1;3C' forward-word

      # Source private/sensitive environment variables
      [ -e ~/.zprivate ] && source ~/.zprivate
    '';
  };
}
