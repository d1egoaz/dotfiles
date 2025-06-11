{ ... }:

{
  programs.fzf = {
    enable = true;
    tokyonight.enable = true;
    defaultOptions = [
      "--exact"
      "--height 30%"
      "--no-preview"
      "--layout reverse"
      "--multi"
      "-0"
      "--no-info"
      "--pointer ●"
      "--color gutter:-1,pointer:#00ff00"
    ];
    defaultCommand = "fd --type f --hidden --follow --exclude .git";
    changeDirWidgetCommand = "fd --type f --hidden --follow --exclude .git";
  };
}
