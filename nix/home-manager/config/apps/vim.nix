_:

{
  programs.vim = {
    enable = true;
    defaultEditor = false; # Keep emacs as default

    settings = {
      # Visual settings
      number = true;
      relativenumber = true;
      background = "dark";

      # Editor behavior
      history = 1000;
    };

    extraConfig = ''
      " File type detection
      filetype plugin indent on

      " Key mappings
      inoremap jk <ESC>

      " Visual indicators
      set colorcolumn=120
      set showcmd
      set showmatch
      set laststatus=2
      syntax on

      " Set encoding and colors
      set encoding=utf-8
      set termguicolors

      " Terminal color fix for 256-color terminals
      if &term =~ '256color'
        " disable Background Color Erase (BCE) so that color schemes
        " render properly when inside 256-color tmux and GNU screen.
        " see also http://snk.tuxfamily.org/log/vim-256color-bce.html
        set t_ut=
      endif
    '';
  };
}
