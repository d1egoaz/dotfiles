set number
syntax enable
set background=dark
colorscheme gruvbox
nnoremap <leader>y :call system('nc -U ~/.local/share/clipper/clipper.sock', @0)<CR>
" set mouse=a

if &term =~ '256color'
  " disable Background Color Erase (BCE) so that color schemes
  " render properly when inside 256-color tmux and GNU screen.
  " see also http://snk.tuxfamily.org/log/vim-256color-bce.html
  set t_ut=
endif
