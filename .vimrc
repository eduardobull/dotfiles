""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Custom: custom vimrc settings
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

autocmd FileType * setlocal formatoptions-=cro

" Enable filetype plugins
filetype plugin on
filetype indent on

" Set to auto read when a file is changed from the outside
set autoread

" Color scheme
set t_Co=256

set background=dark
try
    colorscheme molokai
catch
endtry

" Fix background
if &term =~ '256color'
  " disable Background Color Erase (BCE) so that color schemes
  " render properly when inside 256-color tmux and GNU screen.
  " see also http://snk.tuxfamily.org/log/vim-256color-bce.html
  set t_ut=
endif


" Set 3 lines to the cursor - when moving vertically using j/k
set so=3

" With a map leader it's possible to do extra key combinations
" like <leader>w saves the current file
let mapleader = ","
let g:mapleader = ","
let maplocalleader = ";"
let g:maplocalleader = ";"

" When searching try to be smart about cases
set nosmartcase

" Turn backup off, since most stuff is in SVN, git et.c anyway...
set writebackup

" Height of the command bar
set cmdheight=1

" Add a bit extra margin to the left
set foldcolumn=0

" Turn on line numbers
set number

" Disable scratch window on complete preview
set completeopt-=preview

" Disable switchbuf new tab
set switchbuf-=newtab

" Disable mouse support
autocmd BufEnter * set mouse=

" :W sudo saves the file
" (useful for handling the permission-denied error)
"command! W w !sudo tee % > /dev/null

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Mappings: custom mappings
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Buffers navigation
nmap <C-b><right> :bn<cr>
nmap <C-b><left> :bp<cr>

" Close buffer
nmap <C-x> :bp\|bd #<cr>

" Disable command history window
nnoremap q: <NOP>

" Fast resizing
nmap <C-w>> :vertical resize +8<cr>
nmap <C-w>< :vertical resize -8<cr>

" Map <C-space> to disable search highlight
nnoremap <NUL> :noh<cr>

" Add new line from Normal mode
nmap <S-Enter> O<Esc>
nmap <CR> o<Esc>

" Toggle paste mode
set pastetoggle=<F2>
