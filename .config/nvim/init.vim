""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Plugins: git clone https://github.com/VundleVim/Vundle.vim.git ~/.vim/bundle/Vundle.vim
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
set nocompatible              " be iMproved, required
filetype off                  " required

" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

Plugin 'VundleVim/Vundle.vim'
Plugin 'sonph/onehalf', {'rtp': 'vim/'}
Plugin 'vim-airline/vim-airline'
Plugin 'vim-airline/vim-airline-themes'
Plugin 'tpope/vim-sensible'
Plugin 'tpope/vim-surround'
Plugin 'sheerun/vim-polyglot'
Plugin 'scrooloose/syntastic'
Plugin 'farmergreg/vim-lastplace'
"Plugin 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
"Plugin 'tpope/vim-fugitive'
Plugin 'airblade/vim-gitgutter'
Plugin 'scrooloose/nerdcommenter'
"Plugin 'preservim/nerdtree'

" All of your Plugins must be added before the following line
call vundle#end()            " required
filetype plugin indent on    " required


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Custom: custom vimrc settings
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

autocmd FileType * setlocal formatoptions-=cro

" Enable filetype plugins
filetype plugin on
filetype indent on

if exists('+termguicolors')
	let &t_8f = "\<Esc>[38;2;%lu;%lu;%lum"
	let &t_8b = "\<Esc>[48;2;%lu;%lu;%lum"
	set termguicolors
endif

set t_Co=256
try
	colorscheme onehalfdark
	let g:airline_theme='onehalfdark'
catch
endtry

" Set 3 lines to the cursor - when moving vertically using j/k
set so=3

" Highlight current line
set cursorline

" With a map leader it's possible to do extra key combinations
" like <leader>w saves the current file
let mapleader = ","
let g:mapleader = ","
let maplocalleader = ";"
let g:maplocalleader = ";"

" When searching try to be smart about cases
set ignorecase
set smartcase

" Tabs
set tabstop=4
set shiftwidth=0

" Turn backup off, since most stuff is in SVN, git et.c anyway...
set writebackup

" Turn on line numbers
set number

" Disable scratch window on complete preview
set completeopt-=preview

" Disable switchbuf new tab
set switchbuf-=newtab

" Enable mouse support
set mouse=a

" Highlight all search matches
set hlsearch

" Disable mouse support
"autocmd BufEnter * set mouse=

" :W sudo saves the file
" (useful for handling the permission-denied error)
"command! W w !sudo tee % > /dev/null


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Mappings: custom mappings
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Buffers navigation
nmap <C-b><right> :bn<CR>
nmap <C-b><left> :bp<CR>

" Close buffer
nmap <C-x> :bp\|bd #<CR>

" Disable command history window
nnoremap q: <NOP>

" Fast resizing
nmap <C-w>> :vertical resize +8<CR>
nmap <C-w>< :vertical resize -8<CR>

" Map <ESC> and Enter to disable search highlight
nnoremap <ESC> :noh<CR><C-L>
nnoremap <CR> :noh<CR><C-L>

" Press F4 to toggle highlighting on/off, and show current value.
noremap <F4> :set hlsearch! hlsearch?<CR>

" Add new line from Normal mode
"nmap <CR> O<Esc><down>

" Toggle paste mode
set pastetoggle=<F2>


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Plugins: misc plugins options
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" -------------------------
" NERDTree
" -------------------------
autocmd StdinReadPre * let s:std_in=1
autocmd VimEnter * if argc() == 0 && !exists("s:std_in") | NERDTree | endif

" -------------------------
" deoplete
" -------------------------
let g:deoplete#enable_at_startup = 1

" -------------------------
" Syntastic
" -------------------------
set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*
let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0

" -------------------------
" The NERD Commenter
" -------------------------
nnoremap <C-_> :call NERDComment(0,"toggle")<C-m>
vnoremap <C-_> :call NERDComment(0,"toggle")<C-m>
" Add spaces after comment delimiters by default
let g:NERDSpaceDelims = 0
" Use compact syntax for prettified multi-line comments
let g:NERDCompactSexyComs = 1
" Align line-wise comment delimiters flush left instead of following code indentation
let g:NERDDefaultAlign = 'left'
" Set a language to use its alternate delimiters by default
let g:NERDAltDelims_java = 1
" Add your own custom formats or override the defaults
let g:NERDCustomDelimiters = { 'c': { 'left': '/**','right': '*/' } }
" Allow commenting and inverting empty lines (useful when commenting a region)
let g:NERDCommentEmptyLines = 1
" Enable trimming of trailing whitespace when uncommenting
let g:NERDTrimTrailingWhitespace = 1
" Enable NERDCommenterToggle to check all selected lines is commented or not
let g:NERDToggleCheckAllLines = 1
