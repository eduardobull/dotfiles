""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Plugins: install plugins
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Load The Ultimate vimrc Basic - github.com/amix/vimrc
so ~/.vim/amix-vimrc.vim

" Load plugins before everything else
set nocompatible              " be iMproved, required
filetype off                  " required

" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

" let Vundle manage Vundle, required
Plugin 'gmarik/Vundle.vim'
Plugin 'tpope/vim-fugitive'
"Plugin 'airblade/vim-gitgutter'
Plugin 'scrooloose/nerdtree'
Plugin 'vim-airline/vim-airline'
Plugin 'Valloric/YouCompleteMe' "cd ~/.vim/bundle/YouCompleteMe && sudo apt-get install build-essential cmake python-dev python3-dev && ./install.py
Plugin 'scrooloose/syntastic'
Plugin 'ctrlpvim/ctrlp.vim'
Plugin 'majutsushi/tagbar'
Plugin 'tpope/vim-surround'
Plugin 'Raimondi/delimitMate'
Plugin 'rking/ag.vim' "sudo apt-get install silversearcher-ag

" Go
Plugin 'fatih/vim-go'

" Clojure
Plugin 'guns/vim-sexp'
Plugin 'tpope/vim-fireplace'
Plugin 'guns/vim-clojure-static'
"Plugin 'guns/vim-clojure-highlight'
Plugin 'venantius/vim-cljfmt'
"Plugin 'venantius/vim-eastwood'

" JavaScript
Plugin 'pangloss/vim-javascript'
Plugin 'maksimr/vim-jsbeautify'
Plugin 'einars/js-beautify'
Plugin 'othree/javascript-libraries-syntax.vim'

" Themes
Plugin 'fatih/molokai'
Plugin 'chriskempson/vim-tomorrow-theme'

call vundle#end()            " required
filetype plugin indent on    " required


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Plugins Config: custom plugins configurations
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" rking/ag.vim
let g:ag_mapping_message=0
let g:ag_qhandler = "botright copen 5"
let g:ag_lhandler = "botright copen 5"
nmap <c-f> :Ag<space>

" vim-airline/vim-airline
let g:airline#extensions#tabline#enabled = 1

" Valloric/YouCompleteMe
let g:ycm_min_num_of_chars_for_completion = 3
let g:ycm_seed_identifiers_with_syntax = 1
let g:ycm_complete_in_strings = 0

" kien/ctrlp.vim
let g:ctrlp_map = '<c-p>'
let g:ctrlp_cmd = 'CtrlPMixed'
let g:ctrlp_working_path_mode = 'ra'

" vim-go
let g:go_fmt_command = "goimports"
let g:go_fmt_fail_silently = 1
let g:go_highlight_functions = 1
let g:go_highlight_methods = 1
let g:go_highlight_structs = 1
let g:go_highlight_operators = 1
let g:go_highlight_build_constraints = 1

" vim-clojure-static
let g:clojure_align_subforms = 1
let g:clojure_align_multiline_strings = 1

" vim-clojure-highlight
"autocmd BufRead *.clj try | silent! Require | catch /^Fireplace/ | endtry

" vim-javascript
let g:javascript_enable_domhtmlcss = 1

" javascript-libraries-syntax.vim
let g:used_javascript_libs = 'jquery,angularjs,angularui,angularuirouter'

" Raimondi/delimitMate
au FileType *.clj let b:delimitMate_autoclose = 0 
au FileType *.cls let b:delimitMate_autoclose = 0 
let g:delimitMate_expand_cr = 1

" majutsushi/tagbar
nmap <F8> :TagbarToggle<CR>
let g:tagbar_autofocus = 0
let g:tagbar_expand = 0

" scrooloose/syntastic
set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*
let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 1
let g:syntastic_go_checkers = ['golint', 'govet', 'errcheck']
let g:syntastic_mode_map = { 'mode': 'active', 'passive_filetypes': ['go'] }

" scrooloose/nerdtree
let g:NERDTreeShowBookmarks = 1
autocmd StdinReadPre * let s:std_in=1
" Open a NERDTree automatically when vim starts up if no files were specified
autocmd VimEnter * if argc() == 0 && !exists("s:std_in") | NERDTree | endif 
" Close vim if the only window left open is a NERDTree
"autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTree") && b:NERDTree.isTabTree()) | q | endif 


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

" :W sudo saves the file 
" (useful for handling the permission-denied error)
command! W w !sudo tee % > /dev/null

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Mappings: custom mappings
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Buffers navigation
nmap <c-b><right> :bn<cr>
nmap <c-b><left> :bp<cr>

" Disable command history window
nnoremap q: <NOP>

" UnMap <Space> to / (search) and Ctrl-<Space> to ? (backwards search)
unmap <space>
unmap <c-space>

" Fast resizing
map <c-w>> :vertical resize +8<cr>
map <c-w>< :vertical resize -8<cr>

" Map <C-space> to disable search highlight
map <NUL> :noh<cr>

" Add new line from Normal mode
nmap <S-Enter> O<Esc>
nmap <CR> o<Esc>
