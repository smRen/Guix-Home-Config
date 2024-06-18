"" Install vim plug
let autoload_plug_path = stdpath('data') . '/site/autoload/plug.vim'
if !filereadable(autoload_plug_path)
  silent execute '!curl -fLo ' . autoload_plug_path . '  --create-dirs
      \ "https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim"'
  autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif
unlet autoload_plug_path

"" Plugins
call plug#begin()

"" Surround objects with other stuff
Plug 'tpope/vim-surround'

""Comment lines filetype specific
Plug 'tpope/vim-commentary'

"" Snippets
" Plug 'SirVer/ultisnips'
Plug 'honza/vim-snippets'

"" Vertical Line indentation
" Plug 'Yggdroot/indentLine'

"" Highlight same objects/words
Plug 'RRethy/vim-illuminate'

"" Better syntax highlighting
Plug 'sheerun/vim-polyglot'

"" Theming
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'edkolev/tmuxline.vim'
Plug 'ayu-theme/ayu-vim'
Plug 'joshdick/onedark.vim'
Plug 'morhetz/gruvbox'
Plug 'challenger-deep-theme/vim', { 'as': 'challenger-deep' }
Plug 'tyrannicaltoucan/vim-deep-space'
Plug 'danilo-augusto/vim-afterglow'

"" Fuzzy finder
Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'junegunn/fzf.vim'

" Warping through text
Plug 'easymotion/vim-easymotion' " <leader> <leader> w

"" Vim-Slime -> Send text to other panel
Plug 'jpalardy/vim-slime'

call plug#end()

"" Remappings
" remap leader key to comma 
let mapleader = ","
let maplocalleader = "\\"

"Turn off search highlight with esc 
noremap <silent><esc> <esc>:noh<CR><esc>   

"" Theming
" colorscheme gruvbox
" colorscheme onedark
colorscheme challenger_deep
"let ayucolor="dark"
" set background=dark

"" IndentLine stuff
" let g:indentLine_char = '┆'
" let g:indentLine_first_char = '┆'
" let g:indentLine_showFirstIndentLevel = 1
" let g:indentLine_setColors = 0

"" General Config
" let g:python3_host_prog = "/home/smakey18/.pyenv/versions/neovim/bin/python" 
filetype plugin indent on                   " File specific settings (like number of spaces in tabs) 
syntax enable                               " Nice looking colors
set nohls                                   " No search highlight
set number                                  " Absolute number
set ruler    
set termguicolors                           " enable true colors support
set lazyredraw
set smarttab                                " set tabs for a shifttabs logic    
set expandtab                               " expand tabs into spaces    
set autoindent                              " indent when moving to the next line while writing code
"set cursorline                              " shows line under the cursor's line
"hi clear CursorLine
"hi CursorLine gui=underline cterm=underline
hi link illuminatedWord Visual
set showmatch                               " shows matching part of bracket pairs (), [], {}
"set scrolloff=3                            " let 10 lines before/after cursor during scroll
set clipboard+=unnamedplus                       " use system clipboard
set undofile                                " maintain undo history   
set undodir=~/.config/nvim/undodir	    " undo directory
set mouse=a				    " enable mouse
set splitbelow                              " split goes below
set splitright                              " split goes right
set wildmode=longest,full,list              " Menu for tab complete files

"" Airline Configs
let g:tmuxline_powerline_separators = 1
let g:airline_powerline_fonts = 1
let g:airline_theme='ayu'
let g:airline#extensions#tabline#enabled = 1
let g:airline#extensions#tabline#buffer_nr_show = 1
let g:airline#extensions#tabline#ignore_bufadd_pat = 'defx|gundo|nerd_tree|startify|tagbar|undotree|vimfiler'

"" Netrw 
let g:netrw_banner = 0
let g:netrw_liststyle = 3
let g:netrw_browse_split = 4
let g:netrw_altv = 1
let g:netrw_winsize = 25
let g:NetrwIsOpen=0
function! ToggleNetrw()
    if g:NetrwIsOpen
        let i = bufnr("$")
        while (i >= 1)
            if (getbufvar(i, "&filetype") == "netrw")
                silent exe "bwipeout " . i 
            endif
            let i-=1
        endwhile
        let g:NetrwIsOpen=0
    else
        let g:NetrwIsOpen=1
        silent Lexplore
    endif
endfunction
noremap <silent> <F2> :call ToggleNetrw()<CR>

" Terminal stuff
" Launch terminal below
nnoremap <F1> :10sp term://zsh <CR>i
" Escape terminal with escape
tnoremap <Esc> <C-\><C-n>		      	
" CTRL-R paste in insert mode inside terminal
tnoremap <expr> <M-R> '<C-\><C-N>"'.nr2char(getchar()).'pi'
" Show job id
nnoremap <leader><C-i> :echo b:terminal_job_id <CR>

"" Vim-slime config
let g:slime_target = "neovim"
let g:slime_python_ipython = 1

"" Buffer/Window shortcut
nnoremap <C-W><leader>q :bd! <CR>
nnoremap <leader>n :bn <CR>
nnoremap <leader>p :bp! <CR>
" Movement between windows
tnoremap <M-h> <C-\><C-N><C-w>h
tnoremap <M-j> <C-\><C-N><C-w>j
tnoremap <M-k> <C-\><C-N><C-w>k
tnoremap <M-l> <C-\><C-N><C-w>l
inoremap <M-h> <C-\><C-N><C-w>h
inoremap <M-j> <C-\><C-N><C-w>j
inoremap <M-k> <C-\><C-N><C-w>k
inoremap <M-l> <C-\><C-N><C-w>l
nnoremap <M-h> <C-w>h
nnoremap <M-j> <C-w>j
nnoremap <M-k> <C-w>k
nnoremap <M-l> <C-w>l

"" FZF
nnoremap <C-p> :Files <CR>
nnoremap <M-p> :Buffers <CR>
let FZF_DEFAULT_COMMAND='rg --hidden --no-ignore -l ""'


function! AutoSaveWinView()
    if !exists("w:SavedBufView")
        let w:SavedBufView = {}
    endif
    let w:SavedBufView[bufnr("%")] = winsaveview()
endfunction

" Restore current view settings.
function! AutoRestoreWinView()
    let buf = bufnr("%")
    if exists("w:SavedBufView") && has_key(w:SavedBufView, buf)
        let v = winsaveview()
        let atStartOfFile = v.lnum == 1 && v.col == 0
        if atStartOfFile && !&diff
            call winrestview(w:SavedBufView[buf])
        endif
        unlet w:SavedBufView[buf]
    endif
endfunction

" When switching buffers, preserve window view.
if v:version >= 700
    autocmd BufLeave * call AutoSaveWinView()
    autocmd BufEnter * call AutoRestoreWinView()
endif
