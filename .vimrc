" disable powerline, in favor of airline
let g:powerline_loaded = 1 

" ============= Vundle 
set nocompatible              " be iMproved, required
filetype off                  " required


" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/vundle
call vundle#begin()

" let Vundle manage Vundle, required
Plugin 'gmarik/Vundle.vim'
Plugin 'tpope/vim-fugitive'
Plugin 'rking/ag.vim'
Plugin 'shougo/neocomplete.vim'
Plugin 'sirver/ultisnips'
Plugin 'bling/vim-airline'
"Bundle 'severin-lemaignan/vim-minimap'

call vundle#end()
filetype plugin indent on


" ======================

let g:neocomplete#enable_at_startup = 1

set nocp
filetype on
filetype plugin on
syntax on

set nu
set hlsearch 
set laststatus=2

" Recomendado por vim
set tabstop=4
set softtabstop=4
set shiftwidth=4
set noexpandtab

set foldmethod=marker

set background=dark

" Si es xterm o screen, probablemente tenga 256 colores
if (&term=="xterm" || &term=="screen") || (&term=="screen-bce" || &term=="fbterm")
	let g:zenburn_high_Contrast=1
	set t_Co=256
	colorscheme zenburn
	let g:airline_theme="ubaryd"
endif


" Para que ande bien el mouse:
set ttymouse=xterm2
set mouse=a


" Para mostrar lineas de indentación
" set list
" set listchars=tab:\»\·
" set listchars=tab:\|\ 

" ================= Airline

let g:airline_powerline_fonts = 1

" OmniCompletionOptions
set completeopt=menu,menuone
let OmniCpp_NamespaceSearch=2
let OmniCpp_ShowPrototypeInAbbr=1
" let OmniCpp_MayCompleteScope =1
highlight Pmenu ctermbg=brown 
highlight PmenuSel ctermbg=red

"if &modifiable
"	startinsert
"endif

" F1 next buffer
map <f1> :bn <CR>
imap <f1> <ESC> :bn <CR>

" F2 save file
map <F2> :up <CR>
imap <F2> <ESC>:w<CR>

" F3 Open a file 
map <F3> :NERDTreeToggle<CR>
imap <F3> <ESC>:NERDTreeToggle<CR>

" Shift-F3 Open a file in a new tab
"map <S-F3> :tabnew<CR>:Ex<CR>
"imap <S-F3> <ESC>:tabnew<CR>:Ex<CR>

" F4 Open a new tab
map <F4> :tabnew <CR>
imap <F4> <ESC> :tabnew <CR>

" F5 next tab
map <F5> :tabNext<CR>
imap <F5> <ESC> :tabNext<CR>

" F6 rotates through out the frames
map <F6> <C-W>wM
imap <F6> <ESC><C-W>wM<INS>

" F10 writes and exits
map <F10> :conf q<CR>
imap <F10> <ESC>:conf q<CR>

" ,d hace un diff de revisiones
map ,d :VCSVimDiff<CR>

" ,c hace un commit
map ,c :VCSCommit<CR>

" ,u hace un update
map ,u :VCSUpdate<CR>

" -------------------
"  NeoComplCache
"  -----------------
let g:NeoComplCache_EnableAtStartup=1
let g:NeoComplCache_SmartCase=1
let g:NeoComplCache_TagsAutoUpdate=1
let g:NeoComplCache_EnableInfo=1

" --------------------
" TagList
" --------------------
" F11:  Switch on/off TagList
map <F11> :TlistToggle<CR>
imap  <F11> <ESC>:TlistToggle<CR><INS>



