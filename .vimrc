set nocompatible

let g:racer_cmd = "/home/epixors/.cargo/bin/racer"

let g:racer_experimental_completer = 1
let g:netrw_liststyle = 3
let g:netrw_banner = 0
let g:netrw_browse_split = 3
let g:netrw_winsize = 25
let g:netrw_list_hide = '.*\.swp$,\~$,\.orig$'

execute pathogen#infect()

filetype plugin on
syntax enable

set hidden

set path+=**
set wildignore+=node_modules/**
set wildignore+=vendor/**
set wildignore+=contrib/**
set wildmenu

set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*

let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0

let base16colorspace=256  " Access colors present in 256 colorspace
colorscheme base16-default-dark

augroup Racer
    autocmd!
    autocmd FileType rust nmap <buffer> gd         <Plug>(rust-def)
    autocmd FileType rust nmap <buffer> gs         <Plug>(rust-def-split)
    autocmd FileType rust nmap <buffer> gx         <Plug>(rust-def-vertical)
    autocmd FileType rust nmap <buffer> <leader>gd <Plug>(rust-doc)
augroup END

nnoremap <silent> <leader>] :cnext<CR>  
nnoremap <silent> <leader>[ :cprevious<CR>
nnoremap <silent> <leader>p :lcd %:p:h<CR>
