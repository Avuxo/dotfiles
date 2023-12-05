" show line numbers
set number
set numberwidth=2

" show matching parenthesis
set showmatch

" 4 spaces no tabs
set expandtab
set tabstop=4

" autocomplete for quotes n such
inoremap " ""<ESC>i
inoremap ' ''<ESC>i
inoremap ( ()<ESC>i
inoremap { {}<ESC>i

"monokai color scheme"
syntax enable
colorscheme monokai
