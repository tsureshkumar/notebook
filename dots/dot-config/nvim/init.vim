""" adopted from https://github.com/mikepqr/dotfiles/blob/main/vim/.config/nvim/init.vim

call plug#begin(stdpath('data') . '/vimplug')
Plug 'Vimjas/vim-python-pep8-indent'
Plug 'ap/vim-buftabline'
Plug 'aymericbeaumet/vim-symlink'
Plug 'christoomey/vim-tmux-navigator'
Plug 'cocopon/iceberg.vim'
Plug 'editorconfig/editorconfig-vim'
Plug 'greymd/oscyank.vim'
Plug 'junegunn/fzf'
Plug 'junegunn/fzf.vim'
Plug 'justinmk/vim-dirvish'
Plug 'mbbill/undotree'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-obsession'
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-rhubarb'
Plug 'tpope/vim-sensible'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-unimpaired'
Plug 'junegunn/vim-easy-align'
Plug 'dense-analysis/ale'

Plug 'jose-elias-alvarez/null-ls.nvim'
Plug 'lukas-reineke/indent-blankline.nvim'
Plug 'nvim-lua/plenary.nvim'
Plug 'nvim-telescope/telescope.nvim', { 'tag': '0.1.1' }
Plug 'neovim/nvim-lspconfig'
Plug 'williamboman/mason.nvim'
Plug 'williamboman/mason-lspconfig.nvim'
Plug 'hrsh7th/nvim-cmp'
Plug 'hrsh7th/cmp-nvim-lsp'
Plug 'hrsh7th/cmp-buffer'
Plug 'hrsh7th/cmp-path'
Plug 'nvim-treesitter/nvim-treesitter', {'do': ':TSUpdate'}
Plug 'nvim-treesitter/nvim-treesitter-textobjects'
Plug 'lewis6991/gitsigns.nvim'
Plug 'epwalsh/obsidian.nvim'
Plug 'junegunn/vim-easy-align'
Plug 'preservim/nerdtree'

Plug 'github/copilot.vim'
if has('nvim')
  Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
else
  Plug 'Shougo/deoplete.nvim'
  Plug 'roxma/nvim-yarp'
  Plug 'roxma/vim-hug-neovim-rpc'
endif

Plug 'Shougo/neosnippet.vim'
Plug 'Shougo/neosnippet-snippets'

Plug 'lervag/vimtex'

" golang
Plug 'mfussenegger/nvim-dap'
Plug 'leoluz/nvim-dap-go'
Plug 'rcarriga/nvim-dap-ui'


call plug#end()


" try
"     execute 'source ' . stdpath('config') . '/private.vim'
" catch /E484:/
"     echom "private.vim not found"
" endtry

" Buffers
set hidden
set autoread
set noswapfile
" Remember more files
set viminfo=!,'1000,<50,s10,h
" Overwrite files to update, instead of renaming + rewriting (which messes up
" file watchers and crontab -e)
set backupcopy=yes
" Windows
set colorcolumn=80
set scrolloff=5
set sidescrolloff=3
set nowrap
set diffopt+=vertical
" Change default position of new splits
set splitbelow
set splitright
" Tabs and whitespace
set textwidth=80
set shiftwidth=4
set expandtab
set list
" Mouse
set mouse=a  " mouse support in terminals

" Clipboard
set clipboard^=unnamed  " yanks and cuts go in system clipboard
" Search
set ignorecase
set smartcase
set showmatch
set hlsearch
" Keys
set pastetoggle=<Leader>tp
nnoremap <leader><space> :noh<cr>
nnoremap <leader>w :bdelete<cr>
" h and l and ~ wrap over lines
set whichwrap=h,l,~
" Q reformats current paragraph or selected text
nnoremap Q gqap
vnoremap Q gq
" Use up and down to move by screen line
nnoremap k gk
nnoremap j gj
vnoremap k gk
vnoremap j gj
" jk for esc
inoremap jk <esc>
" keep cursor centered on n/N
nnoremap n nzz
nnoremap N Nzz
" keep cursor stationary on J
nnoremap J mzJ`z

" Use undotree and persist undo across sessions
set undofile
nnoremap <leader>u :UndotreeToggle<CR>

" Show list of possible files on tab completion, rather than first guess
set wildmode=longest,list

" Screen decoration
set number
set relativenumber
set cursorline
set statusline=
set statusline+=%{FugitiveStatusline()}%{ObsessionStatus()}
set statusline+=\ %f\ %y\ %=%c,%l/%L

" Colors
if filereadable(expand("~/.background-dark"))
    set background=dark
else
    set background=light
endif
if exists('+termguicolors')
    " :help xterm-true-color (possibly unnecessary in nvim)
    let &t_8f = "\<Esc>[38;2;%lu;%lu;%lum"
    let &t_8b = "\<Esc>[48;2;%lu;%lu;%lum"
    set termguicolors
endif
" Must happen before we set colorscheme; will rerun when colorscheme changes
augroup customize_iceberg
    autocmd!
    " Underline spelling problems
    autocmd ColorScheme iceberg highlight! link SpellBad Underlined
    autocmd ColorScheme iceberg highlight! link SpellCap SpellBad
    autocmd ColorScheme iceberg highlight! link SpellLocal SpellBad
    autocmd ColorScheme iceberg highlight! link SpellRare SpellBad
    " Tweak Error group (used for comments, etc.)
    " no bg (so it doesn't look weird under CursorLine)
    autocmd ColorScheme iceberg highlight Error ctermbg=NONE guibg=NONE
    " italic where it works, i.e. tmux-256color, xterm-256color, alacritty, etc.
    if &term !~ '^screen'
        autocmd ColorScheme iceberg highlight Error cterm=italic gui=italic
    endif
    " Prominent comments
    autocmd ColorScheme iceberg highlight! link Comment Error
    " Reverse selection
    autocmd ColorScheme iceberg highlight! Visual cterm=reverse gui=reverse
    " buftabline
    autocmd ColorScheme iceberg highlight! link BufTabLineCurrent PmenuSel
    autocmd ColorScheme iceberg highlight! link BufTabLineHidden LineNr
    autocmd ColorScheme iceberg highlight! link BufTabLineFill LineNr
    " statusline
    autocmd ColorScheme iceberg highlight! link StatusLine PmenuSel
    autocmd ColorScheme iceberg highlight! link StatusLineTerm PmenuSel
    autocmd ColorScheme iceberg highlight! link StatusLineNC LineNr
    autocmd ColorScheme iceberg highlight! link StatusLineTermNC LineNr
    " modify background if ~/.background-light file exists
    " `nested` so that ColorScheme autocmds fire after this
    autocmd FocusGained * nested if filereadable(expand("~/.background-light"))
        \ | set background=light
        \ | else
        \ | set background=dark
        \ | endif
augroup END
try
    colorscheme iceberg
catch /E185:/
    echom "iceberg colorscheme not available"
    set notermguicolors
endtry

" Use :w!! to save root files you forgot to open with sudo
ca w!! w !sudo tee "%"

" Default (overriden for specific filetypes below)
set formatoptions-=t

" Treat trailing whitespace as TODO syntax group
match Todo /\s\+$/

" Expand %% to directory of file in current buffer (also %:h<Tab>)
cnoremap <expr> %% getcmdtype() == ':' ? expand('%:h').'/' : '%%'

" Name current syntax group
map <F10> :echo "hi<" . synIDattr(synID(line("."),col("."),1),"name") . '> trans<'
    \ . synIDattr(synID(line("."),col("."),0),"name") . "> lo<"
    \ . synIDattr(synIDtrans(synID(line("."),col("."),1)),"name") . ">"<CR>

" ss to generate new split
nnoremap <silent> ss <C-w>s

" readline bindings for command mode
cnoremap <C-A> <Home>
cnoremap <C-E> <End>

" copy current buffer path to clipboard
noremap <silent> <leader>p :let @+ = expand("%")<CR>    " relative to current directory
noremap <silent> <leader>/ :let @+ = expand("%:p")<CR>  " absolute
noremap <silent> <leader>~ :let @+ = expand("%:~")<CR>  " relative to home

" Leader-y to yank to system clipboard (works over remote tmux)
noremap <silent> <Leader>y :Oscyank<cr>

" Indentation
let g:indent_blankline_show_first_indent_level = v:false
let g:indent_blankline_filetype_exclude = ['help']

" Don't indent continued vimscript lines 3 * shiftwidth(?!)
let g:vim_indent_cont = &sw

augroup vimrc
    autocmd! vimrc

    " Markdown: link selected text using URL in system clipboard
    autocmd Filetype markdown vnoremap <Leader>k <ESC>`>a](<ESC>"*pa)<ESC>`<i[<ESC>

    " Automatic rename of tmux window
    if exists('$TMUX') && !exists('$NORENAME')
        autocmd BufEnter * if empty(&buftype) | call system('tmux rename-window '.expand('%:t:S')) | endif
        autocmd VimLeave * call system('tmux set-window automatic-rename on')
    endif

    " Trigger `autoread` when files changes on disk
    " https://unix.stackexchange.com/questions/149209/refresh-changed-content-of-file-opened-in-vim/383044#383044
    " https://vi.stackexchange.com/questions/13692/prevent-focusgained-autocmd-running-in-command-line-editing-mode
    autocmd FocusGained,BufEnter,CursorHold,CursorHoldI *
        \ if mode() !~ '\v(c|r.?|!|t)' && getcmdwintype() == '' | checktime | endif

    " Notification after file change
    " https://vi.stackexchange.com/questions/13091/autocmd-event-for-autoread
    autocmd FileChangedShellPost *
        \ echohl WarningMsg | echo "File changed on disk. Buffer reloaded." | echohl None

    " When editing a file, always jump to the last known cursor position.
    " Don't do it for commit messages, when the position is invalid, or when
    " inside an event handler (happens when dropping a file on gvim).
    " https://github.com/dkarter/dotfiles/blob/99b4c5675cb859a9e50f95c40b73cb3f95414ad0/vimrc#L1128-L1134
    autocmd BufReadPost *
        \ if &ft != 'gitcommit' && line("'\"") > 0 && line("'\"") <= line("$") |
        \   exe "normal g`\"" |
        \ endif

    " formatlistpat taken from ftplugin/markdown.vim
    " spellcapcheck= disables capitalization checks
    autocmd FileType asciidoc,markdown,text,gitcommit,rst setlocal
        \ formatoptions+=tcqln formatoptions-=r formatoptions-=o
        \ formatlistpat=^\\s*\\d\\+\\.\\s\\+\\\|^\\s\\+[-*+]\\s\\+\\\|^\\[^\\ze[^\\]]\\+\\]:
        \ nojoinspaces spell spellcapcheck= shiftwidth=4

    " Don't spellcheck URLs https://vi.stackexchange.com/a/4003
    syntax match UrlNoSpell "\w\+:\/\/[^[:space:]]\+" contains=@NoSpell

    autocmd FileType css,html,javascript,typescript,typescriptreact,lua setlocal shiftwidth=2
    autocmd FileType go setlocal shiftwidth=8 tabstop=8 noexpandtab
    autocmd FileType bash,python,sh
        \ setlocal foldexpr=nvim_treesitter#foldexpr() |
        \ setlocal foldmethod=expr |
        \ setlocal nofoldenable


augroup END

" Write file without changing modification time
" https://unix.stackexchange.com/a/527154/20079
function! WriteSmall()
    let mtime = system("stat -c %.Y ".shellescape(expand('%:p')))
    write
    call system("touch --date='@".mtime."' ".shellescape(expand('%:p')))
    edit
endfunction

" Recompile spell/*.add to *.add.spl if necessary
" https://vi.stackexchange.com/a/5052
for d in glob(stdpath('config') . '/spell/*.add', 1, 1)
    if filereadable(d) && (!filereadable(d . '.spl') || getftime(d) > getftime(d . '.spl'))
        exec 'mkspell! ' . fnameescape(d)
    endif
endfor

au TermOpen * setlocal nonumber norelativenumber
tnoremap <Esc> <C-\><C-n>

" Delegated RG, see https://github.com/junegunn/fzf.vim#example-advanced-ripgrep-integration
function! RipgrepFzf(query, fullscreen)
  let command_fmt = 'rg --column --line-number --no-heading --color=always --smart-case -- %s || true'
  let initial_command = printf(command_fmt, shellescape(a:query))
  let reload_command = printf(command_fmt, '{q}')
  let spec = {'options': ['--phony', '--query', a:query, '--bind', 'change:reload:'.reload_command]}
  call fzf#vim#grep(initial_command, 1, fzf#vim#with_preview(spec), a:fullscreen)
endfunction

command! -nargs=* -bang RG call RipgrepFzf(<q-args>, <bang>0)

" FZF bindings
nmap <leader>h :History<cr>
nmap <leader>b :Buffers<cr>
nmap <leader>f :Files<cr>
nmap <leader>F :GFiles<cr>
nmap <leader>g :RG<space>
" Search for current word
nnoremap <silent> <Leader>rg :RG <C-R><C-W><CR>

" Highlight on yank
augroup YankHighlight
    autocmd!
    autocmd TextYankPost * silent! lua vim.highlight.on_yank()
augroup end

" lua require('gitsigns').setup()

lua <<EOF
require("utils")
require("lsp")
require("completion")
require("treesitter")
require("nullls")
--require("metals_config")
require("telescope_config")
require("copilot-config")
require("latex")
require("dap-debugger-config")
require("obsidian").setup({
  dir = "~/vault",
  notes_subdir = "notes",
  completion = {
    nvim_cmp = true,
  },
  daily_notes = {
    folder = "daily",
  },
  note_id_func = function(title)
    suffix = ""
    if title ~= nil then
      -- If title is given, transform it into valid file name.
      suffix = title:gsub(" ", "-"):gsub("[^A-Za-z0-9-]", ""):lower()
    else
      -- If title is nil, use 4 random letters
      for _ = 1, 4 do
        suffix = suffix .. string.char(math.random(97, 122))
      end
    end
    return tostring(os.date("%Y-%m-%d")) .. "-" .. suffix
  end
})
EOF
"
""" replace selected text in visual mode. 
vnoremap <C-r> "hy:%s/<C-r>h//gc<left><left><left>

nmap <leader>c :ObsidianSearch<space>
nmap <leader>t :ObsidianToday<cr>

" Start interactive EasyAlign in visual mode (e.g. vipga)
xmap ga <Plug>(EasyAlign)

" Start interactive EasyAlign for a motion/text object (e.g. gaip)
nmap ga <Plug>(EasyAlign)

" NeoSnippets Plugin key-mappings.
" Note: It must be "imap" and "smap".  It uses <Plug> mappings.
let g:deoplete#enable_at_startup = 1
imap <C-k>     <Plug>(neosnippet_expand_or_jump)
smap <C-k>     <Plug>(neosnippet_expand_or_jump)
xmap <C-k>     <Plug>(neosnippet_expand_target)

" SuperTab like snippets behavior.
" Note: It must be "imap" and "smap".  It uses <Plug> mappings.
"imap <expr><TAB>
" \ pumvisible() ? "\<C-n>" :
" \ neosnippet#expandable_or_jumpable() ?
" \    "\<Plug>(neosnippet_expand_or_jump)" : "\<TAB>"
smap <expr><TAB> neosnippet#expandable_or_jumpable() ?
\ "\<Plug>(neosnippet_expand_or_jump)" : "\<TAB>"

" For conceal markers.
if has('conceal')
  set conceallevel=2 concealcursor=niv
endif

" nnoremap <leader>c :lua require'utils'.CamelCase()<CR>
nnoremap <Leader>s :lua require'utils'.switch_case()<CR>
" let g:ale_fixers = {
" \   'javascript': ['prettier'],
" \   'typescript': ['prettier'],
" \   'css': ['prettier'],
" \}
" " Run prettier on save
" let g:ale_fix_on_save = 1


" copilot settings
imap <Leader>cn <Plug>(copilot-next)
imap <Leader>cp <Plug>(copilot-previous)
imap <silent><script><expr> <C-K> copilot#Accept("\<CR>")
let g:copilot_no_tab_map = v:true
