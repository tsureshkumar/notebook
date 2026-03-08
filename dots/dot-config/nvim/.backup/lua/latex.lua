local function setup_vimtex()
  -- Enable filetype plugins and indentation
  vim.cmd("filetype plugin indent on")

  -- Enable syntax highlighting
  vim.cmd("syntax enable")

  -- Set the viewer options
  vim.g.vimtex_view_method = 'zathura'  -- Built-in viewer method
  vim.g.vimtex_view_general_viewer = 'okular'  -- Generic interface viewer
  vim.g.vimtex_view_general_options = '--unique file:@pdf\\#src:@line@tex'

  -- Set the compiler backend
  vim.g.vimtex_compiler_method = 'latexrun'

  -- Set the localleader key
  vim.g.maplocalleader = ','
end

setup_vimtex()
