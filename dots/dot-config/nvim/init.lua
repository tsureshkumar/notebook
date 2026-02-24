-- Ultimate Nerdy Systems + Polyglot Neovim Configuration (Pure Lua)

-- 1. Global Options
vim.g.mapleader = "\\"
vim.g.maplocalleader = ","

local opt = vim.opt
opt.hidden = true
opt.autoread = true
opt.swapfile = false
opt.backupcopy = "yes"
opt.undofile = true
opt.undodir = vim.fn.stdpath("data") .. "/undo"

opt.number = true
opt.relativenumber = true
opt.cursorline = true
opt.scrolloff = 5
opt.sidescrolloff = 3
opt.colorcolumn = "80,120"
opt.wrap = false

opt.splitbelow = true
opt.splitright = true

opt.expandtab = true
opt.shiftwidth = 4
opt.tabstop = 4
opt.textwidth = 80
opt.list = true
opt.listchars = { tab = "» ", trail = "·", nbsp = "␣" }
opt.mouse = "a"

opt.ignorecase = true
opt.smartcase = true
opt.showmatch = true
opt.hlsearch = true

opt.clipboard = "unnamedplus"
opt.termguicolors = true
opt.signcolumn = "yes"
opt.updatetime = 250
opt.timeoutlen = 300

-- 2. Keymaps
local map = vim.keymap.set

-- Navigation & UI
map("n", "<leader><space>", ":noh<cr>")
map("n", "<leader>w", ":bdelete<cr>")
map("n", "ss", "<C-w>s")
map("n", "J", "mzJ`z")
map("n", "n", "nzz")
map("n", "N", "Nzz")
map("i", "jk", "<esc>")
map({ "n", "v" }, "k", "gk")
map({ "n", "v" }, "j", "gj")

-- Systems Engineering: Build & Quickfix
map("n", "]q", ":cnext<cr>zz")
map("n", "[q", ":cprev<cr>zz")
map("n", "<F7>", ":make!<cr>", { desc = "Run Make" })
map("n", "<S-F7>", ":make clean all<cr>", { desc = "Make Clean All" })
map("n", "<F5>", ":!./%<<cr>", { desc = "Run compiled binary" })

-- Utilities: Path & Case
map("n", "<leader>p", function() vim.fn.setreg('+', vim.fn.expand('%')) end, { desc = "Copy relative path" })
map("n", "<leader>/", function() vim.fn.setreg('+', vim.fn.expand('%:p')) end, { desc = "Copy absolute path" })

local function switch_case()
    local line, col = unpack(vim.api.nvim_win_get_cursor(0))
    local word = vim.fn.expand("<cword>")
    local line_str = vim.api.nvim_get_current_line()
    local start_col = vim.fn.matchstrpos(line_str, "\\k*\\%" .. (col + 1) .. "c\\k*")[2]

    if word:find("[a-z][A-Z]") then
        local snake = word:gsub("([a-z])([A-Z])", "%1_%2"):lower()
        vim.api.nvim_buf_set_text(0, line - 1, start_col, line - 1, start_col + #word, { snake })
    elseif word:find("_[a-z]") then
        local camel = word:gsub("(_)([a-z])", function(_, l) return l:upper() end)
        vim.api.nvim_buf_set_text(0, line - 1, start_col, line - 1, start_col + #word, { camel })
    end
end
map("n", "<leader>s", switch_case, { desc = "Switch Case" })

-- 3. Plugin Manager (lazy.nvim)
local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not (vim.uv or vim.loop).fs_stat(lazypath) then
    vim.fn.system({"git", "clone", "--filter=blob:none", "https://github.com/folke/lazy.nvim.git", "--branch=stable", lazypath})
end
vim.opt.rtp:prepend(lazypath)

require("lazy").setup({
    -- Themes
    { "rebelot/kanagawa.nvim", priority = 1000, config = function() vim.cmd.colorscheme("kanagawa") end },
    { "ellisonleao/gruvbox.nvim" },
    {
      "NLKNguyen/papercolor-theme",
      lazy = false,
      priority = 1000,
      config = function()
        vim.opt.background = "light"
        vim.cmd("colorscheme PaperColor")
      end
    }, 


    -- Core Tools
    { "nvim-telescope/telescope.nvim", dependencies = { "nvim-lua/plenary.nvim" }, config = true,
      keys = { {"<leader>f", "<cmd>Telescope find_files<cr>"}, {"<leader>g", "<cmd>Telescope live_grep<cr>"}, {"<leader>b", "<cmd>Telescope buffers<cr>"}, {"<leader>h", "<cmd>Telescope help_tags<cr>"} } },
    { "stevearc/oil.nvim", opts = {}, keys = { {"-", "<cmd>Oil<cr>"} } },
    { "mbbill/undotree", keys = { {"<leader>u", "<cmd>UndotreeToggle<cr>"} } },
    { "christoomey/vim-tmux-navigator" },
    { "junegunn/vim-easy-align", keys = { {"ga", "<Plug>(EasyAlign)", mode = {"n", "x"}} } },

    -- Treesitter
    { "nvim-treesitter/nvim-treesitter", build = ":TSUpdate", config = function()
        require("nvim-treesitter.config").setup({
            ensure_installed = { "c", "cpp", "lua", "python", "javascript", "typescript", "go", "scala", "markdown", "bash", "make", "cmake", "rust" },
            highlight = { enable = true },
        })
    end },

    -- LSP, Linting, Formatting
    { "neovim/nvim-lspconfig", dependencies = { "williamboman/mason.nvim", "williamboman/mason-lspconfig.nvim", "folke/lazydev.nvim", "hrsh7th/cmp-nvim-lsp" },
      config = function()
          require("mason").setup()
          require("mason-lspconfig").setup({
              ensure_installed = { "clangd", "lua_ls", "pyright", "ts_ls", "gopls" },
              handlers = {
                  function(server_name) require("lspconfig")[server_name].setup({ capabilities = require("cmp_nvim_lsp").default_capabilities() }) end,
                  ["clangd"] = function()
                      require("lspconfig").clangd.setup({
                          cmd = { "clangd", "--background-index", "--clang-tidy", "--header-insertion=iwyu", "--completion-style=detailed" },
                      })
                  end,
              },
          })
          vim.api.nvim_create_autocmd("LspAttach", {
              callback = function(ev)
                  local opts = { buffer = ev.buf }
                  map("n", "gd", vim.lsp.buf.definition, opts)
                  map("n", "K", vim.lsp.buf.hover, opts)
                  map("n", "gi", vim.lsp.buf.implementation, opts)
                  map("n", "gr", vim.lsp.buf.references, opts)
                  map("n", "<leader>rn", vim.lsp.buf.rename, opts)
                  map("n", "<leader>ca", vim.lsp.buf.code_action, opts)
                  if vim.bo.filetype == "c" or vim.bo.filetype == "cpp" then
                      map("n", "<A-o>", "<cmd>ClangdSwitchSourceHeader<cr>", opts)
                  end
              end,
          })
      end },

    -- DAP (Debugger)
    { "mfussenegger/nvim-dap", dependencies = { "rcarriga/nvim-dap-ui", "nvim-neotest/nvim-nio", "jay-babu/mason-nvim-dap.nvim" },
      keys = { {"<F9>", function() require('dap').toggle_breakpoint() end}, {"<F4>", function() require('dap').continue() end}, {"<leader>du", function() require('dapui').toggle() end} },
      config = function()
          require("mason-nvim-dap").setup({ ensure_installed = { "codelldb" } })
          local dap, dapui = require("dap"), require("dapui")
          dapui.setup()
          dap.listeners.after.event_initialized["dapui_config"] = function() dapui.open() end
      end },

    -- Completion
    { "hrsh7th/nvim-cmp", dependencies = { "hrsh7th/cmp-nvim-lsp", "hrsh7th/cmp-buffer", "hrsh7th/cmp-path", "L3MON4D3/LuaSnip", "saadparwaiz1/cmp_luasnip" },
      config = function()
          local cmp = require("cmp")
          cmp.setup({
              snippet = { expand = function(args) require("luasnip").lsp_expand(args.body) end },
              mapping = cmp.mapping.preset.insert({ ["<CR>"] = cmp.mapping.confirm({ select = true }), ["<C-Space>"] = cmp.mapping.complete() }),
              sources = { { name = "nvim_lsp" }, { name = "luasnip" }, { name = "buffer" }, { name = "path" } },
          })
      end },

    -- Formatting
    { "stevearc/conform.nvim", opts = {
        formatters_by_ft = { lua = {"stylua"}, python = {"isort", "black"}, javascript = {"prettier"}, typescript = {"prettier"}, c = {"clang-format"}, cpp = {"clang-format"} },
        format_on_save = { timeout_ms = 500, lsp_fallback = true },
    } },

    -- Specialized
    { "scalameta/nvim-metals", dependencies = { "nvim-lua/plenary.nvim" }, config = function()
        local config = require("metals").bare_config()
        vim.api.nvim_create_autocmd("FileType", { pattern = { "scala", "sbt" }, callback = function() require("metals").initialize_or_attach(config) end })
    end },
    { "epwalsh/obsidian.nvim", ft = "markdown", opts = { workspaces = { { name = "vault", path = "~/vault" } } } },
    { "lervag/vimtex", ft = "tex" },
    { "zbirenbaum/copilot.lua", cmd = "Copilot", opts = { suggestion = { enabled = true } } },
    { "lewis6991/gitsigns.nvim", opts = {} },
    { "tpope/vim-fugitive" },
})

-- Agent Bridge
local ok, ab = pcall(require, "agent-bridge")
if ok then ab.setup({ host = "127.0.0.1", port = 7777, enable_shell = true }) end

-- Global Autocmds
vim.api.nvim_create_autocmd("TextYankPost", { callback = function() vim.highlight.on_yank() end })
