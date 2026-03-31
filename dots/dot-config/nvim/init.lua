-- Ultimate Nerdy Systems + Polyglot Neovim Configuration (Pure Lua)

-- 1. Global Options
vim.g.mapleader = "\\"
vim.g.maplocalleader = ","

local opt = vim.opt
opt.hidden = true
opt.autoread = true
opt.swapfile = false
opt.undofile = true
opt.undodir = vim.fn.stdpath("data") .. "/undo"
opt.autoindent = true
opt.smartindent = true

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
opt.mouse = "r"

opt.ignorecase = true
opt.smartcase = true
opt.showmatch = true
opt.hlsearch = true

opt.clipboard = "unnamed"
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
map("n", "<leader>l", ":.lua<CR>", { desc = "Execute current line as Lua" })

-- Path & Case Utilities
map("n", "<leader>p", function() vim.fn.setreg("+", vim.fn.expand("%")) end, { desc = "Copy relative path" })
map("n", "<leader>/", function() vim.fn.setreg("+", vim.fn.expand("%:p")) end, { desc = "Copy absolute path" })
map("n", "<leader>s", require("utils").switch_case, { desc = "Switch Case" })

-- 3. Plugin Manager (lazy.nvim)
local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not (vim.uv or vim.loop).fs_stat(lazypath) then
    vim.fn.system({ "git", "clone", "--filter=blob:none", "https://github.com/folke/lazy.nvim.git", "--branch=stable", lazypath })
end
vim.opt.rtp:prepend(lazypath)

require("lazy").setup({
    -- Appearance & Themes
    {
        "catppuccin/nvim",
        name = "catppuccin",
        priority = 1001,
        config = function()
            vim.cmd.colorscheme("catppuccin-mocha")
        end,
    },
    { "rebelot/kanagawa.nvim" },
    { "ellisonleao/gruvbox.nvim" },
    { "sainnhe/everforest" },
    { "rose-pine/neovim" },
    { "NLKNguyen/papercolor-theme" },
    {
        "zenbones-theme/zenbones.nvim",
        dependencies = { "rktjmp/lush.nvim" },
        config = function()
            vim.g.zenbones_italic_comments = true
            vim.g.zenbones_solid_line_nr = true
            vim.g.zenbones_darken_comments = 45
        end,
    },

    -- Core Navigation & UI
    {
        "nvim-telescope/telescope.nvim",
        dependencies = { "nvim-lua/plenary.nvim" },
        config = true,
        keys = {
            { "<leader>f", "<cmd>Telescope find_files<cr>" },
            { "<leader>g", "<cmd>Telescope live_grep<cr>" },
            { "<leader>b", "<cmd>Telescope buffers<cr>" },
        },
    },
    { "stevearc/oil.nvim",             opts = {},  keys = { { "-", "<cmd>Oil<cr>" } } },
    { "mbbill/undotree",               keys = { { "<leader>u", "<cmd>UndotreeToggle<cr>" } } },
    { "christoomey/vim-tmux-navigator" },
    { "junegunn/vim-easy-align",       keys = { { "ga", "<Plug>(EasyAlign)", mode = { "n", "x" } } } },

    -- PRODUCTIVITY: Org Mode Suite
    {
        "nvim-orgmode/orgmode",
        event = "VeryLazy",
        ft = { "org" },
        config = function()
            require("orgmode").setup({
                org_agenda_files = { "~/org/**/*", "~/my/notebook-private/SecondBrain/my/notebook/gtd/**/*" },
                org_default_notes_file = "~/org/refile.org",
                org_todo_keywords = { "TODO(t)", "PROGRESS(p)", "NEXT(n)", "WAITING(w)", "|", "DONE(d)", "REJECTED(r)" },
                ui = {
                    menu = {
                        handler = function(data)
                            require("org-modern.menu"):new():open(data)
                        end,
                    },
                },
            })
        end,
    },
    { "akinsho/org-bullets.nvim",     ft = { "org" },             config = function() require("org-bullets").setup() end },
    { "lukas-reineke/headlines.nvim", ft = { "org", "markdown" }, dependencies = "nvim-treesitter/nvim-treesitter", config = true },
    { "danilshvalov/org-modern.nvim" },

    -- Treesitter
    {
        "nvim-treesitter/nvim-treesitter",
        build = ":TSUpdate",
        config = function()
            require("nvim-treesitter.config").setup({
                ensure_installed = { "c", "cpp", "lua", "python", "javascript", "typescript", "go", "scala", "markdown", "org", "bash", "rust" },
                highlight = { enable = true, additional_vim_regex_highlighting = { "org" } },
            })
        end,
    },

    -- LSP, Formatting & Linting
    {
        "neovim/nvim-lspconfig",
        dependencies = { "williamboman/mason.nvim", "williamboman/mason-lspconfig.nvim", "hrsh7th/cmp-nvim-lsp" },
        config = function()
            require("mason").setup()
            require("mason-lspconfig").setup({
                ensure_installed = { "clangd", "lua_ls", "pyright", "ts_ls", "gopls" },
                handlers = {
                    function(server_name)
                        require("lspconfig")[server_name].setup({
                            capabilities = require("cmp_nvim_lsp").default_capabilities(),
                        })
                    end,
                },
            })
        end,
    },

    -- Prose Linting
    {
        "mfussenegger/nvim-lint",
        config = function()
            require("lint").linters_by_ft = {
                markdown = { "vale" },
                org = { "vale" },
            }
            vim.api.nvim_create_autocmd({ "BufWritePost", "BufEnter" }, {
                callback = function() require("lint").try_lint() end,
            })
        end,
    },

    -- Completion
    {
        "hrsh7th/nvim-cmp",
        dependencies = { "hrsh7th/cmp-nvim-lsp", "hrsh7th/cmp-buffer", "hrsh7th/cmp-path", "L3MON4D3/LuaSnip", "saadparwaiz1/cmp_luasnip" },
        config = function()
            local cmp = require("cmp")
            cmp.setup({
                snippet = { expand = function(args) require("luasnip").lsp_expand(args.body) end },
                mapping = cmp.mapping.preset.insert({
                    ["<CR>"] = cmp.mapping.confirm({ select = true }),
                    ["<C-Space>"] = cmp.mapping.complete(),
                }),
                sources = { { name = "nvim_lsp" }, { name = "orgmode" }, { name = "luasnip" }, { name = "buffer" }, { name = "path" } },
            })
        end,
    },

    -- Formatting
    {
        "stevearc/conform.nvim",
        opts = {
            formatters_by_ft = { lua = { "stylua" }, python = { "black" }, javascript = { "prettier" }, c = { "clang-format" } },
            format_on_save = { timeout_ms = 500, lsp_fallback = true },
        },
    },

    -- Debugging
    {
        "mfussenegger/nvim-dap",
        dependencies = { "rcarriga/nvim-dap-ui", "nvim-neotest/nvim-nio", "jay-babu/mason-nvim-dap.nvim" },
        config = function()
            require("mason-nvim-dap").setup({ ensure_installed = { "codelldb" } })
            local dap, dapui = require("dap"), require("dapui")
            dapui.setup()
            dap.listeners.after.event_initialized["dapui_config"] = function() dapui.open() end
        end,
    },

    -- Search
    {
        "ibhagwan/fzf-lua",
        dependencies = { "nvim-tree/nvim-web-devicons" },
        config = function()
            local fzf = require("fzf-lua")
            fzf.setup({
                fzf_opts = { ["--layout"] = "reverse" },
                winopts = {
                    height = 0.85,
                    width = 0.80,
                    preview = { layout = "vertical" },
                },
            })
            vim.keymap.set("n", "<leader>sf", fzf.files, { desc = "FZF: Find Files" })
            vim.keymap.set("n", "<leader>sg", fzf.live_grep, { desc = "FZF: Live Grep" })
            vim.keymap.set("n", "<leader>sw", fzf.grep_cword, { desc = "FZF: Search Word Under Cursor" })
        end,
    },

    -- Specialized & External Tools
    { "scalameta/nvim-metals",        dependencies = { "nvim-lua/plenary.nvim" } },
    { "epwalsh/obsidian.nvim",        ft = "markdown", opts = { workspaces = { { name = "vault", path = "~/vault" } } } },
    { "lervag/vimtex",                ft = "tex" },
    { "zbirenbaum/copilot.lua",       cmd = "Copilot", opts = { suggestion = { enabled = true } } },
    { "lewis6991/gitsigns.nvim",      opts = {} },
    { "tpope/vim-fugitive" },
    { "iamcco/markdown-preview.nvim", ft = "markdown", build = "cd app && npm install" },
    { "rmagatti/auto-session",        lazy = false,    opts = { auto_restore_enabled = true } },
})

-- GTD Initialization
pcall(function() require("gtd").init() end)

-- Agent Bridge
local ok, ab = pcall(require, "agent-bridge")
if ok then ab.setup({ host = "127.0.0.1", port = 7777, enable_shell = true }) end

-- Highlight on yank
vim.api.nvim_create_autocmd("TextYankPost", {
    callback = function() vim.highlight.on_yank() end,
})

-- Autocommands
vim.api.nvim_create_autocmd("InsertEnter", { callback = function() opt.relativenumber = false end })
vim.api.nvim_create_autocmd("InsertLeave", { callback = function() opt.relativenumber = true end })
vim.api.nvim_create_autocmd({ "BufRead", "BufNewFile" }, {
    pattern = { "*.h", "*.c" },
    callback = function() vim.bo.filetype = "c.doxygen" end,
})
