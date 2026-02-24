-- Simple & Minimal Neovim Configuration (Pure Lua)

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
opt.colorcolumn = "80"
opt.wrap = false

opt.splitbelow = true
opt.splitright = true

opt.expandtab = true
opt.shiftwidth = 4
opt.tabstop = 4
opt.textwidth = 80
opt.list = true
opt.mouse = "a"

opt.ignorecase = true
opt.smartcase = true
opt.showmatch = true
opt.hlsearch = true

opt.clipboard = "unnamedplus" -- OSC 52 support is automatic in Nvim 0.11+
opt.termguicolors = true
opt.signcolumn = "yes"
opt.updatetime = 250
opt.timeoutlen = 300

-- 2. Keymaps
local map = vim.keymap.set

-- Clear search highlights
map("n", "<leader><space>", ":noh<cr>")
-- Close buffer
map("n", "<leader>w", ":bdelete<cr>")

-- Screen line movement
map({ "n", "v" }, "k", "gk")
map({ "n", "v" }, "j", "gj")

-- jk for Esc
map("i", "jk", "<esc>")

-- Centered search result
map("n", "n", "nzz")
map("n", "N", "Nzz")

-- Stationary Join
map("n", "J", "mzJ`z")

-- Quick Split
map("n", "ss", "<C-w>s")

-- Path copying
map("n", "<leader>p", function() vim.fn.setreg('+', vim.fn.expand('%')) end, { desc = "Copy relative path" })
map("n", "<leader>/", function() vim.fn.setreg('+', vim.fn.expand('%:p')) end, { desc = "Copy absolute path" })

-- Case Switching Utility
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
map("n", "<leader>s", switch_case, { desc = "Switch between camelCase and snake_case" })

-- Print to PDF Utility
local function print_to_pdf()
    local file = vim.fn.expand("%:r")
    vim.cmd("syntax off")
    vim.cmd("highlight clear")
    vim.cmd("highlight Normal ctermfg=black ctermbg=white")
    vim.cmd("hardcopy > " .. file .. ".ps")
    if vim.fn.executable("ps2pdf") == 1 then
        vim.fn.system("ps2pdf -dPDFSETTINGS=/ebook " .. file .. ".ps " .. file .. ".pdf")
        vim.fn.system("rm " .. file .. ".ps")
        print("Printed to reduced PDF: " .. file .. ".pdf")
    end
    vim.cmd("syntax on")
end
map("n", "<leader>P", print_to_pdf, { desc = "Print buffer to PDF" })

-- 3. Plugin Manager (lazy.nvim)
local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not (vim.uv or vim.loop).fs_stat(lazypath) then
    vim.fn.system({
        "git", "clone", "--filter=blob:none",
        "https://github.com/folke/lazy.nvim.git", "--branch=stable", lazypath,
    })
end
vim.opt.rtp:prepend(lazypath)

require("lazy").setup({
    -- Nerdy Colorscheme
    {
        "rebelot/kanagawa.nvim",
        priority = 1000,
        config = function()
            vim.cmd.colorscheme("kanagawa")
        end,
    },

    -- Fuzzy Finder
    {
        "nvim-telescope/telescope.nvim",
        dependencies = { "nvim-lua/plenary.nvim" },
        keys = {
            { "<leader>f", "<cmd>Telescope find_files<cr>" },
            { "<leader>g", "<cmd>Telescope live_grep<cr>" },
            { "<leader>b", "<cmd>Telescope buffers<cr>" },
            { "<leader>h", "<cmd>Telescope help_tags<cr>" },
        },
        config = true,
    },

    -- File Explorer (Oil)
    {
        "stevearc/oil.nvim",
        opts = {},
        keys = { { "-", "<cmd>Oil<cr>", desc = "Open parent directory" } },
    },

    -- Treesitter
    -- {
    --   "nvim-treesitter/nvim-treesitter",
    --   build = ":TSUpdate",
    --   config = function()
    --     local configs = require("nvim-treesitter.configs")
    --     configs.setup({
    --       ensure_installed = { "lua", "python", "javascript", "typescript", "go", "scala", "markdown", "bash" },
    --       highlight = { enable = true },
    --     })
    --   end,
    -- },

    -- LSP Support
    {
        "neovim/nvim-lspconfig",
        dependencies = {
            "williamboman/mason.nvim",
            "williamboman/mason-lspconfig.nvim",
            "folke/lazydev.nvim",
            "hrsh7th/cmp-nvim-lsp",
        },
        config = function()
            require("mason").setup()
            require("mason-lspconfig").setup({
                ensure_installed = { "lua_ls" },
                handlers = {
                    function(server_name)
                        require("lspconfig")[server_name].setup({
                            capabilities = require("cmp_nvim_lsp").default_capabilities(),
                        })
                    end,
                },
            })

            -- LSP Keymaps
            vim.api.nvim_create_autocmd("LspAttach", {
                callback = function(ev)
                    local opts = { buffer = ev.buf }
                    map("n", "gD", vim.lsp.buf.declaration, opts)
                    map("n", "gd", vim.lsp.buf.definition, opts)
                    map("n", "K", vim.lsp.buf.hover, opts)
                    map("n", "gi", vim.lsp.buf.implementation, opts)
                    map("n", "gr", vim.lsp.buf.references, opts)
                    map("n", "<leader>rn", vim.lsp.buf.rename, opts)
                    map("n", "<leader>ca", vim.lsp.buf.code_action, opts)
                end,
            })
        end,
    },

    -- Completion
    {
        "hrsh7th/nvim-cmp",
        dependencies = {
            "hrsh7th/cmp-nvim-lsp",
            "hrsh7th/cmp-buffer",
            "hrsh7th/cmp-path",
            "L3MON4D3/LuaSnip",
            "saadparwaiz1/cmp_luasnip",
        },
        config = function()
            local cmp = require("cmp")
            local luasnip = require("luasnip")
            cmp.setup({
                snippet = { expand = function(args) luasnip.lsp_expand(args.body) end },
                mapping = cmp.mapping.preset.insert({
                    ["<C-d>"] = cmp.mapping.scroll_docs(-4),
                    ["<C-f>"] = cmp.mapping.scroll_docs(4),
                    ["<C-Space>"] = cmp.mapping.complete(),
                    ["<CR>"] = cmp.mapping.confirm({ select = true }),
                    ["<Tab>"] = cmp.mapping(function(fallback)
                        if cmp.visible() then
                            cmp.select_next_item()
                        elseif luasnip.expand_or_jumpable() then
                            luasnip.expand_or_jump()
                        else
                            fallback()
                        end
                    end, { "i", "s" }),
                }),
                sources = {
                    { name = "nvim_lsp" },
                    { name = "luasnip" },
                    { name = "buffer" },
                    { name = "path" },
                },
            })
        end,
    },

    -- Formatting
    {
        "stevearc/conform.nvim",
        opts = {
            formatters_by_ft = {
                lua = { "stylua" },
                python = { "isort", "black" },
                javascript = { "prettier" },
                typescript = { "prettier" },
            },
            format_on_save = { timeout_ms = 500, lsp_fallback = true },
        },
    },

    -- Git
    { "lewis6991/gitsigns.nvim", opts = {} },
    { "tpope/vim-fugitive" },

    -- Scala (Metals)
    {
        "scalameta/nvim-metals",
        dependencies = { "nvim-lua/plenary.nvim" },
        config = function()
            local metals_config = require("metals").bare_config()
            metals_config.on_attach = function(client, bufnr)
                require("metals").setup_dap()
            end
            local nvim_metals_group = vim.api.nvim_create_augroup("nvim-metals", { clear = true })
            vim.api.nvim_create_autocmd("FileType", {
                pattern = { "scala", "sbt", "java" },
                callback = function() require("metals").initialize_or_attach(metals_config) end,
                group = nvim_metals_group,
            })
        end,
    },

    -- Obsidian
    {
        "epwalsh/obsidian.nvim",
        version = "*",
        lazy = true,
        ft = "markdown",
        dependencies = { "nvim-lua/plenary.nvim" },
        opts = {
            workspaces = { { name = "vault", path = "~/vault" } },
            notes_subdir = "notes",
            daily_notes = { folder = "daily" },
            completion = { nvim_cmp = true },
        },
        keys = {
            { "<leader>ot", "<cmd>ObsidianToday<cr>" },
            { "<leader>os", "<cmd>ObsidianSearch<cr>" },
        },
    },

    -- Utilities
    { "mbbill/undotree",               keys = { { "<leader>u", "<cmd>UndotreeToggle<cr>" } } },
    { "christoomey/vim-tmux-navigator" },
    { "junegunn/vim-easy-align",       keys = { { "ga", "<Plug>(EasyAlign)", mode = { "n", "x" } } } },
    { "lervag/vimtex",                 ft = "tex" },
    { "zbirenbaum/copilot.lua",        cmd = "Copilot",                                              event = "InsertEnter", opts = { suggestion = { enabled = true } } },
})

-- Agent Bridge setup
local ok, ab = pcall(require, "agent-bridge")
if ok then
    ab.setup({ host = "127.0.0.1", port = 7777, enable_shell = true })
end

-- Highlight on yank (built-in)
vim.api.nvim_create_autocmd("TextYankPost", {
    callback = function() vim.highlight.on_yank() end,
})
