-- Ultimate Nerdy Systems + Polyglot Neovim Configuration (Pure Lua)

-- 1. Global Options
vim.g.mapleader = "\\"
vim.g.maplocalleader = ","

local vault_root = vim.fn.expand("~/vault/my/notebook/")

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
map("n", "<leader><space>", ":noh<cr>", { desc = "Clear Search Highlights" })
map("n", "ss", "<C-w>s", { desc = "Split Horizontal" })
map("n", "sv", "<C-w>v", { desc = "Split Vertical" })
map("n", "J", "mzJ`z")
map("n", "n", "nzz")
map("n", "N", "Nzz")
map("i", "jk", "<esc>")
map({ "n", "v" }, "k", "gk")
map({ "n", "v" }, "j", "gj")

-- Systems Engineering & Tools
map("n", "<leader>d", ":!d2 --watch %<CR>", { desc = "D2: Live Browser Preview" })
map("n", "<leader>l", ":.lua<CR>", { desc = "Execute current line as Lua" })
map("n", "]q", ":cnext<cr>zz")
map("n", "[q", ":cprev<cr>zz")
map("n", "<F7>", ":make!<cr>", { desc = "Run Make" })
map("n", "<S-F7>", ":make clean all<cr>", { desc = "Make Clean All" })
map("n", "<F5>", ":!./%<<cr>", { desc = "Run compiled binary" })

-- Path & Case Utilities (Consolidated under \c)
map("n", "<leader>cp", function() vim.fn.setreg("+", vim.fn.expand("%")) end, { desc = "Copy relative path" })
map("n", "<leader>cP", function() vim.fn.setreg("+", vim.fn.expand("%:p")) end, { desc = "Copy absolute path" })
map("n", "<leader>cs", function() require("utils").switch_case() end, { desc = "Change: Switch Case" })

-- 3. Plugin Manager (lazy.nvim)
local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not (vim.uv or vim.loop).fs_stat(lazypath) then
    vim.fn.system({ "git", "clone", "--filter=blob:none", "https://github.com/folke/lazy.nvim.git", "--branch=stable",
        lazypath })
end
vim.opt.rtp:prepend(lazypath)

-- Check if dev.lua exists and safely load its content
local has_dev, dev_specs = pcall(require, "dev")
local plugins = {
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

    -- Core Navigation & UI (Snacks & Which-Key)
    {
        "folke/which-key.nvim",
        event = "VeryLazy",
        opts = {
            preset = "modern",
            spec = {
                { "<leader>f", group = "Find (FZF)", icon = " " },
                { "<leader>c", group = "Change/Copy", icon = "󰛢 " },
                { "<leader>n", group = "Notify", icon = "󰵙 " },
                { "<leader>s", group = "Scratch", icon = "󰄱 " },
                { "<leader>T", group = "Telescope", icon = "󰭎 " },
            },
        },
        keys = {
            {
                "<leader>?",
                function()
                    require("which-key").show({ global = false })
                end,
                desc = "Buffer Local Keymaps (which-key)",
            },
        },
    },
    {
        "folke/snacks.nvim",
        priority = 1000,
        lazy = false,
        ---@type snacks.Config
        opts = {
            bigfile = { enabled = true },
            dashboard = { enabled = true },
            indent = { enabled = true },
            input = { enabled = true },
            notifier = { enabled = true },
            quickfile = { enabled = true },
            scroll = { enabled = true },
            statuscolumn = { enabled = true },
            words = { enabled = true },
            scratch = { enabled = true },
            zen = { enabled = true },
            terminal = { enabled = true },
        },
        keys = {
            -- Scratch (grouped under \s)
            { "<leader>ss", function() Snacks.scratch() end,               desc = "Scratch: Show" },
            { "<leader>sS", function() Snacks.scratch.select() end,        desc = "Scratch: Select" },
            -- Notifications (grouped under \n)
            { "<leader>nh", function() Snacks.notifier.show_history() end, desc = "Notify: History" },
            { "<leader>nc", function() Snacks.notifier.hide() end,         desc = "Notify: Clear All" },
            -- UI/Tools
            { "<leader>w",  function() Snacks.bufdelete() end,             desc = "Wipe Buffer (Clean)" },
            { "<leader>cr", function() Snacks.rename.rename_file() end,    desc = "Change: Rename File" },
            { "<leader>z",  function() Snacks.zen() end,                   desc = "Zen Mode" },
            { "<leader>t",  function() Snacks.terminal() end,              desc = "Terminal (Floating)" },
        },
    },
    {
        "nvim-lualine/lualine.nvim",
        dependencies = { "nvim-tree/nvim-web-devicons" },
        config = function()
            local function org_breadcrumbs()
                if vim.bo.filetype ~= "org" then return "" end
                local ok, org_api = pcall(require, "orgmode.api")
                if not ok then return "" end
                local headline = org_api.get_current_headline()
                if not headline then return "" end
                local path = headline:get_headline_path()
                return table.concat(path, " > ")
            end
            require("lualine").setup({
                options = { theme = "catppuccin", component_separators = "|", section_separators = "" },
                sections = {
                    lualine_c = {
                        { "filename", path = 1 },
                        org_breadcrumbs
                    }
                },
            })
        end,
    },
    {
        "folke/noice.nvim",
        event = "VeryLazy",
        opts = {
            lsp = { override = { ["vim.lsp.util.convert_input_to_markdown_lines"] = true, ["vim.lsp.util.stylize_markdown"] = true, ["cmp.entry.get_documentation"] = true } },
            presets = { bottom_search = true, command_palette = true, long_message_to_split = true, inc_rename = false, lsp_doc_border = false },
        },
        dependencies = { "MunifTanjim/nui.nvim", "rcarriga/nvim-notify" },
    },
    {
        "nvim-telescope/telescope.nvim",
        dependencies = { "nvim-lua/plenary.nvim" },
        config = true,
        keys = {
            { "<leader>Tf", "<cmd>Telescope find_files<cr>", desc = "Files" },
            { "<leader>Tg", "<cmd>Telescope live_grep<cr>",  desc = "Grep" },
            { "<leader>Tb", "<cmd>Telescope buffers<cr>",    desc = "Buffers" },
        },
    },
    { "stevearc/oil.nvim",             opts = {},                                                               keys = { { "-", "<cmd>Oil<cr>" } } },
    { "mbbill/undotree",               keys = { { "<leader>U", "<cmd>UndotreeToggle<cr>", desc = "Undotree" } } },
    { "christoomey/vim-tmux-navigator" },
    { "junegunn/vim-easy-align",       keys = { { "ga", "<Plug>(EasyAlign)", mode = { "n", "x" } } } },

    { "dhruvasagar/vim-table-mode",    ft = { "markdown", "org" } },

    -- PRODUCTIVITY: Org & Markdown Suite
    {
        "nvim-orgmode/orgmode",
        event = "VeryLazy",
        ft = { "org" },
        config = function()
            local vault_root = vim.fn.expand("~/vault/my/notebook/")
            local current_month = os.date("%B")
            local current_quarter = "Q" .. math.ceil(tonumber(os.date("%m")) / 3)
            local month_names = {
                "January", "February", "March", "April", "May", "June",
                "July", "August", "September", "October", "November", "December"
            }
            local current_month_num = tonumber(os.date("%m"))
            local quarter = math.ceil(current_month_num / 3)
            local start_idx = (quarter - 1) * 3 + 1

            -- Join the three months of the current quarter
            local q_months_match = month_names[start_idx] .. "|" ..
                month_names[start_idx + 1] .. "|" ..
                month_names[start_idx + 2]


            local function skip_if_subtree_has_todo()
                local headline = require('orgmode.api').get_current_headline()
                if not headline then return end
                local has_todo = false
                for _, child in ipairs(headline:get_all_child_headlines()) do
                    local todo = child:get_todo()
                    if todo and vim.tbl_contains({ "TODO", "NEXT", "PROGRESS", "WAITING" }, todo) then
                        has_todo = true
                        break
                    end
                end
                if has_todo then return headline:get_subtree_end() end
            end

            local function skip_if_subtree_has_next()
                local headline = require('orgmode.api').get_current_headline()
                if not headline then return end
                local has_next = false
                for _, child in ipairs(headline:get_all_child_headlines()) do
                    if child:get_todo() == "NEXT" then
                        has_next = true
                        break
                    end
                end
                if has_next then return headline:get_subtree_end() end
            end

            require("orgmode").setup({
                org_agenda_files = { vault_root .. "gtd/**/*", vault_root .. "notes/**/*" },
                org_default_notes_file = vault_root .. "gtd/0-Inbox/inbox.org",
                org_todo_keywords = { "TODO(t)", "PROGRESS(p)", "NEXT(n)", "WAITING(w)", "|", "DONE(d)", "REJECTED(r)", "CANCELLED(c)" },
                org_agenda_custom_commands = {
                    P = {
                        description = "Projects",
                        types = {
                            { type = "tags", query = "PROJECT" }
                        },
                    },
                    H = {
                        description = "Home & Office",
                        types = {
                            { type = "agenda",    org_agenda_span = "day" },
                            { type = "tags_todo", query = "OFFICE" },
                            { type = "tags_todo", query = "HOME" }
                        },
                    },
                    D = {
                        description = "Daily Action List",
                        types = {
                            { type = "agenda", org_agenda_span = "day" }
                        },
                    },
                    W = {
                        description = "Weekly Plan",
                        types = {
                            { type = "agenda", org_agenda_span = "week" },
                            {
                                type = "tags_todo",
                                match = current_month,
                                org_agenda_overriding_header = "Unscheduled " .. current_month .. " Tasks"
                            },
                        },
                    },
                    M = {
                        description = "Monthly Plan",
                        types = {
                            { type = "agenda",    org_agenda_span = "month" },
                            { type = "tags_todo", match = current_month,    org_agenda_overriding_header = "Unscheduled " .. current_month .. " Tasks" }
                        },
                    },
                    Q = {
                        description = "Quarterly Plan",
                        types = {
                            { type = "agenda",    org_agenda_span = 90 },
                            { type = "tags_todo", match = q_months_match, org_agenda_overriding_header = "Unscheduled " .. current_quarter .. " Tasks" }
                        },
                    },
                    r = {
                        description = "Recently Added Projects",
                        types = {
                            { type = "tags", query = "PROJECT", org_agenda_sorting_strategy = { "priority-down", "category-up" } }
                        },
                    },
                    E = {
                        description = "Empty Projects (No active tasks in subtree)",
                        types = {
                            { type = "tags", query = "PROJECT", org_agenda_skip_function = skip_if_subtree_has_todo }
                        },
                    },
                    S = {
                        description = "Stuck Projects (Missing NEXT action in subtree)",
                        types = {
                            { type = "tags", query = "PROJECT", org_agenda_skip_function = skip_if_subtree_has_next }
                        },
                    },
                    o = {
                        description = "At the office",
                        types = {
                            { type = "tags_todo", query = "@office" }
                        },
                    },
                },
                ui = { menu = { handler = function(data) require("org-modern.menu"):new():open(data) end } },
            })
        end,
    },
    { "akinsho/org-bullets.nvim",     ft = { "org" },             config = function() require("org-bullets").setup() end },
    { "lukas-reineke/headlines.nvim", ft = { "org", "markdown" }, dependencies = "nvim-treesitter/nvim-treesitter",      config = true },
    { "danilshvalov/org-modern.nvim" },

    -- Treesitter & Context
    {
        "nvim-treesitter/nvim-treesitter",
        build = ":TSUpdate",
        config = function()
            require("nvim-treesitter.config").setup({
                ensure_installed = { "c", "cpp", "lua", "python", "javascript", "typescript", "go", "scala", "markdown", "org", "bash", "rust", "d2", "quint" },
                highlight = { enable = true, additional_vim_regex_highlighting = { "org" } },
            })
        end,
    },
    { "nvim-treesitter/nvim-treesitter-context", opts = { max_lines = 3 } },
    { "HiPhish/rainbow-delimiters.nvim" },

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
                            capabilities = require("cmp_nvim_lsp")
                                .default_capabilities()
                        })
                    end,
                },
            })

            local lspconfig, configs = require("lspconfig"), require("lspconfig.configs")
            if not configs.quint then
                configs.quint = {
                    default_config = {
                        cmd = { "quint-language-server", "--stdio" },
                        filetypes = { "quint" },
                        root_dir = function(fname)
                            return lspconfig.util.find_git_ancestor(fname) or
                                vim.loop.os_homedir()
                        end,
                        settings = {},
                    },
                }
            end
            lspconfig.quint.setup({ capabilities = require("cmp_nvim_lsp").default_capabilities() })
        end,
    },

    -- Search (FZF - Primary Engine)
    {
        "ibhagwan/fzf-lua",
        dependencies = { "nvim-tree/nvim-web-devicons" },
        config = function()
            local fzf = require("fzf-lua")
            fzf.setup({ fzf_opts = { ["--layout"] = "reverse" }, winopts = { height = 0.85, width = 0.80, preview = { layout = "vertical" } } })
            vim.keymap.set("n", "<leader>ff", fzf.files, { desc = "Files" })
            vim.keymap.set("n", "<leader>fg", fzf.live_grep, { desc = "Grep" })
            vim.keymap.set("n", "<leader>fb", fzf.buffers, { desc = "Buffers" })
            vim.keymap.set("n", "<leader>fw", fzf.grep_cword, { desc = "Word under cursor" })
        end,
    },

    -- Prose, Completion, Formatting, Debugging & session...
    {
        "mfussenegger/nvim-lint",
        config = function()
            require("lint").linters_by_ft = { markdown = { "vale" }, org = { "vale" } }
            vim.api.nvim_create_autocmd({ "BufWritePost", "BufEnter" },
                { callback = function() require("lint").try_lint() end })
        end
    },
    {
        "hrsh7th/nvim-cmp",
        dependencies = { "hrsh7th/cmp-nvim-lsp", "hrsh7th/cmp-buffer", "hrsh7th/cmp-path", "L3MON4D3/LuaSnip", "saadparwaiz1/cmp_luasnip" },
        config = function()
            local cmp = require("cmp")
            cmp.setup({
                snippet = { expand = function(args) require("luasnip").lsp_expand(args.body) end },
                mapping = cmp.mapping.preset.insert({
                    ["<CR>"] = cmp.mapping.confirm({ select = true }),
                    ["<C-Space>"] =
                        cmp.mapping.complete()
                }),
                sources = { { name = "nvim_lsp" }, { name = "orgmode" }, { name = "luasnip" }, { name = "buffer" }, { name = "path" } },
            })
        end
    },
    { "stevearc/conform.nvim",        opts = { formatters_by_ft = { lua = { "stylua" }, python = { "black" }, javascript = { "prettier" }, c = { "clang-format" } }, format_on_save = { timeout_ms = 500, lsp_fallback = true } } },
    {
        "mfussenegger/nvim-dap",
        dependencies = { "rcarriga/nvim-dap-ui", "nvim-neotest/nvim-nio", "jay-babu/mason-nvim-dap.nvim" },
        config = function()
            require("mason-nvim-dap").setup({ ensure_installed = { "codelldb" } })
            local dap, dapui = require("dap"), require("dapui")
            dapui.setup()
            dap.listeners.after.event_initialized["dapui_config"] = function() dapui.open() end
        end
    },
    {
        "chipsenkbeil/org-roam.nvim",
        tag = "0.2.0",
        dependencies = {
            {
                "nvim-orgmode/orgmode",
                tag = "0.7.0",
            },
        },
        config = function()
            require("org-roam").setup({
                directory = vim.fn.expand("~/vault/my/notebook"),
                database = {
                    path = vim.fn.expand("~/.local/share/nvim/org-roam.db"),
                },
            })
        end
    },
    { "scalameta/nvim-metals",        dependencies = { "nvim-lua/plenary.nvim" } },
    { "epwalsh/obsidian.nvim",        ft = "markdown",                                                                                                                                                                            opts = { workspaces = { { name = "vault", path = "~/vault" } } } },
    { "lervag/vimtex",                ft = "tex" },
    { "zbirenbaum/copilot.lua",       cmd = "Copilot",                                                                                                                                                                            opts = { suggestion = { enabled = true } } },
    { "lewis6991/gitsigns.nvim",      opts = {} },
    { "tpope/vim-fugitive" },
    { "iamcco/markdown-preview.nvim", ft = "markdown",                                                                                                                                                                            build = "cd app && npm install" },
    { "terrastruct/d2-vim",           ft = { "d2" },                                                                                                                                                                              config = function() vim.g.d2_ascii_preview = 1 end },
    { "rmagatti/auto-session",        lazy = false,                                                                                                                                                                               opts = { auto_restore_enabled = true } },
}

-- If dev.lua exists and returned a table, merge it into the plugins list
if has_dev and type(dev_specs) == "table" then
    for _, spec in ipairs(dev_specs) do
        table.insert(plugins, spec)
    end
end

require("lazy").setup(plugins, {
    dev = {
        path = "~/vault/projects/",
        fallback = true,
    }
})

-- Final setup
pcall(function() require("gtd").init() end)
vim.filetype.add({ extension = { d2 = "d2", qnt = "quint" } })
local ok, ab = pcall(require, "agent-bridge")
if ok then ab.setup({ host = "127.0.0.1", port = 7777, enable_shell = true }) end
vim.api.nvim_create_autocmd("TextYankPost", { callback = function() vim.highlight.on_yank() end })
vim.api.nvim_create_autocmd({ "BufRead", "BufNewFile" },
    { pattern = { "*.h", "*.c" }, callback = function() vim.bo.filetype = "c.doxygen" end })
