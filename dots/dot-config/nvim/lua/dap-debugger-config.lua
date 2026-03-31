require("dap").set_log_level("TRACE")

require("dap-go").setup({
    dap_configurations = {
        {
            type = "go",
            name = "Attach remote",
            mode = "remote",
            request = "attach",
            showLog = true,
            connect = {
                host = "127.0.0.1",
                port = 2345,
            },
            -- Adjust substitutePath entries to match your project layout
            substitutePath = {},
        },
    },
    delve = {
        path = "dlv",
        initialize_timeout_sec = 20,
        port = "2345",
        build_flags = "",
    },
})

vim.keymap.set("n", "<leader>ds", ":DapStepOver<CR>")
vim.keymap.set("n", "<leader>dc", ":DapContinue<CR>")
vim.keymap.set("n", "<leader>db", ":DapToggleBreakpoint<CR>")
vim.keymap.set("n", "<leader>du", function() require("dapui").toggle() end)
