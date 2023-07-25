---  write a lua array
--- write a dap adaptor for a debugger
require('dap').set_log_level('TRACE')
require('dap-go').setup()

require('dap-go').setup {
  dap_configurations = {
    {
      type = "go",
      name = "Attach remote",
      mode = "remote",
      program = "dlv",
      request = "attach",
      showLog = true,
      connect = {
        host = "127.0.0.1",
        port = 2345,
      },
      substitutePath = {
                {
                    from = "/home/user/go-code/src",
                    to = "src"
                },
                {
                    from= "/home/user/go-code/bazel-go-code/external/",
                    to= "external/"
                },
                {
                    from =  "/home/user/go-code/bazel-out/",
                    to =  "bazel-out/"
                },
                {
                    from =  "/home/user/go-code/bazel-go-code/external/go_sdk",
                    to =  "GOROOT/"
                }
            },
    },
  },
  -- delve configurations
  delve = {
    -- the path to the executable dlv which will be used for debugging.
    -- by default, this is the "dlv" executable on your PATH.
    path = "dlv",
    -- time to wait for delve to initialize the debug session.
    -- default to 20 seconds
    initialize_timeout_sec = 20,
    -- a string that defines the port to start delve debugger.
    -- default to string "${port}" which instructs nvim-dap
    -- to start the process in a random available port
    port = "2345",
    -- additional args to pass to dlv
    type = "executable",
    args = {"connect"},
    -- the build flags that are passed to delve.
    -- defaults to empty string, but can be used to provide flags
    -- such as "-tags=unit" to make sure the test suite is
    -- compiled during debugging, for example.
    -- passing build flags using args is ineffective, as those are
    -- ignored by delve in dap mode.
    build_flags = "",
  },
  --- adapters.delve = {
  ---   type = "server",
  ---   host = "127.0.0.1",
  ---   port = 2345,
  --- },
}

vim.keymap.set("n", '<leader>ds', ":DapStepOver<CR>")
vim.keymap.set("n", '<leader>dc', ":DapContinue<CR>")
vim.keymap.set("n", '<leader>db', ":DapToggleBreakpoint<CR>")
vim.keymap.set("n", '<leader>du', ":lua require('dapui').toggle()<CR>")
