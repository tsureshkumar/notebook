-- Java filetype plugin: on-demand initialization only (zero default overhead)

local map = vim.keymap.set
local bufopts = { buffer = true, silent = true }

-- On-demand Java initiator
local function init_java()
    -- Ensure nvim-jdtls is loaded
    local ok, jdtls = pcall(require, "jdtls")
    if not ok then
        -- Attempt to load via lazy if not yet loaded
        local lazy_ok, lazy = pcall(require, "lazy")
        if lazy_ok then
            lazy.load({ plugins = { "nvim-jdtls" } })
            ok, jdtls = pcall(require, "jdtls")
        end
    end

    if not ok then
        vim.notify("nvim-jdtls could not be loaded. Please install or enable it.", vim.log.levels.ERROR)
        return
    end

    -- Check if jdtls executable is available
    if vim.fn.executable("jdtls") == 0 then
        vim.notify("jdtls language server not found in PATH or Mason. Run :MasonInstall jdtls to install.", vim.log.levels.WARN)
    end

    -- Boilerplate Code Generation & Refactoring Mappings
    map("n", "<leader>jgs", function() jdtls.generate_accessor_methods() end,
        vim.tbl_extend("force", bufopts, { desc = "Java: Generate Getters/Setters" }))

    map("n", "<leader>jts", function() jdtls.generate_to_string() end,
        vim.tbl_extend("force", bufopts, { desc = "Java: Generate toString()" }))

    map("n", "<leader>jeq", function() jdtls.generate_eq_and_hash_code() end,
        vim.tbl_extend("force", bufopts, { desc = "Java: Generate equals & hashCode" }))

    map("n", "<leader>jc", function() jdtls.generate_constructor() end,
        vim.tbl_extend("force", bufopts, { desc = "Java: Generate Constructor" }))

    map("n", "<leader>joi", function() jdtls.organize_imports() end,
        vim.tbl_extend("force", bufopts, { desc = "Java: Organize Imports" }))

    map("n", "<leader>jev", function() jdtls.extract_variable() end,
        vim.tbl_extend("force", bufopts, { desc = "Java: Extract Variable" }))
    map("v", "<leader>jev", function() jdtls.extract_variable(true) end,
        vim.tbl_extend("force", bufopts, { desc = "Java: Extract Variable" }))

    map("n", "<leader>jec", function() jdtls.extract_constant() end,
        vim.tbl_extend("force", bufopts, { desc = "Java: Extract Constant" }))
    map("v", "<leader>jec", function() jdtls.extract_constant(true) end,
        vim.tbl_extend("force", bufopts, { desc = "Java: Extract Constant" }))

    map("v", "<leader>jem", function() jdtls.extract_method(true) end,
        vim.tbl_extend("force", bufopts, { desc = "Java: Extract Method" }))

    -- Start JDTLS
    local home = vim.env.HOME
    local project_name = vim.fn.fnamemodify(vim.fn.getcwd(), ":p:h:t")
    local workspace_dir = home .. "/.local/share/nvim/site/java-workspace-roots/" .. project_name

    local capabilities = {}
    local cmp_ok, cmp_lsp = pcall(require, "cmp_nvim_lsp")
    if cmp_ok then
        capabilities = cmp_lsp.default_capabilities()
    end

    local config = {
        cmd = { "jdtls", "-data", workspace_dir },
        root_dir = require("jdtls.setup").find_root({ ".classpath", ".project", "mvnw", "gradlew", "pom.xml", "build.gradle", ".git" })
            or vim.fn.expand("%:p:h"),
        capabilities = capabilities,
    }

    jdtls.start_or_attach(config)
    vim.notify("Java initiated: jdtls attached and boilerplate keymaps active (<leader>j*).", vim.log.levels.INFO)
end

-- Expose command and keymap ONLY on-demand
vim.api.nvim_buf_create_user_command(0, "JavaInit", init_java, { desc = "Initialize Java LSP and boilerplate tools on demand" })
map("n", "<leader>ji", init_java, vim.tbl_extend("force", bufopts, { desc = "Java: Initialize environment on demand" }))
