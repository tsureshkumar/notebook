local M = {}

function M.open_quarterly_agenda()
    vim.ui.input({ prompt = 'Enter Quarter (1-4): ' }, function(input)
        local q = tonumber(input)
        if not q or q < 1 or q > 4 then
            return print("Invalid quarter. Please enter 1, 2, 3, or 4.")
        end

        local month_names = {
            "January", "February", "March", "April", "May", "June",
            "July", "August", "September", "October", "November", "December"
        }

        local start_idx = (q - 1) * 3 + 1
        local match_str = table.concat(
            { month_names[start_idx], month_names[start_idx + 1], month_names[start_idx + 2] },
            "|")

        local keys = vim.api.nvim_replace_termcodes(
            string.format(":Org agenda M<CR>%s<CR>", match_str),
            true, false, true
        )
        vim.api.nvim_feedkeys(keys, 'm', false)
    end)
end

function M.export_buffer(format)
    local file = vim.fn.expand("%:p")
    local output = vim.fn.expand("%:p:r") .. "." .. format
    local extra_args = ""
    if format == "pdf" then
        extra_args = "-V geometry:\"margin=0.5in,a4paper\" -V fontsize=12pt"
    end
    local cmd = string.format("pandoc %s %s -o %s", vim.fn.shellescape(file), extra_args, vim.fn.shellescape(output))

    print("Exporting to " .. format .. "...")
    vim.fn.jobstart(cmd, {
        on_exit = function(_, code)
            if code == 0 then
                print("Export successful: " .. output)
            else
                print("Export failed for " .. format)
            end
        end
    })
end

function M.setup()
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
        org_agenda_sorting_strategy = {
            tags = { 'tag-up', 'priority-down', 'category-keep' },
        },
        org_agenda_files = { vault_root .. "gtd/**/*", vault_root .. "notes/**/*" },
        org_default_notes_file = vault_root .. "gtd/0-Inbox/inbox.org",
        org_todo_keywords = { "TODO(t)", "PROGRESS(p)", "NEXT(n)", "WAITING(w)", "|", "DONE(d)", "REJECTED(r)", "CANCELLED(c)" },
        mappings = {
            org = {
                org_refile = '<leader>or',
            },
            agenda = {
                org_agenda_switch_to = 'E',
                org_agenda_goto = '<CR>',
                org_insert_todo_heading = 'C-CR',
                org_agenda_bulk_mark = 'x',
                org_agenda_bulk_unmark = 'X',
                org_agenda_bulk_action = 'B',
            }
        },
        org_agenda_custom_commands = {
            i = {
                description = "Inbox (Bulk Refile)",
                types = {
                    {
                        type = "tags_todo",
                        match = "",
                        org_agenda_files = { vault_root .. "gtd/0-Inbox/inbox.org" },
                        org_agenda_overriding_header = "Inbox Tasks - Mark with 'x' and use 'B r' to refile"
                    }
                }
            },
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
            K = {
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
            X = {
                description = "Pick a Quarter...",
                action = M.open_quarterly_agenda,
                types = {}
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

    vim.api.nvim_create_user_command("OrgQuarterlyAgenda", M.open_quarterly_agenda, {})

    -- Export Commands
    vim.api.nvim_create_user_command("OrgExportHTML", function() M.export_buffer("html") end, {})
    vim.api.nvim_create_user_command("OrgExportMarkdown", function() M.export_buffer("md") end, {})
    vim.api.nvim_create_user_command("OrgExportPDF", function() M.export_buffer("pdf") end, {})

    vim.api.nvim_create_autocmd("FileType", {
        pattern = "org",
        callback = function()
            -- Set conceallevel for Obsidian and better org-mode look
            vim.opt_local.conceallevel = 2
            vim.opt_local.concealcursor = 'nc'

            -- Shift+Enter: Insert TODO heading below
            vim.keymap.set({ "n", "i" }, "<S-CR>", function()
                require('orgmode').action('org_mappings.insert_todo_heading')
            end, { buffer = true, desc = "Org: Insert TODO Heading" })

            -- Ctrl+Shift+Enter: Insert TODO heading below and schedule today
            vim.keymap.set({ "n", "i" }, "<C-S-CR>", function()
                require('orgmode').action('org_mappings.insert_todo_heading')
                vim.defer_fn(function()
                    local api = require('orgmode').api
                    local headline = api.get_current_headline()
                    if headline then
                        headline:set_scheduled(os.date('%Y-%m-%d'))
                        vim.cmd("startinsert!")
                    end
                end, 150)
            end, { buffer = true, desc = "Org: Insert TODO Scheduled Today" })

            vim.keymap.set("n", "<leader>oeh", ":OrgExportHTML<CR>", { desc = "Org: Export to HTML", buffer = true })
            vim.keymap.set("n", "<leader>oem", ":OrgExportMarkdown<CR>",
                { desc = "Org: Export to Markdown", buffer = true })
            vim.keymap.set("n", "<leader>oep", ":OrgExportPDF<CR>", { desc = "Org: Export to PDF", buffer = true })
        end
    })

    vim.api.nvim_create_autocmd("FileType", {
        pattern = "orgagenda",
        callback = function()
            vim.keymap.set("n", "x", function()
                local ok = pcall(function() require('orgmode').action('agenda.org_agenda_bulk_mark') end)
                if not ok then
                    require('orgmode').action('org_agenda_bulk_mark')
                end
            end, { buffer = true, desc = "Org Agenda: Bulk Mark" })

            vim.keymap.set("n", "B", function()
                local ok = pcall(function() require('orgmode').action('agenda.org_agenda_bulk_action') end)
                if not ok then
                    require('orgmode').action('org_agenda_bulk_action')
                end
            end, { buffer = true, desc = "Org Agenda: Bulk Action" })
        end
    })
end

return M
