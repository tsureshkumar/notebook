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
        local match_str = table.concat({month_names[start_idx], month_names[start_idx+1], month_names[start_idx+2]}, "|")

        -- Call the orgmode agenda directly with our dynamic match
        require('orgmode').agenda:set_filters(match_str):render()
    end)
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
            X = {
                description = "Pick a Quarter...",
                action = M.open_quarterly_agenda
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
end

return M
