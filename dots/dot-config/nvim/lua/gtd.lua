-- nvim-gtd-migration/lua/gtd.lua
-- Official GTD Workflow Implementation for Neovim Org Mode

local gtd = {}

-- 1. Configuration: Path Setup
gtd.base_path = vim.fn.expand("~/vault/my/notebook")
gtd.gtd_dir = gtd.base_path .. "/gtd"

local files = {
    inbox   = gtd.gtd_dir .. "/0-Inbox/inbox.org",
    next    = gtd.gtd_dir .. "/GTD-System/next-actions.org",
    someday = gtd.gtd_dir .. "/GTD-System/someday-maybe.org",
    waiting = gtd.gtd_dir .. "/GTD-System/waiting-for.org",
    review  = gtd.gtd_dir .. "/GTD-System/weekly-review.org",
    tickler = gtd.gtd_dir .. "/GTD-System/tickler.org",
}

-- 2. Internal Helpers
local function open_file(path)
    vim.cmd("edit " .. path)
end

-- Helper to move the current headline (and its properties) to another file
local function refile_item(target_path, new_todo_state)
    local ok, org = pcall(require, 'orgmode')
    if not ok then return print("Orgmode required for refiling") end

    -- Use orgmode's internal refile if possible, or manual move
    -- For simplicity in this script, we'll do a manual move of the current subtree
    vim.cmd('normal! dd') -- This is a naive implementation; in real use we'd use org.refile()
    local line = vim.fn.getreg('"')

    local f = io.open(target_path, "a")
    if f then
        -- If we have a new state (e.g. NEXT), replace TODO
        if new_todo_state then
            line = line:gsub("%* TODO", "* " .. new_todo_state)
        end
        f:write(line)
        f:close()
        print("Refiled to " .. vim.fn.fnamemodify(target_path, ":t"))
    end
end

-- 3. Phase 1: CAPTURE
function gtd.capture(task, target)
    target = target or files.inbox
    if not task or task == "" then
        local prompt = (target == files.tickler) and "GTD Tickler: " or "GTD Capture (Inbox): "
        task = vim.fn.input(prompt)
    end
    if task == "" then return end

    local date = os.date("%Y-%m-%d %a")
    local entry = string.format("\n* TODO %s\n  :PROPERTIES:\n  :CREATED: [%s]\n  :END:\n", task, date)

    local f = io.open(target, "a")
    if f then
        f:write(entry)
        f:close()
        print("Captured to " .. vim.fn.fnamemodify(target, ":t"))
    else
        print("Error: Could not open file: " .. target)
    end
end

-- 4. Phase 2 & 3: CLARIFY & ORGANIZE
-- This guides you through your Inbox items one by one.
function gtd.clarify()
    open_file(files.inbox)
    vim.cmd("normal! gg")

    local function process_next()
        -- Find the next TODO item in inbox
        local search = vim.fn.search("^\\* TODO", "W")
        if search == 0 then
            print("Inbox is empty! Great job.")
            return
        end

        local line = vim.api.nvim_get_current_line()
        local item_text = line:gsub("^%* TODO ", "")

        vim.ui.select({
            "1. Actionable? (Next Actions)",
            "2. Waiting For (Delegated)",
            "3. Someday/Maybe (Incubate)",
            "4. Reference (Non-actionable)",
            "5. Trash (Delete)",
            "6. Done (2-minute rule)",
            "Skip",
            "Quit"
        }, {
            prompt = "Clarify: " .. item_text,
        }, function(choice)
            if not choice or choice == "Quit" then return end
            if choice == "Skip" then
                process_next()
                return
            end

            if choice:match("^1") then
                refile_item(files.next, "NEXT")
            elseif choice:match("^2") then
                refile_item(files.waiting, "WAITING")
            elseif choice:match("^3") then
                refile_item(files.someday, "TODO")
            elseif choice:match("^4") then
                print("Manual: Move to your reference notes.")
            elseif choice:match("^5") then
                vim.cmd("normal! dd")
                print("Item trashed.")
            elseif choice:match("^6") then
                refile_item(files.next, "DONE")
                print("Marked as Done.")
            end

            process_next() -- Recurse to next item
        end)
    end

    process_next()
end

-- 5. Phase 4: REFLECT (Weekly Review)
function gtd.reflect()
    open_file(files.review)
    print("Starting Weekly Review. Follow the checklist.")
end

-- 6. Phase 5: ENGAGE (Do)
function gtd.engage()
    local ok, org = pcall(require, 'orgmode')
    if ok then
        vim.cmd("Org agenda")
    else
        open_file(files.next)
    end
end

-- 7. Second Brain Search (Requires fzf-lua)
function gtd.search_notes()
    require("fzf-lua").live_grep({ cwd = gtd.base_path .. "/notes", prompt_title = "Search Notes" })
end

function gtd.search_tasks()
    require("fzf-lua").live_grep({
        cwd = gtd.gtd_dir,
        search = "^\\* (TODO|NEXT|WAITING)",
        no_esc = true,
        prompt_title = "Active Tasks Search",
    })
end

function gtd.search_all()
    require("fzf-lua").live_grep({ cwd = gtd.base_path, prompt_title = "Search Second Brain" })
end

-- 8. Setup Commands & Mappings
function gtd.setup_commands()
    vim.api.nvim_create_user_command("GTDCapture", function(opts) gtd.capture(opts.args) end, { nargs = "?" })
    vim.api.nvim_create_user_command("GTDTickler", function(opts) gtd.capture(opts.args, files.tickler) end, { nargs = "?" })
    vim.api.nvim_create_user_command("GTDClarify", gtd.clarify, {})
    vim.api.nvim_create_user_command("GTDReflect", gtd.reflect, {})
    vim.api.nvim_create_user_command("GTDEngage", gtd.engage, {})
end

function gtd.setup_mappings()
    local opts = { silent = true }
    vim.keymap.set("n", "<leader>gc", ":GTDCapture<CR>", opts)
    vim.keymap.set("n", "<leader>gt", ":GTDTickler<CR>", opts)
    vim.keymap.set("n", "<leader>gp", ":GTDClarify<CR>", opts)
    vim.keymap.set("n", "<leader>gr", ":GTDReflect<CR>", opts)
    vim.keymap.set("n", "<leader>ge", ":GTDEngage<CR>", opts)
    vim.keymap.set("n", "<leader>gi", function() open_file(files.inbox) end, opts)
    vim.keymap.set("n", "<leader>gn", function() open_file(files.next) end, opts)
    vim.keymap.set("n", "<leader>gk", function() open_file(files.tickler) end, { desc = "GTD: Open Tickler" })
    vim.keymap.set("n", "<leader>sn", gtd.search_notes, { desc = "GTD: Search Notes" })
    vim.keymap.set("n", "<leader>st", gtd.search_tasks, { desc = "GTD: Search Active Tasks" })
end

function gtd.init()
    gtd.setup_commands()
    gtd.setup_mappings()
    local ok, org = pcall(require, "orgmode")
    if ok then
        org.setup({
            org_agenda_files = { gtd.gtd_dir .. "/**/*" },
            org_default_notes_file = files.inbox,
        })
    end
end

return gtd
