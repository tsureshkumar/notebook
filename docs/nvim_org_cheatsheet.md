# Neovim Org Mode & GTD Cheatsheet

## 🌙 GTD Workflow (Custom `gtd.lua`)
- **<leader>gc**: Capture to Inbox
- **<leader>gt**: Capture to Tickler
- **<leader>gp**: Clarify (Process Inbox)
- **<leader>gr**: Reflect (Weekly Review)
- **<leader>ge**: Engage (Open Agenda/Next Actions)
- **<leader>gi**: Open Inbox file
- **<leader>gn**: Open Next Actions file
- **<leader>gk**: Open Tickler file
- **<leader>sn**: Search Notes
- **<leader>st**: Search Active Tasks

## 🗓️ Agenda View (`nvim-orgmode`)
- **<leader>oa**: Open Agenda Menu
- **f**: Filter agenda by tag
- **r**: Rebuild/Refresh agenda
- **s**: Save all Org buffers
- **t**: Cycle TODO state
- **<RET>**: Open item in buffer
- **Custom Views:** (Press `a` in Agenda menu)
  - **P**: Projects (tags "PROJECT")
  - **H**: Home & Office (Daily + Tags)
  - **D**: Daily Action List
  - **W**: Weekly Plan (7-day agenda)
  - **M**: Monthly Plan (30-day agenda)
  - **Q**: Quarterly Plan (Year-view agenda)
  - **r**: Recently Added Projects (sorted)
  - **E**: Empty Projects (No active actions)
  - **S**: Stuck Projects (Missing NEXT action)
  - **o**: At the office (tags "@office")

## 🛠️ Core Org Keymaps
- **<TAB>**: Cycle visibility
- **M-RET**: New heading
- **M-S-RET**: New TODO heading
- **cit**: Change TODO state
- **<leader>oc**: Capture menu
- **<leader>oJ / <leader>oK**: Move subtree Down/Up
- **<leader>oH / <leader>oL**: Promote/Demote heading
- **<leader>os**: Schedule task
- **<leader>od**: Set deadline
- **<leader>ot**: Set tags
- **<leader>oi**: Clock in
- **<leader>oo**: Clock out

## 📊 Table Mode (`vim-table-mode`)
- **<leader>tm**: Toggle Table Mode
- **<leader>tr**: Realign table
- **|**: Trigger table creation (when in Table Mode)

## 📁 Shared File Structure (`~/vault/my/notebook/gtd/`)
- **0-Inbox/inbox.org**: New captures
- **GTD-System/next-actions.org**: Active tasks
- **GTD-System/tickler.org**: Reminders/Follow-ups
- **GTD-System/someday-maybe.org**: Ideas for later
- **GTD-System/waiting-for.org**: Delegated tasks
