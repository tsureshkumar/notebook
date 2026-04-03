# Emacs (Doom) Org Mode & GTD Cheatsheet

## 🐘 GTD Workflow (Doom Emacs)
- **SPC X**: Org Capture
- **SPC a o**: Org Agenda
- **SPC n s**: Search Notes (Roam)
- **SPC n b**: Switch Org buffer

## 🗓️ Agenda View (Doom Emacs)
- **SPC a o**: Open Agenda Menu
- **/**: Filter agenda by tag
- **< / >**: Restrict to current buffer / Clear
- **v**: View options (Log, Archive)
- **r**: Rebuild/Refresh agenda
- **s**: Save all Org buffers
- **Custom Views:**
  - **P**: Projects (tags "PROJECT")
  - **H**: Home & Office (Daily + Tags)
  - **D**: Daily Action List
  - **o**: At the office (tags "@office")

## 🛠️ Core Org Keymaps (Doom/Evil)
- **TAB**: Cycle visibility
- **M-RET**: New heading
- **M-S-RET**: New TODO heading
- **t**: Cycle TODO state (in Agenda)
- **C-c C-t**: Cycle TODO state (in Buffer)
- **M-j / M-k**: Move subtree Down/Up
- **M-h / M-l**: Promote/Demote heading
- **SPC m s**: Schedule task
- **SPC m d**: Set deadline
- **SPC m t**: Set tags
- **SPC m i**: Clock in
- **SPC m o**: Clock out

## 📊 Table Mode (Emacs Built-in)
- **C-c |**: Create table
- **TAB / RET**: Next cell/row
- **M-Left/Right**: Move column
- **M-Up/Down**: Move row

## 📁 Shared File Structure (`~/vault/my/notebook/gtd/`)
- **0-Inbox/inbox.org**: New captures
- **GTD-System/next-actions.org**: Active tasks
- **GTD-System/tickler.org**: Reminders/Follow-ups
- **GTD-System/someday-maybe.org**: Ideas for later
- **GTD-System/waiting-for.org**: Delegated tasks
