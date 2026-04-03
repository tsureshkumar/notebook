# Org Mode & GTD: One-Page Cheatsheet

---

## 🐘 Emacs (Org-mode)

### Custom Keybindings
- **C-c l**: Store link | **C-c c** / **C-c C-c**: Capture
- **C-c a**: Agenda | **C-c b**: Switch Org buffers | **C-c g**: Open Inbox

### GTD Files (Emacs)
- **Inbox**: `~/.../org/inbox.org` | **GTD**: `~/.../org/gtd.org`
- **Tickler**: `~/.../org/tickler.org` | **Someday**: `~/.../org/someday.org`

---

## 🌙 Neovim (nvim-orgmode + GTD)

### Custom GTD Keybindings (`gtd.lua`)
- **<leader>gc**: Capture | **<leader>gp**: Clarify | **<leader>gr**: Reflect
- **<leader>ge**: Engage | **<leader>gi**: Inbox | **<leader>gn**: Next Actions
- **<leader>sn**: Search Notes | **<leader>st**: Search Tasks

### GTD Files (Neovim)
- **Inbox**: `~/.../gtd/0-Inbox/inbox.org`
- **Next**: `~/.../gtd/GTD-System/next-actions.org`
- **Waiting**: `~/.../gtd/GTD-System/waiting-for.org`

### Standard nvim-orgmode
- **<TAB>**: Visibility | **cit**: TODO state | **g?**: Help
- **<leader>oa**: Agenda | **<leader>oc**: Capture | **M-RET**: New heading

---

## 🗓️ Agenda View (Both Emacs & Neovim)

### Filtering & Restriction
- **/** (Emacs) / **f** (Nvim): Filter agenda by tag
- **<** / **>** (Emacs): Restrict to current buffer / Clear restriction
- **v** (Emacs): View options (Log, Archive, etc.)
- **r** (Both): Rebuild/Refresh agenda
- **s** (Both): Save all Org buffers

### Custom Agenda Commands (Emacs)
- **C-c a P**: Projects view (`tags "PROJECT"`)
- **C-c a H**: Home/Office Lists
- **C-c a D**: Daily Action List

---

## 🛠️ Core Org Concepts

### Structure Editing
- **M-Left/Right**: Promote/Demote | **M-Up/Down**: Move subtree
- **M-RET**: New heading | **M-S-RET**: New TODO heading

### TODO & Scheduling
- **C-c C-t** (Emacs) / **cit** (Nvim): Rotate TODO state
- **C-c C-s**: Schedule | **C-c C-d**: Deadline | **C-c .**: Timestamp

### Links & Tables
- **C-c C-l**: Insert link | **C-c C-o**: Open link | **C-c |**: Create table
- **TAB / RET**: Next cell/row | **M-Left/Right**: Move column
