# Notebook

A personal systems engineering and polyglot development environment. Dotfiles, editor configs, utility scripts, and tooling for Neovim, Vim, Emacs, zsh, tmux, and Xmonad.

---

## Structure

```
notebook/
├── dots/              # Dotfiles and configs (symlink to ~)
├── bin/               # Executable utilities (add to PATH)
├── scripts/           # Helper scripts
├── emacs/             # Emacs config and site-lisp
├── vim/               # Vim templates
├── apps/              # Desktop/browser apps
├── notes/             # Personal notes
└── my-source-samples/ # Code samples
```

---

## Dots

Configuration files intended to be symlinked into `$HOME` or `~/.config`.

| File/Dir | Target | Description |
|----------|--------|-------------|
| `dot-zshrc` | `~/.zshrc` | zsh config (oh-my-zsh, conda, PATH, colors) |
| `dot-vimrc` | `~/.vimrc` | Vim config (polyglot, systems-oriented) |
| `dot-config/nvim` | `~/.config/nvim` | Neovim config (pure Lua) |
| `dot-tmux.conf` | `~/.tmux.conf` | Tmux config |
| `dot-Xresources` | `~/.Xresources` | X resources |
| `dot-oh-my-zshrc` | (sourced by zshrc) | oh-my-zsh plugins and theme |
| `dot-bash-rc` | `~/.bashrc` | Bash config |
| `dot-spacemacs` | (Spacemacs) | Spacemacs layer config |
| `neosnippets/` | `~/.config/nvim/snippets` or vim runtime | Snippets (cpp, python) |
| `xmonad/` | `~/.xmonad` | Xmonad tiling WM config |

### Symlink setup

```bash
# Example: link Neovim config
ln -sf "$HOME/my/notebook/dots/dot-config/nvim" "$HOME/.config/nvim"

# Example: link zshrc
ln -sf "$HOME/my/notebook/dots/dot-zshrc" "$HOME/.zshrc"
```

---

## Neovim

Pure Lua configuration with lazy.nvim. Systems-oriented, polyglot setup.

### Features

- **Themes**: Kanagawa, Gruvbox, Everforest, Rose Pine, Zenbones, Catppuccin, PaperColor
- **Navigation**: Telescope (find files, grep, buffers), Oil.nvim (file explorer), vim-tmux-navigator
- **LSP**: Mason + mason-lspconfig, clangd, lua_ls, pyright, ts_ls, gopls
- **DAP**: nvim-dap with codelldb, breakpoints, continue
- **Completion**: nvim-cmp, LSP, buffer, path, LuaSnip
- **Formatting**: conform.nvim (stylua, black, isort, clang-format, prettier)
- **Specialized**: Metals (Scala), Obsidian.nvim, vimtex, Copilot, gitsigns, fugitive

### Key bindings

| Key | Action |
|-----|--------|
| `\` | Leader |
| `\f` | Telescope find files |
| `\g` | Telescope live grep |
| `\b` | Telescope buffers |
| `\u` | Undotree |
| `-` | Oil (edit dir as buffer) |
| `gd` | LSP definition |
| `K` | LSP hover |
| `\rn` | LSP rename |
| `\ca` | Code action |
| `<F7>` | Make |
| `<F5>` | Run compiled binary |
| `ga` | EasyAlign |

### Agent Bridge

`lua/agent-bridge.lua` — TCP server (default `127.0.0.1:7777`) that lets external tools (e.g. AI agents) interact with Neovim safely:

- **get_state**: Buffer content, cursor, selection, diagnostics
- **apply_edits**: Apply text edits with approval
- **exec_ex**: Run allowlisted ex commands
- **run_shell**: Run shell commands (with approval prompt)

Start with `:AgentBridgeStart`, stop with `:AgentBridgeStop`. Shell commands and non-allowlisted ex commands require user confirmation.

---

## Vim

Legacy Vim config (`dot-vimrc`) — systems-oriented, polyglot. Mirrors Neovim options and keymaps where applicable.

---

## Bin

Utilities in `bin/` — add to PATH, e.g. `export PATH="$HOME/my/notebook/bin:$PATH"`.

| Script | Purpose |
|--------|---------|
| `timer.py` | Pomodoro-style timer |
| `csv2json.py` | CSV → JSON |
| `ldap2csv.py` | LDAP → CSV |
| `http_dump.py` | HTTP traffic dump |
| `ajp_dump.py` | AJP protocol dump |
| `python-proxy.py` | Python HTTP proxy |
| `img_clip_capture.py` | Screenshot/clip capture |
| `decrypt-pdf.sh` | PDF decryption |
| `ssl_decrypt.sh` | SSL decrypt helper |
| `uml_text2svg.sh` | UML text → SVG |
| `vimwiki-customwiki-markdown-converter.sh` | Vimwiki → Markdown |
| `ediff3merge.sh` | Git merge driver |
| `photo.sh`, `photo-frame.sh` | Photo utilities |
| `docker-registry.sh` | Docker registry helper |

---

## Emacs

Emacs config in `emacs/` — Spacemacs, CEDET, language-specific configs (Haskell, Clojure, Scala, Go, etc.), ERC, BBDB, AUCTeX.

---

## Snippets

- **neosnippets/**: cpp.snip, python.snip
- **vim-neosnippets/**: cpp.snip (vim-snippets compatible)

---

## Requirements

- **Neovim**: Neovim 0.9+ (0.10+ for `vim.system()` in agent-bridge)
- **LSP**: clangd, lua_ls, pyright, etc. (installed via Mason)
- **Formatters**: stylua, black, isort, clang-format, prettier
- **zsh**: oh-my-zsh, conda (optional)

---

## License

Personal configuration — use and adapt as needed.
