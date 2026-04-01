# Doom Emacs Configuration

This is my consolidated, modern Doom Emacs configuration. It replaces my legacy Emacs and Spacemacs setups with a fast, lightweight environment optimized for systems engineering.

## Setup Instructions

1.  **Install Emacs 29+** (preferably with native-comp).
2.  **Install Doom Emacs**:
    ```bash
    git clone --depth 1 https://github.com/doomemacs/doomemacs ~/.config/emacs
    ~/.config/emacs/bin/doom install
    ```
3.  **Link this configuration**:
    ```bash
    # Assuming this repo is cloned at ~/my/notebook
    rm -rf ~/.config/doom
    ln -s ~/my/notebook/dots/dot-config/doom ~/.config/doom
    ```
4.  **Synchronize**:
    ```bash
    doom sync
    ```

## Modern Features
- **LSP Support**: C/C++, Go, Python, Scala, Rust, JS/TS via Eglot.
- **Org Mode**: Integrated with GTD and agenda files in `~/org/`.
- **Nvim Keybindings**: `jk` to escape, `ss`/`sv` for splits, and search centering.
- **Custom Modes**: D2 diagrams and Quint language support.

For details on how this replaces your old configuration, see [MIGRATION.md](./MIGRATION.md).
