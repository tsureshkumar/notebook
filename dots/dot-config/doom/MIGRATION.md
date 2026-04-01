# Emacs Migration: Legacy to Modern Doom

This document maps your old Spacemacs and custom Emacs configurations to the new, optimized Doom Emacs setup.

## Core UI & Experience
| Feature | Legacy Setup | Modern Doom Equivalent | Status |
|:---|:---|:---|:---|
| **Completion Engine** | `iswitchb`, `Helm`, `ido` | **Vertico + Corfu** | **Upgrade** (Faster, non-blocking) |
| **Theme Engine** | `color-theme.el` | **Doom Themes** | **Upgrade** (Optimized for performance) |
| **Modeline** | `powerline`, `spaceline` | **Doom Modeline** | **Upgrade** (Clean, fast, info-rich) |
| **Search/Narrowing** | `isearch-regexp` | **Consult + Orderless** | **Upgrade** (Fuzzy search everywhere) |

## Programming & IDE Features
| Feature | Legacy Setup | Modern Doom Equivalent | Status |
|:---|:---|:---|:---|
| **C/C++** | `cedet`, `ecb` | **LSP (clangd)** | **Upgrade** (Robust, industry standard) |
| **Scala** | `ensime` | **LSP (Metals)** | **Upgrade** (Full IDE features) |
| **Lisp/Paredit** | `paredit.el` | **Smartparens** | **Consolidated** (Integrated in `:config default`) |
| **Formatting** | `die-tabs` | **Apheleia / Format-on-save** | **Upgrade** (Standard tools like Prettier, Black) |

## Tools & Productivity
| Feature | Legacy Setup | Modern Doom Equivalent | Status |
|:---|:---|:---|:---|
| **Git** | `git-config.el` | **Magit** | **Consolidated** (Native Doom module) |
| **Org Mode** | Fragmented `.el` files | Consolidated `:lang org` | **Cleanup** (Integrated `+pretty` & `+roam2`) |
| **GTD** | Custom `gtd` function | Integrated `Org-Agenda` | **Cleanup** (Integrated Refile & Agenda) |
| **Snippets** | `yasnippet` | **Doom Snippets** | **Consolidated** (Extensible via `snippets/`) |

## Obsolete Configurations Removed
- **`cedet`, `ecb`**: These were heavy and often fragile; replaced by the lightweight and powerful LSP.
- **`iswitchb`, `ido`**: These older completion mechanisms were replaced by the much more cohesive Vertico stack.
- **`ensime`**: This was the primary Scala IDE engine, but it is now deprecated in favor of Metals, which is more reliable.
- **`w3m`, `erc`**: These are now available as modular options if needed, but removed to keep the core configuration lean.
