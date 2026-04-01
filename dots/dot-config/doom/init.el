;;; init.el -*- lexical-binding: t; -*-

(doom! :completion
       (corfu +orderless)
       vertico

       :ui
       doom
       doom-dashboard
       hl-todo
       modeline
       ophints
       (popup +defaults)
       (vc-gutter +pretty)
       vi-tilde-fringe
       workspaces
       treemacs

       :editor
       (evil +everywhere)
       file-templates
       fold
       (format +onsave)
       snippets
       (whitespace +guess +trim)

       :emacs
       dired
       electric
       undo
       vc

       :term
       vterm

       :checkers
       syntax
       (spell +flyspell)

       :tools
       (eval +overlay)
       lookup
       magit
       docker
       editorconfig
       (lsp +eglot)
       pdf

       :os
       (:if (featurep :system 'macos) macos)

       :lang
       (cc +lsp)
       (go +lsp)
       (java +lsp)
       (javascript +lsp)
       (lua +lsp)
       (python +lsp)
       (rust +lsp)
       (scala +lsp)
       sh
       (typescript +lsp)
       data
       json
       yaml
       markdown
       (org +pretty +dragndrop +present +roam2)
       latex
       plantuml

       :config
       (default +bindings +smartparens))
