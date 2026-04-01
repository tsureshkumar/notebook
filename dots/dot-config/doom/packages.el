;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

(package! d2-mode)
(package! ox-reveal)
(package! org-bullets)

;; Quint support (not on MELPA)
(package! quint-mode
  :recipe (:host github :repo "informalsystems/quint" :files ("editor-plugins/emacs/quint-mode.el")))

(package! lsp-quint
  :recipe (:host github :repo "informalsystems/quint" :files ("editor-plugins/emacs/lsp-quint.el")))
