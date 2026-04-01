(add-local-path "emacs/sml-mode-4.0/")
(require 'sml-mode)
(require 'sml-proc)
(setq sml-compile-command "CM.make \"sources.cm\"")
(add-to-list 'sml-compile-commands-alist 
             (cons "CM.make \"sources.cm\"" "sources.cm"))
