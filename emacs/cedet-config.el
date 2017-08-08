;;(add-to-list 'load-path/ "~/notebook/emacs/site-lisp/cedet-1.1/lisp/cedet")
;;(add-to-list 'load-path/ "~/notebook/emacs/site-lisp/cedet-1.1/lisp/eieio")
(require 'cedet)

(require 'semantic/ia)
(require 'semantic/bovine/gcc)
(require 'semantic/db-ref)
(semantic-mode 1)



(setq-mode-local c-mode semanticdb-find-default-throttle
                 '(project unloaded system recursive))
