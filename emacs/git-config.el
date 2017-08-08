(add-local-path (concat emacs-root "emacs/site-lisp/magit-1.2.0/"))
;;(add-to-list 'load-path/ (concat emacs-root "emacs/site-lisp/magit-1.2.0"))
(require 'magit)
(require 'magit-svn)

;; mo-git-blame
;;(add-to-list 'load-path/ (concat emacs-root "emacs/site-lisp/mo-git-blame"))
(autoload 'mo-git-blame-file "mo-git-blame" nil t)
(autoload 'mo-git-blame-current "mo-git-blame" nil t)
