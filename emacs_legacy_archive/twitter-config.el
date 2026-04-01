(add-local-path "emacs/site-lisp/twitter/")
(add-local-path "emacs/site-lisp/emacs-oauth/")
(add-local-path "emacs/site-lisp/ublog.el/")


(autoload 'twitel-get-friends-timeline "twitel" nil t)
(autoload 'twitel-status-edit "twitel" nil t)
(global-set-key "\C-xt" 'twitel-get-friends-timeline)
(add-hook 'twitel-status-edit-mode-hook 'longlines-mode)


(require 'twitel)
(require 'ublog)