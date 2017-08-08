(add-local-path (concat emacs-root "emacs/site-lisp/scala-mode/"))
;;(add-to-list 'load-path/ (concat emacs-root "emacs/site-lisp/scala-mode/"))
(autoload 'scala-mode "scala-mode.el")
(require 'scala-mode)
;;(require 'scala-mode-auto)

;# yasnip bundle install
;;(add-to-list 'load-path/ (concat emacs-root "emacs/site-lisp/"))
;(require 'yasnippet-bundle)

;;;# yasnippet seperate install
;;(add-to-list 'load-path/ (concat emacs-root "emacs/site-lisp/yasnippet-0.5.10"))
(require 'yasnippet)

(add-hook 'scala-mode-hook
          '(lambda ()
             (yas/minor-mode-on)))
          
