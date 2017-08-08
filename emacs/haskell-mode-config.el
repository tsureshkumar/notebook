(add-local-path (concat emacs-root "emacs/site-lisp/haskell-mode-2.8.0"))
;;(add-to-list 'load-path/ (concat emacs-root "emacs/site-lisp/haskell-mode-2.8.0"))

(require 'haskell-mode)
(require 'haskell-indent)
(require 'inf-haskell)

(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
;;(add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)

(add-hook 'haskell-mode-hook 'font-lock-mode)

(require 'yasnippet)

;;(require 'yasnippet-bundle)
(add-hook 'haskell-mode-hook
          '(lambda ()
             (yas/minor-mode-on)))

(setq auto-mode-alist
      (cons '("\\.\\(hs\\)\\'" . haskell-mode)
                       auto-mode-alist))

(setq delete-selection-mode nil)


(setq haskell 'ghc)
;    (async-shell-command (format "%s %s -rtsopts -prof -auto-all -caf-all" haskell buffer-file-name))
(defun auto-compile-haskell ()
  (save-window-excursion
    (async-shell-command (format "%s %s -threaded -rtsopts -auto-all -caf-all" haskell buffer-file-name))))

(defun haskell-custom ()
  "haskell-mode-hook"
  (make-local-variable 'tab-width)
  (set 'tab-width 2)
  (add-hook 'after-save-hook 'auto-compile-haskell nil 'make-it-local)
  )

(add-hook 'haskell-mode-hook 'haskell-custom)

(eval-after-load 'haskell-mode
          '(define-key haskell-mode-map [f8] 'haskell-navigate-imports))

; intero http://commercialhaskell.github.io/intero/
(add-hook 'haskell-mode-hook 'intero-mode)



