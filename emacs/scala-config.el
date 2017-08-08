;;(add-to-list 'load-path/ "/home/tsureshkumar/notebook/emacs/site-lisp/scala-emacs/")
;;(require 'scala-mode-auto)
(require 'scala-mode)

;; Load the ensime lisp code...
;;(add-to-list 'load-path "/home/tsureshkumar/notebook/emacs/site-lisp/ensime-git/ensime/elisp")
;;(add-to-list 'load-path "/home/tsureshkumar/notebook/emacs/site-lisp/ensime_2.8.1-0.5.0/elisp")
;;(require 'ensime)

;; This step causes the ensime-mode to be started whenever
;; scala-mode is started for a buffer. You may have to customize this step
;; if you're not using the standard scala mode.
;;(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)

;; MINI HOWTO:
;; Open .scala file. M-x ensime (once per project)


(setq scalac 'scalac)
;(setq scalac 'fsc)

(defun auto-compile-scala ()
  (save-window-excursion
    (async-shell-command (format "%s %s" scalac buffer-file-name))))

(defun scala-custom ()
  "scala-mode-hook"
  (make-local-variable 'tab-width)
  (set 'tab-width 2)
  (add-hook 'after-save-hook 'auto-compile-scala nil 'make-it-local)
  )

(add-hook 'scala-mode-hook 'scala-custom)


