;;(add-to-list 'load-path/ "~/notebook/emacs/site-lisp/cedet-1.0pre6/common")
;;(add-to-list 'load-path/ "~/notebook/emacs/site-lisp/cedet-1.0pre6/semantic")
;;(add-to-list 'load-path/ "~/notebook/emacs/site-lisp/cedet-1.0pre6/eieio")
;;(add-to-list 'load-path/ "~/notebook/emacs/site-lisp/elib-1.0")

;;;_+ semantic stuff
(require 'eieio)
(require 'cedet)
(setq semantic-load-turn-everything-on t)
(require 'semantic-load)

;; i dislike annoying modes
;(global-semantic-show-dirty-mode -1)
;(global-semantic-show-unmatched-syntax-mode -1)

; I like the current function name at the top of file
(semantic-load-enable-excessive-code-helpers)

;;;_- semantic stuff end
;;(add-to-list 'load-path/ (expand-file-name "~/notebook/emacs/site-lisp/jde/lisp"))

(setq debug-on-error t)
;; If you want Emacs to defer loading the JDE until you open a 
;; Java file, edit the following line
(setq defer-loading-jde nil)
;; to read:
;;
;;  (setq defer-loading-jde t)
;;

;;(require 'jde)

(if defer-loading-jde
    (progn
      (autoload 'jde-mode "jde" "JDE mode." t)
      (setq auto-mode-alist
	    (append
	     '(("\\.java\\'" . jde-mode))
	     auto-mode-alist)))
  (require 'jde))

(setq jde-doc-dir "/usr/lib/jvm/java")


;; Sets the basic indentation for Java source files
;; to two spaces.
(defun my-jde-mode-hook ()
  (setq c-basic-offset 2))

(add-hook 'jde-mode-hook 'my-jde-mode-hook)

;; Include the following only if you want to run
;; bash as your shell.

;; Setup Emacs to run bash as its primary shell.
(setq shell-file-name "bash")
(setq shell-command-switch "-c")
(setq explicit-shell-file-name shell-file-name)
(setenv "SHELL" shell-file-name)
(setq explicit-sh-args '("-login" "-i"))