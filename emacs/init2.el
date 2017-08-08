;; Load user-specified local config
;;(when (file-readable-p "~/.user-init.el")
;;  (load "~/.user-init.el"))
;;(when (file-readable-p "~/user-init.el")
;;  (load "~/user-init.el"))

;; Add "Common Lisp" functionality (not really, just a handy library
;; which duplicates many of Common Lisp's functions)

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(require 'cl)

;; The intelligent buffer switching package! so much nicer than
;; default.  Now C-x b becomes an "incremental search through buffers"
;; just begin typing any part of the name you want to find!  C-s
;; cycles through the choices until you find what you want, hit ENTER.
(require 'iswitchb)
(iswitchb-mode 1)

;; Turn off cruft.  This is EMACS, not ditzy stupid Notepad.
;; Real Programmers use the keyboard for everything.  If you're not,
;; time to learn.  See  C-h t  or  M-x help-with-tutorial

;;(when (fboundp 'tool-bar-mode) (call-interactively 'tool-bar-mode 0))
;;(call-interactively 'menu-bar-mode 0)
;;(when (fboundp 'scroll-bar-mode) (call-interactively 'scroll-bar-mode 0))

;; loading paths for libraries, set it to wherever you put the emacs/
;; directory that contains all these files.
(defvar emacs-root "~/my/notebook/")
(setq custom-file (concat "~/my/notebook-private/emacs/" "custom-config.el"))

;; convenience function for defining library paths
(defun add-local-path (p)
  (add-to-list 'load-path
               (concat emacs-root p)))

(add-local-path "elisp/")
(add-local-path "emacs/")
(add-local-path "emacs/lisp")

;; load sensitive information from a private file

;; load and set color theme.
;; You can get an interactive color-theme selector by using:
;;   M-x color-theme-select
(load-library "melpa-config")
(load-library "packages")
;;(add-local-path "/emacs/color-theme-6.6.0/")
;;(require 'color-theme)
;(load-library "color-theme")
;;(color-theme-initialize)
;(color-theme-charcoal-black)
;(color-theme-tango)
;(color-theme-classic)
;;(color-theme-subtle-hacker)
;;(color-theme-zenburn)
;;(load-theme 'zenburn t)

;; --

;(setq where 'work)
;(require 'custom)
(defgroup suresh nil "suresh's customization" :version "0.1")
(defcustom where "work"
  "where are you working from"
  :type 'string
  :group 'suresh
  )

; general integration
(add-local-path "emacs/site-lisp/")
(load-library "edit-server")
(require 'edit-server)
(setq edit-server-new-frame nil)
(edit-server-start)
;(edit-server-stop)


;;(load-library "init-frame-setup")
(load-library "shell-integration-config")
(load-library "navigation")
(load-library "ui-config")
(load-library "editing-helpers")
(load-library "emacs-config-adv")
(load-library "edit-helpers")
(load-library "general-programming")
(load-library "lisp-programming")
(autoload 'slime "slime-config" "Slime configuration" t)
(load-library "nxml-config")
(autoload 'sml-mode "sml-config" "SML configuration" t)
(autoload 'lua-mode "lua-config" "Lua configuration" t)
;;(load-library "haskell-config")
(load-library "ruby-config")
;(load-library "rails-config")
(load-library "scala-mode-config")
(load-library "haskell-mode-config")
(load-library "muse-config")
(load-library "modes-config")
;;(load-library "xref-config")

(load-library "misc")
(load-library "bbdb-config")
(load-library "cedet-config")
;;(load-library "jde-config")
(load-library "ecb-config")
(load-library "scala-config")
(load-library "csharp-config")
;;(load-library "java-config")
(load-library "coffeescript-config")
(load-library "org-mode-config")
(load-library "org-config")
;(load-library "dotgnus")
(load-library "cpp-config")
(load-library "yas-config")
(load-library "color-theme-config")
(load-library "org-journal-config")
(load-library "clojure-config")
(load-library "smart-compile-config")

(when (file-exists-p custom-file)
	(load custom-file))

;(set-default-font " -xos4-xos4 Terminus-normal-normal-normal-*-14-*-*-*-c-80-iso10646-1")
(put 'scroll-left 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)


;; utilities
(defun replace-last-sexp ()
  (interactive)
  (let ((value (eval (preceding-sexp))))
    (kill-sexp -1)
    (insert (format "%S" value))))

;; get el-get lazy
;; So the idea is that you copy/paste this code into your *scratch* buffer,
;; hit C-j, and you have a working el-get.
;; (url-retrieve
;;  "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el"
;;  (lambda (s)
;;    (goto-char (point-max))
;;    (eval-print-last-sexp)))



(defun move-line-up ()
  (interactive)
  (transpose-lines 1)
  (forward-line -2))

(defun move-line-down ()
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1))

(global-set-key (kbd "M-<up>") 'move-line-up)
(global-set-key (kbd "M-<down>") 'move-line-down)
