(defmacro sacha/gnu-vs-x (a b)
  "Execute A under GNU Emacs and B under XEmacs."
  `(if (featurep 'xemacs)
       ,b
     ,a))

; I like menus under XEmacs, but not under GNU Emacs.
;; (set-specifier menubar-visible-p t)
;(unless (featurep 'xemacs) (menu-bar-mode -1))

;; No toolbars or scrollbars in either.
(sacha/gnu-vs-x (tool-bar-mode -1) (set-specifier default-toolbar-visible-p t))
(sacha/gnu-vs-x (scroll-bar-mode -1) (set-specifier scrollbar-width (cons (selected-frame) 0)))

;; For syntax highlighting of random files.
(unless (featurep 'xemacs)
  (require 'generic-x))

(sacha/gnu-vs-x (set-coding-priority '(coding-category-utf-8))
		(progn
		  (require 'un-define)
		  (set-coding-priority-list '(utf-8))
		  (set-coding-category-system 'utf-8 'utf-8)))
 
(defun sacha/always-recover-autosave-file ()
  "Attempt to recover file."
  (condition-case err (recover-this-file) (error nil)))
(add-hook (sacha/gnu-vs-x 'find-file-hook
			  'find-file-hooks) 
	  'sacha/always-recover-autosave-file)

;; Get around recently-added emacs bug that made add-hook ignore
;; the auto-local of some variables
(make-variable-buffer-local 'local-write-file-hooks)

;; I'm lazy.
(defalias 'yes-or-no-p 'y-or-n-p)

(quietly-read-abbrev-file)

;; Add very basic syntax highlighting for all sorts of files

;; Allow other processes to connect to this Emacs
;(unless (featurep 'xemacs)
;  (load "/etc/emacs-snapshot/site-start.d/50gnuserv.el"))
;(gnuserv-start)

(auto-compression-mode t)

;; For some reason, many people don't like tabs.
(setq-default indent-tabs-mode nil)

;; So that horizontally-split windows will wrap unless told otherwise
(setq truncate-partial-width-windows nil)

;; Incremental completion for the minibuffer.
(icomplete-mode 1)

(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(setq debian-changelog-full-name "Sureshkumar T")
(setq change-log-default-name nil)


(set-cursor-color "blue")

