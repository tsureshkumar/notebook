(add-local-path "emacs/site-lisp/muse-3.20/lisp")
;(add-to-list 'load-path "/home/tsureshkumar/notebook/emacs/dev/muse/lisp")
;(add-to-list 'load-path "/home/sacha/notebook/emacs")
;(add-to-list 'load-path "/home/sacha/notebook/emacs/planner-muse")

(setq muse-mode-auto-p t)
(require 'muse)
(require 'muse-mode)
(require 'muse-project)
(require 'muse-html)
(require 'muse-journal)
(require 'muse-colors)
(require 'muse-wiki)
(require 'muse-publish)

;; (setq muse-project-alist
;;       '(("website"			; my various writings
;; 	 ("~/Pages" :default "index")
;; 	 (:base "html" :path "~/public_html"))
;;       ))


(add-to-list 'muse-project-alist
             `("website"
               (,@(muse-project-alist-dirs "~/my/notebook/public/muse/writings")
                  :default "index")
               (:base "html" :path "~/public_html/www"))
             )

(add-to-list 'muse-project-alist
             `("Journal" 
	       	(,@(muse-project-alist-dirs "~/my/notebook/private/muse/journal")
                           :default "journal")
                (:base "journal-html" 
                       :path "~/public_html/private/journal")))


;;; Muse utility functions
;; for journal
(defun muse-insert-date ()
  (interactive)
  (insert (format-time-string "%Y%m%d" (current-time)))
  )

(defun muse-insert-today ()
  (interactive)
  (insert (format-time-string "%A, %d %B %Y "))
  )

(defun muse-open-today ()
  (interactive)
  (insert "* ")
  (muse-insert-date)
  (insert ": ")
  (muse-insert-today)
  (insert "\n")
  )

(defun journal-this-month ()
"journal's file name for this month"
  (format-time-string "%B%Y"))
(defun journal-today ()
"journals today's default headline"
  (format-time-string "%A, %d %B %Y"))
(defun journal-today-entry ()
"journal's start entry. this is the entry of which today is found in a journal file"
  (format-time-string "%Y%m%d"))

(defun journal ()
  (interactive)
  (setq journal-dir "~/my/notebook/private/muse/journal/")
  (find-file (concat journal-dir (journal-this-month)))
  (search-forward-regexp  (concat (journal-today-entry) ".*$") )
  )

