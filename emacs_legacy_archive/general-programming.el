(global-font-lock-mode t)
(setq font-lock-maximum-decoration t)

;; tabs considered harmful
(setq-default indent-tabs-mode nil)

(set-face-bold-p 'font-lock-builtin-face nil)
(set-face-bold-p 'font-lock-comment-face nil)
(set-face-bold-p 'font-lock-function-name-face nil)
(set-face-bold-p 'font-lock-keyword-face nil)
(set-face-bold-p 'font-lock-variable-name-face nil)
(set-face-foreground 'font-lock-builtin-face "cyan")
(set-face-foreground 'font-lock-comment-face "pale green")
(set-face-foreground 'font-lock-function-name-face "green")
(set-face-foreground 'font-lock-variable-name-face "pale green")

(load-library "xcscope" )

(global-set-key (kbd "C-x a r") 'align-regexp)

(defun xah-insert-random-string (*n)
  "Insert a random alphanumerics string of length 10.
The possible chars are 0 to 9, and a to z (lower case).
Call `universal-argument' before for different count.
URL `http://ergoemacs.org/emacs/elisp_insert_random_number_string.html'
Version 2016-01-12"
  (interactive "P")
  (let* ((-charset "1234567890abcdefghijklmnopqrstuvwxyz")
         (-baseCount (length -charset)))
    (dotimes (-i (if (numberp *n) (abs *n) 10))
      (insert (elt -charset (random -baseCount))))))

