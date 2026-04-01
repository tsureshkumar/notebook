;;;; Emacs Lisp tweaks to Common Lisp IDEs

(autoload 'paredit-mode "paredit"
  "Minor mode for pseudo-structurally editing Lisp code."
  t)
(add-hook 'lisp-mode-hook (lambda () (paredit-mode +1)))
(add-hook 'emacs-lisp-mode-hook (lambda () (paredit-mode +1)))

(defun unwrap-next-sexp (n)
  "Convert (x ...) to ..."
  (interactive "p")
  (forward-sexp)
  (backward-delete-char 1)
  (backward-up-list)
  (delete-char 1)
  (kill-sexp n)
  (fixup-whitespace)
  (lisp-indent-line))

(global-set-key "\M-_" 'unwrap-next-sexp)


;; make [ ] act like ( )
(modify-syntax-entry ?[ "(]" lisp-mode-syntax-table)
(modify-syntax-entry ?] ")[" lisp-mode-syntax-table)

;; make mic-paren less annoying
(setq paren-sexp-mode nil)
