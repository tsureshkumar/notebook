;; Causes nXML and associated libs to be loaded "on-demand"
(add-local-path "emacs/site-lisp/nxml-mode-20041004/")
;;(load-library "rng-auto")

;; Treat elements and contents like S-expressions!  Oh, the magic.
;; (if you know S-expression movement commands, it's great)
(setq-default nxml-sexp-element-flag t)

;; Whenever you type </ it will fill out the rest.
(setq-default nxml-slash-auto-complete-flag t)

;; Causes files with extensions .xml .xsl .rng .xhtml .html and .tal
;; to invoke nxml-mode.
(setq auto-mode-alist
      (cons '("\\.\\(xml\\|xsl\\|rng\\|xhtml\\|html\\|tal\\)\\'" . nxml-mode)
            (remove-if (lambda (x)
                         (eq (cdr x) 'html-mode))
                       auto-mode-alist)))
(defun my-nxml-mode-hook ()
"Functions to run when in nxml mode."
(setq nxml-sexp-element-flag t)
(hs-minor-mode 1))

(add-hook 'nxml-mode-hook 'my-nxml-mode-hook)

(eval-after-load "hideshow.el"
(let ((nxml-mode-hs-info '(nxml-mode ("^\\s-*\\(<[^/].*>\\)\\s-*$" 1)
"^\\s-*</.*>\\s-*$")))
(when (not (member nxml-mode-hs-info hs-special-modes-alist))
(setq hs-special-modes-alist
(cons nxml-mode-hs-info hs-special-modes-alist)))))
