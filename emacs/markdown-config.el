(add-hook 'markdown-mode-hook
          (lambda ()
            (auto-fill-mode 0)
            (visual-line-mode 1)
            (local-set-key (kbd "C-c b") 'insert-markdown-inline-math-block)) t)

;; (add-hook 'after-save-hook 'mark
;;           (lambda () (when (eq major-mode 'twiki-mode) 
;;                        (twiki-render-for-edit)
;;                        (set-buffer-modified-p nil)
;;                        )))

(setq markdown-css-path 
      (list
;       "~/my/notebook/emacs/other-files/css/github-markdown.css"
       "~/my/notebook/emacs/other-files/css/markdown/Laguna%20Sunset.css"
       ))
(setq markdown-css-paths ()
      )

(eval-after-load "markdown-mode"
  '(defalias 'markdown-add-xhtml-header-and-footer 'as/markdown-add-xhtml-header-and-footer))

(defun as/markdown-add-xhtml-header-and-footer (title)
    "Wrap XHTML header and footer with given TITLE around current buffer."
    (goto-char (point-min))
    (insert "<!DOCTYPE html5>\n"
	    "<html>\n"
	    "<head>\n<title>")
    (insert title)
    (insert "</title>\n")
    (insert "<meta charset=\"utf-8\" />\n")
    (when (> (length markdown-css-path) 0)
      (insert (mapconcat 'markdown-stylesheet-link-string markdown-css-path "\n")))
    (insert "\n</head>\n\n"
	    "<body>\n\n<div class='markdown-body'>\n\n")
    (goto-char (point-max))
    (insert "\n</div>\n"
	    "</body>\n"
	    "</html>\n"))


;; customes markdown toc
(require 'markdown-toc)
(custom-set-variables
 '(markdown-toc-header-toc-start "[//]: # (toc start)")
; '(markdown-toc-header-toc-title "**customized title**")
 '(markdown-toc-header-toc-end "[//]: # (toc end)")
)
