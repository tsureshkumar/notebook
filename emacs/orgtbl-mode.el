(defun my-orgtbl-mode-config ()
       "Modify key binding for orgtbl mode"
       (local-set-key (kbd "\C-c \C-x \C-w") 'org-table-cut-region)
       (local-set-key (kbd "\C-c \C-x \C-w") 'org-table-cut-region)
       (local-set-key (kbd "\C-c \C-x \C-w") 'org-table-cut-region)
       )

(add-hook 'org-mode-hook 'my-orgtbl-mode-config)

;(eval-after-load "org-table" (
;  '(define-key org-table-map "\C-c \C-x \C-w" 'org-table-cut-region)
;  '(define-key org-table-map "\C-c \C-x \C-y" 'org-table-paste-rectangle)
;  '(define-key org-table-map "\C-c \C-x M-w" 'org-table-copy-region)
;  ))
