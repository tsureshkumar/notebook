(defun my-orgtbl-mode-config ()
       "Modify key binding for orgtbl mode"
       (org-defkey orgtbl-mode-map "\C-c\C-x\C-w" (orgtbl-make-binding 'org-table-cut-region 1001 "\C-c\C-x\C-w"))
       (org-defkey orgtbl-mode-map "\C-c\C-x\M-w" (orgtbl-make-binding 'org-table-copy-region 1002 "\C-c\C-x\C-w"))
       (org-defkey orgtbl-mode-map "\C-c\C-x\C-y" (orgtbl-make-binding 'org-table-paste-rectangle 1003 "\C-c\C-x\C-w"))
       )

(my-orgtbl-mode-config)

(add-hook 'org-mode-hook 'my-orgtbl-mode-config)

;(eval-after-load "org-table" (
;  '(define-key org-table-map "\C-c \C-x \C-w" 'org-table-cut-region)
;  '(define-key org-table-map "\C-c \C-x \C-y" 'org-table-paste-rectangle)
;  '(define-key org-table-map "\C-c \C-x M-w" 'org-table-copy-region)
;  ))
