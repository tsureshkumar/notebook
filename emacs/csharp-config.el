;;; Helpers for building regexps.
(defmacro c-paren-re (re)
`(concat "\\(" ,re "\\)"))
(defmacro c-identifier-re (re)
`(concat "\\[^_]"))

(autoload 'csharp-mode "csharp-mode" "Major mode for editing C# code." t)

(setq auto-mode-alist
      (append '(("\\.cs$" . csharp-mode)) auto-mode-alist))

;;(defun my-csharp-mode-fn ()
;;"function that runs when csharp-mode is initialized for a buffer."
;;)

;;(add-hook  'csharp-mode-hook 'my-csharp-mode-fn t)




(add-hook 'csharp-mode-hook 'cscope-bind-keys-3deep)

(local-set-key "\C-c\C-ss" 'cscope-find-this-symbol)
(local-set-key "\C-c\C-as" 'cscope-find-this-symbol)

