(add-hook 'csharp-mode-hook (function cscope:hook))


(global-set-key (kbd "C-c C-a s") 'cscope-find-this-symbol)
(global-set-key (kbd "C-c C-a f") 'cscope-find-global-definition)
(global-set-key (kbd "C-c C-a u") 'cscope-pop-mark)
