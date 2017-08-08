(global-set-key "\C-x5\M-x" 'run-command-other-frame)
(when (load "ibuffer" t) (global-set-key "\C-x\C-b" 'ibuffer))
(global-set-key "\331" 'sacha/yank-push) ;; M-Y
(global-set-key "\C-xQ" 'my-macro-query)

;; 2004.11.26 sindre #emacs
(setq icomplete-show-key-bindings t)

(global-set-key (kbd "<f5>") 'debug-on-entry)
(global-set-key (kbd "<f10>") 'debugger-step-through)
(global-set-key (kbd "<f11>") 'debugger-continue)

(global-set-key (kbd "\C-x ra") 'append-to-register)


;(global-set-key "\C-c\C-a" "\C-c\C- \C-n\M-w\C-y")


(global-set-key (kbd "C-*") 'evil-search-word-forward)
(global-set-key (kbd "C-#") 'evil-search-word-backward)
