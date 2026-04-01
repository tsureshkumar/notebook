(add-local-path "emacs/lisp/ruby-mode")
(autoload 'ruby-mode "ruby-mode"
  "Mode for editing ruby source files" t)

(add-to-list 'auto-mode-alist
             '("\\.rb$" . ruby-mode))  

(setq interpreter-mode-alist 
      (append '(("ruby" . ruby-mode)) 
              interpreter-mode-alist))

(autoload 'run-ruby "inf-ruby"
  "Run an inferior Ruby process")

(autoload 'inf-ruby-keys "inf-ruby"
  "Set local key defs for inf-ruby in ruby-mode")

(add-hook 'ruby-mode-hook
          '(lambda ()
             (inf-ruby-keys)
             (require 'ruby-electric)
             (ruby-electric-mode t)))

