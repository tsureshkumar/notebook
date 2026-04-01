(autoload 'ruby-mode "ruby-mode" "Load ruby-mode")
(add-to-list 'auto-mode-alist '("\\.rb\\'" . ruby-mode))
;; uncomment the next line if you want syntax highlighting
(add-hook 'ruby-mode-hook 'turn-on-font-lock)

;;This is also of interrest, it automagically does a "chmod u+x" when
;;you save a script file (starting with "#!"). Works with every kind
;;of script, not only ruby ones. Just add that into .emacs
(add-hook 'after-save-hook
          '(lambda ()
             (progn
               (and (save-excursion
                      (save-restriction
                        (widen)
                        (goto-char (point-min))
                        (save-match-data
                          (looking-at "^#!"))))
                    (shell-command (concat "chmod u+x " buffer-file-name))
                    (message (concat "Saved as script: " buffer-file-name))
                    ))))

