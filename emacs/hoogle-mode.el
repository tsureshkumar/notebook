(require 'thingatpt)

(defun hoogle (arg)
  "Query hoogle via the shell with ARG."
  (interactive (list (let ((sym (thing-at-point 'symbol)))
                       (read-string (if sym
                                        (format "Hoogle query (default: %s): " sym)
                                      "Hoogle query: ")
                                    nil nil (or sym "")))))
  (let ((b (get-buffer-create "*Hoogle output*")))
    (shell-command (concat "hoogle " arg) b)
    (with-current-buffer b 
      (toggle-read-only)
      (set-buffer-modified-p nil))))

(eval-after-load 'haskell-mode
  '(add-hook 'haskell-mode-hook
             (lambda ()
               (local-set-key (kbd "C-c h") 'hoogle))))

