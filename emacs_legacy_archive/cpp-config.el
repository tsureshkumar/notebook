;(autoload 'cpp-mode "c++-mode" "Major mode for editing cpp code." t)
;(setq auto-mode-alist
;(append '(("\\.cpp$" . cpp-mode)) auto-mode-alist))


;; (defun auto-compile-cpp ()
;;   (save-window-excursion
;;     (async-shell-command (format "g++ -o %s %s" (car ( last ( split-string (first (split-string buffer-file-name "\\.")) "/"))) buffer-file-name))))

;; (defun cpp-custom ()
;;   "c++-mode-hook"
;;   (make-local-variable 'tab-width)
;;   (set 'tab-width 2)
;;   (add-hook 'after-save-hook 'auto-compile-cpp nil 'make-it-local)
;;   )

;; (add-hook 'c++-mode-hook 'cpp-custom)
;; ;(remove-hook 'c++-mode-hook 'cpp-custom)



(defun compile-on-save-start ()
  (let ((buffer (compilation-find-buffer)))
    ;  (unless (get-buffer-process buffer) 
    ;    (recompile))
    (recompile)
    )
  )

(define-minor-mode compile-on-save-mode
  "Minor mode to automatically call `recompile' whenever the
current buffer is saved. When there is ongoing compilation,
nothing happens."
  :lighter " CoS"
    (if compile-on-save-mode
    (progn  (make-local-variable 'after-save-hook)
        (add-hook 'after-save-hook 'compile-on-save-start nil t))
      (kill-local-variable 'after-save-hook)))

(defun cpp-custom ()
  (unless (or (file-exists-p "makefile")
              (file-exists-p "Makefile"))
    (set (make-local-variable 'compile-command)
         (concat "CPPFLAGS=\"-g -O0\" make -k "
                 (file-name-sans-extension buffer-file-name))))
  (set 'tab-width 4)
  )

(add-hook 'c++-mode-hook 'cpp-custom)
;(remove-hook 'c++-mode-hook 'cpp-custom)


