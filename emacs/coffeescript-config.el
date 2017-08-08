(autoload 'coffee-mode "coffee-mode" "Major mode for editing coffeescript code." t)
(setq auto-mode-alist
      (append '(("\\.coffee$" . coffee-mode)) auto-mode-alist))

(setq coffee 'iced)

(defun auto-compile-coffee ()
  (save-window-excursion
    (async-shell-command (format "%s %s" coffee buffer-file-name))))

(defun coffee-custom ()
  "coffee-mode-hook"
  (make-local-variable 'tab-width)
  (set 'tab-width 2)
  (add-hook 'after-save-hook 'auto-compile-coffee nil 'make-it-local)
  )

(add-hook 'coffee-mode-hook 'coffee-custom)



;;;; copied from stack overflow


;; (defun auto-compile-latex ()
;;   (save-window-excursion
;;     (async-shell-command (format "cd %s; scons -u" default-directory))))
;; (add-hook 'LaTeX-mode-hook '(lambda ()
;;      (add-hook 'after-save-hook 'auto-compile-latex nil 'make-it-local)))

;; (defun run-coffeescript ()
;;   "Asynchronously run `coffee' and attach a sentinel to it"
;;   (let ((process (start-process "coffee" "*output*"
;;                                 "coffee" buffer-file-name)))
;;     ))

;; (defun latexmk-sentinel (p e)
;;   "Display the pdf if `latexmk' was successful"
;;   (when (= 0 (process-exit-status p))
;;     (start-process "displaypdf" "*output*"
;;                    "/bin/echo" "DISPLAY PDF")))

;; ;; Example use
;; (with-current-buffer (get-buffer-create "*output*") (erase-buffer))
;; (run-latexmk)

