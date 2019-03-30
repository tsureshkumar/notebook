;; Open the .pu extension file with plantuml-mode
(add-to-list 'auto-mode-alist '("\.pu$" . plantuml-mode))
;; Write your absolute plantuml.jar path 
;; (setq plantuml-jar-path "Here your plant.jar file path")
;; If you want to pass java options, write here.
(setq plantuml-java-options "")
;; Comment in here if you want to preview plantuml as svg, png
;; Display ASCII art by default
;;(setq plantuml-output-type "svg")
;; chart is utf-8
(setq plantuml-options "-charset UTF-8 ")


;; Execute plantuml-save-png function with C-c C-s at plantuml-mode
(add-hook 'plantuml-mode-hook
  (lambda () (local-set-key (kbd "C-c C-s") 'plantuml-save-png)))
 
;; If you want to save png file when saving .pu file, comment in here
;; (add-hook 'plantuml-mode-hook
;;    (lambda () (add-hook 'after-save-hook 'plantuml-save-png)))
 
;; Function to save plantuml as png
(defun plantuml-save-png ()
  (interactive)
  (when (buffer-modified-p)
    (map-y-or-n-p "Save this buffer before executing PlantUML?"
      'save-buffer (list (current-buffer))))
  (let ((code (buffer-string))
         out-file
         cmd)
    (when (string-match "^\\s-*@startuml\\s-+\\(\\S-+\\)\\s*$" code)
      (setq out-file (match-string 1 code)))
    (setq cmd (concat
                "java -Djava.awt.headless=true -jar " plantuml-java-options " "
                (shell-quote-argument plantuml-jar-path) " "
                (and out-file (concat "-t" (file-name-extension out-file))) " "
                plantuml-options " "
                (buffer-file-name)))
    (message cmd)
    (call-process-shell-command cmd nil 0)))
