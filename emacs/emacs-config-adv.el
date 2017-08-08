(defmacro sacha/gnu-vs-x (a b)
  "Execute A under GNU Emacs and B under XEmacs."
  `(if (featurep 'xemacs)
       ,b
     ,a))

;; Page-up after a Page-down returns to the same position.
(sacha/gnu-vs-x
 (setq scroll-preserve-screen-position t)
 (require 'scroll-in-place))

(defun run-command-other-frame (command)
  "Run COMMAND in a new frame."
  (interactive "CC-x 5 M-x ")
  (select-frame (make-frame))
  (call-interactively command))

;; I like having flashing parentheses
(sacha/gnu-vs-x
 (show-paren-mode t)
 (progn
   (require 'paren)
   (paren-set-mode 'paren)))

;; XEmacs defaults seem to work
(when (fboundp 'auto-image-file-mode) (auto-image-file-mode t))

(defun sacha/compile-adjust-variable ()                                         
  (unless (file-exists-p "Makefile")                                            
    (set (make-local-variable 'compile-command)                                 
         (let ((file (file-name-nondirectory buffer-file-name)))                
           (concat "gcc -O2 -Wall -o " (file-name-sans-extension file)          
                   " " file)))))                                                
                                                                                
(eval-after-load 'compile (lambda ()                                            
			(add-hook 'c-mode-hook                              
				'sacha/compile-adjust-variable)))

;;;_+ diary
(add-hook 'diary-display-hook 'fancy-diary-display)
(setq mark-diary-entries-in-calendar t)
(setq diary-file "~/notebook/personal/diary")
;;;_- diary

(sacha/gnu-vs-x
 (progn (add-hook 'suspend-hook 'resume-suspend-hook)
        (add-hook 'suspend-resume-hook 'resume-process-args))
 (require 'resume))

(sacha/gnu-vs-x
 (global-font-lock-mode 1)
 (setq font-lock-auto-fontify t))

(sacha/gnu-vs-x
 (when (load "ido" t)
   (ido-mode 'buffers))
 (progn (require 'iswitchb)
        (iswitchb-default-keybindings)))

(setq message-sendmail-f-is-evil t)

(unless (featurep 'xemacs) (setq sgml-font-lock-keywords-2 nil))

(setq browse-url-browser-function 'w3m-browse-url)
(setq sentence-end-double-space nil)
(setq sentence-end "[.?!][]\"')]*\\($\\|\t\\| \\)[ \t\n]*")

(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-plug-into-AUCTeX t)

(autoload 'typing-of-emacs "typing" "The Typing Of Emacs, a game." t)

(defun suresh/sha1-region (start end)
  (interactive "r")
  (save-excursion
    (let ((s (sha1 (buffer-substring start end))))
      (delete-region start end)
      (insert s))))

(defun suresh/md5-region (start end)
  (interactive "r")
  (save-excursion
    (let ((s (md5 (buffer-substring start end))))
      (delete-region start end)
      (insert s))))


(setq-default local-write-file-hooks nil)

(unless (featurep 'xemacs)
  (prefer-coding-system 'utf-8))

;; 2003.11.27: I usually work with one frame, so I want the filename
;; all the time
(setq frame-title-format "%b")

;; 2004.02.16: I hate XLS files
(add-to-list 'auto-mode-alist '("\\.xls\\'" . no-xls))
(defun no-xls (&optional filename)
  "Run xlhtml and w3m -dump on the entire buffer.
    Optional FILENAME says what filename to use.
    This is only necessary for buffers without
    proper `buffer-file-name'.  FILENAME should
    be a real filename, not a path."
  (interactive "fExcel File: ")
  (when (and filename
             (not (buffer-file-name)))
    (write-file (make-temp-file filename)))
  (erase-buffer)
  (shell-command
   (format "xlhtml -nc -te %s | w3m -dump -T text/html" (buffer-file-name))
   (current-buffer))
  (setq buffer-file-name nil)
  (set-buffer-modified-p nil))

(add-hook 'c-mode-hook (lambda () (c-set-style "ellemtel")))
;;(start-remem)
;;(load "/usr/share/emacs/site-lisp/debian-startup.el")
;;(debian-startup 'emacs-snapshot)

;; For macro prompting, from http://www.emacswiki.org/cgi-bin/wiki?action=browse;diff=1;id=KeyboardMacros
(defun my-macro-query (arg)
  "Prompt for input using minibuffer during kbd macro execution.
With prefix argument, allows you to select what prompt string to use."
  (interactive "P")
  (let* ((query (lambda () (kbd-macro-query t)))
         (prompt (if arg
                     (read-from-minibuffer "PROMPT: ")
                   "Input: "))
         (input (unwind-protect
                    (progn
                      (add-hook 'minibuffer-setup-hook query)
                      (read-from-minibuffer prompt))
                  (remove-hook 'minibuffer-setup-hook query))))
    (unless (string= "" input) (insert input))))

(defvar sacha/screenshot-file "~/notebook/pics/screen.png"
  "Filename for screenshots.")

(defun sacha/take-screenshot ()
  (interactive)
  (shell-command (concat "import -window root " sacha/screenshot-file)))

(defun sacha/display-screenshot ()
  (interactive)
  (call-process "display" nil 0 sacha/screenshot-file))

(defun sacha/yank-push () (interactive) (yank-pop -1))

(autoload 'sacha/list-web-stats "website-config" nil t)
(put 'erase-buffer 'disabled nil)
;(load-library "/etc/emacs/site-start.d/55prime-el.el")
;(setq default-input-method "japanese-prime")

;; 2004.11.14
(defun sacha/ffap-quick ()
  "Quickly open URL at point in background."
  (interactive)
  (save-window-excursion
    (save-excursion
      (ffap (ffap-guesser)))))

(setq minibuffer-prompt-properties '(face minibuffer-prompt))

(defun sacha/filter-irclog ()
  (interactive)
  (delete-matching-lines "....... \\(>>>\\|<<<\\|---\\|<--\\|-->\\)"))

(defun sacha/irclog-markup ()
  (interactive)
  (let ((emacs-wiki-publishing-header "")
        (emacs-wiki-publishing-footer ""))
    (emacs-wiki-mode)
    (emacs-wiki-replace-markup)
    (fundamental-mode)))

;; 2005.02.27
;; From http://www.emacswiki.org/cgi-bin/wiki/PopularOptions
(setq enable-recursive-minibuffers t)

(require 'appt)
(sacha/gnu-vs-x (display-time-mode 1) (display-time))
;(sacha/gnu-vs-x (appt-activate 1) (appt-initialize))


;;;_+ 2005.03.24: Random taglines

(defun sacha/random-tagline ()
  "Return a random tagline and put it in the kill ring."
  (interactive)
  (with-current-buffer (find-file-noselect "~/.taglines")
    (goto-char (random (point-max)))
    (buffer-substring (line-beginning-position)
                      (line-end-position))))

;; grep -h -w -E 'cats?' ~/notebook/japan/examples* > ~/.taglines 
(eval-after-load 'remember
  '(progn
     (defadvice remember (after sacha-tagline activate)
       "Add random tagline."
       (save-excursion
         (goto-char (point-max))
         (insert "\n\nRandom Japanese sentence: " (sacha/random-tagline) "\n\n")))))

(when (featurep 'xemacs)
  (setq interprogram-cut-function nil
        interprogram-paste-function nil))

;;;_+ Backup configuration

(setq backup-by-copying t)
(setq backup-directory-alist  '(("." . "~/.saves")))
(setq delete-old-versions t)
(setq kept-new-versions 6)
(setq kept-old-versions 2)
(setq version-control t)			

;;;_+ Recentf

;(require 'recentf)
;(recentf-mode 1)
;(global-set-key (kbd "C-M-r") 'recentf-open-files)

;;;_+ Canada

(defun sacha/canada-days ()
  "Display how many days I've been in Canada."
  (interactive)
  (let ((days (- (time-to-days (current-time)) (time-to-days (encode-time 0 0 19 15 7 2005)))))
    (prog1 days
      (when (interactive-p)
        (message "%d days so far" days)
        (kill-new (format "%d days so far" days))))))
(setq undo-limit 50000)


;;;_+ timeclock

(setq timeclock-workday (* (* 8 60) 60))
;;;_- timeclock

;; useful function to insert date at point
;; Useful function:
;; Insert date into buffer
(defun insert-date ()
"Insert date at point."
(interactive)
(insert (format-time-string "%A, %B %e, %Y %k:%M:%S %z")))


                                        ; ido configuration
;; Define he following variables to remove the compile-log warnings
;; when defining ido-ubiquitous
(defvar ido-cur-item nil)
(defvar ido-default-item nil)
(defvar ido-cur-list nil)
(defvar predicate nil)
(defvar inherit-input-method nil)
