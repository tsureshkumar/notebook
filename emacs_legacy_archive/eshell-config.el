;; From http://www.emacswiki.org/cgi-bin/wiki.pl?EshellFunctions

(require 'eshell)

(setenv "JAVA_HOME" "/usr/local/java")
(setenv "CATALINA_HOME" "/var/lib/tomcat4")
(setenv "EDITOR" "emacsclient")
(setenv "LC_ALL" "C")
(setenv "DEBEMAIL" "Sandra Jean Chua (Sacha) <sacha@free.net.ph>")
(setenv "CVSROOT" "sachachua@savannah.nongnu.org:/cvsroot/emacs-wiki")
(setenv "PATH" "/usr/local/java/bin:/usr/local/sbin:/usr/sbin:/sbin:/usr/local/bin:/usr/bin:/bin:/home/sacha/bin")
(setenv "LANG" "en")
(setenv "EDITOR" "emacsclient")

(add-hook 'eshell-mode-hook '(lambda ()
                               (local-set-key (kbd "C-a") 'eshell-bol-maybe-my)
                               (local-set-key (kbd "M-_") 'kai-eshell-insert-last-word)))

;;;###autoload
(defun bol-maybe-general-my (prompt &optional alt-bol-fcn)
  ""
  (interactive)
  (if (and (string-match (concat "^" (regexp-quote prompt)
				 " *$")
			 (buffer-substring-no-properties
			  (line-beginning-position)
			  (point)))
	   (not (bolp)))
      (beginning-of-line)
    (if alt-bol-fcn
	(funcall alt-bol-fcn)
      (beginning-of-line)
      (search-forward-regexp prompt))))

;;;###autoload
(defun eshell-bol-maybe-my ()
  ""
  (interactive)
  (bol-maybe-general-my (funcall eshell-prompt-function)))



(defun eshell/emacs (&rest args)
  "Open a file in emacs. Some habits die hard."
  (if (null args)
      ;; If I just ran "emacs", I probably expect to be launching
      ;; Emacs, which is rather silly since I'm already in Emacs.
      ;; So just pretend to do what I ask.
      (bury-buffer)
    ;; We have to expand the file names or else weird stuff happens
    ;; when you try to open a bunch of different files in wildly
    ;; different places in the filesystem.
    (mapc #'find-file (mapcar #'expand-file-name args))))

(defun eshell/ec (&rest args)
  "Use `compile' to do background makes."
  (if (eshell-interactive-output-p)
      (let ((compilation-process-setup-function
	     (list 'lambda nil
		   (list 'setq 'process-environment
			 (list 'quote (eshell-copy-environment))))))
	(compile (eshell-flatten-and-stringify args))
        (pop-to-buffer compilation-last-buffer))
    (throw 'eshell-replace-command
           (let ((l (eshell-stringify-list (eshell-flatten-list args))))
             (eshell-parse-command (car l) (cdr l))))))
(put 'eshell/ec 'eshell-no-numeric-conversions t)


(defun eshell/vi (&rest args)
  "Invoke `find-file' on the file.	
\"vi +42 foo\" also goes to line 42 in the buffer."
  (while args
    (if (string-match "\\`\\+\\([0-9]+\\)\\'" (car args))
        (let* ((line (string-to-number (match-string 1 (pop args))))
               (file (pop args)))
          (find-file file)
          (goto-line line))
      (find-file (pop args)))))


(defun eshell/perldoc (&rest args)
  "Like `eshell/man', but invoke `perldoc'."
  (funcall 'perldoc (apply 'eshell-flatten-and-stringify args)))


(defun perldoc (man-args)
  (interactive "sPerldoc: ")
  (require 'man)
  (let ((manual-program "perldoc"))
    (man man-args)))


(defun eshell/ec (&rest args)
  "Use `compile' to do background makes."
  (if (eshell-interactive-output-p)
      (let ((compilation-process-setup-function
             (list 'lambda nil
                   (list 'setq 'process-environment
                         (list 'quote (eshell-copy-environment))))))
        (compile (eshell-flatten-and-stringify args))
        (pop-to-buffer compilation-last-buffer))
    (throw 'eshell-replace-command
           (let ((l (eshell-stringify-list (eshell-flatten-list args))))
             (eshell-parse-command (car l) (cdr l))))))
(put 'eshell/ec 'eshell-no-numeric-conversions t)

;; From kai.grossjohann@uni-duisburg.de
(defun kai-eshell-insert-last-word (n)
  (interactive "p")
  (insert (car (reverse
                (split-string
                 (eshell-previous-input-string (- n 1)))))))

;; Stolen from http://www.emacswiki.org/cgi-bin/wiki.pl/EshellEnhancedLS
(eval-after-load "em-ls"
  '(progn
     (defun ted-eshell-ls-find-file-at-point (point)
       "RET on Eshell's `ls' output to open files."
       (interactive "d")
       (find-file (buffer-substring-no-properties
                   (previous-single-property-change point 'help-echo)
                   (next-single-property-change point 'help-echo))))

     (defun pat-eshell-ls-find-file-at-mouse-click (event)
       "Middle click on Eshell's `ls' output to open files.
 From Patrick Anderson via the wiki."
       (interactive "e")
       (ted-eshell-ls-find-file-at-point (posn-point (event-end event))))

     (let ((map (make-sparse-keymap)))
       (define-key map (kbd "RET")      'ted-eshell-ls-find-file-at-point)
       (define-key map (kbd "<return>") 'ted-eshell-ls-find-file-at-point)
       (define-key map (kbd "<mouse-2>") 'pat-eshell-ls-find-file-at-mouse-click)
       (defvar ted-eshell-ls-keymap map))

     (defadvice eshell-ls-decorated-name (after ted-electrify-ls activate)
       "Eshell's `ls' now lets you click or RET on file names to open them."
       (add-text-properties 0 (length ad-return-value)
                            (list 'help-echo "RET, mouse-2: visit this file"
                                  'mouse-face 'highlight
                                  'keymap ted-eshell-ls-keymap)
                            ad-return-value)
       ad-return-value)))
(eshell)