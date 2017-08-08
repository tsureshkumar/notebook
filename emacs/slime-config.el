;; these 3 lines are the basic requirements:
;;(add-to-list 'load-path/ (concat emacs-root "emacs/slime/"))
(require 'slime)
(slime-setup)

;; enable paredit in slime repl
(add-hook 'slime-repl-mode-hook (lambda () (paredit-mode +1)))


;; SLIME defaults to starting an executable named "lisp" in the
;; current path.  You can define other lisps here:
(when (or (eq system-type 'gnu/linux)
          (eq system-type 'linux))
  (add-to-list 'slime-lisp-implementations
               '(sbcl ("/usr/local/bin/sbcl")))
  ;(add-to-list 'slime-lisp-implementations
  ;             '(clisp ("/usr/local/bin/clisp")))
  ;; default meaning, you don't have to explicitly request it
  ;; by invoking: C-u M-x slime <name>
  ;; a mere M-x slime will suffice.
  (setq slime-default-lisp 'sbcl))

;; personal preference
(setq slime-conservative-indentation nil)

;; I don't know if this is really necessary, it's just to deal
;; with old files that include mode-lines with explicit package
;; settings.
(add-hook 'hack-local-variables-hook
          (lambda () 
	    (kill-local-variable 'package)))


;; the hackery needed to get Allegro for Windows to play nice with
;; SLIME:  the basic problem is that SLIME cannot instruct ACL/Win how
;; to work because there is no standard console.  I created a shell
;; script (also a BAT file if you don't use MinGW/Cygwin) which
;; starts ACL with the appropriate commands so that SLIME can connect.
;; You need to fetch that separately and put it in C:/lisp/

;; Note that you should change the RunAllegro.sh to RunAllegro.BAT
;; if you like running Emacs from the Windows Desktop or Command Shell.
;; I prefer to use MinGW shell so it understands .sh

;; Side note: "normal" Lisps like CLISP or the future SBCL for Windows
;; should work through the normal mechanism above.
(when (eq system-type 'windows-nt)
  (defun start-slime ()
    (interactive)
    (shell-command (format "c:/lisp/RunAllegro.sh %s &"
                           (slime-swank-port-file))
                   "*lisp-async-process*")
    (delete-other-windows)
    (slime-read-port-and-connect (get-buffer-process "*lisp-async-process*")
                                 nil))

  ;; Using this hack you start SLIME by using M-x start-slime
  ;; This also sets F5 to start slime.
  (global-set-key
   [(f5)]
   'start-slime)

  ;; I like using "g" to "goto cross reference" sometimes, because
  ;; "space" gets intercepted on Windows occasionally for some unknown
  ;; reason.
  (define-key slime-xref-mode-map "g" 'slime-goto-xref))




;; I'm not currently using these but if you wanted, you could rebind [
;; and ] to s-exp manipulation commands using these commands.

;; Same as M-(
; (define-key slime-mode-map [(?\[)] 'insert-parentheses)

;; Same as M-)
; (define-key slime-mode-map [(?\])] 'move-past-close-and-reindent)

;; Auto-indent when you hit RETURN
; (define-key slime-mode-map [(return)] 'newline-and-indent)
