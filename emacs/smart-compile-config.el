(require 'smart-compile)

(global-set-key "\C-x\C-x" 'smart-compile)
(add-to-list 'smart-compile-alist
             `(("\\.cl$" . "sbcl --noinform %f")
               ("\\.scm$" . "gosh %f")
               ("\\.clj$" . "clj %f")
               ("\\.pir$" . "parrot %f")
               ("\\.\(pl\|pm\)$" . "perl %f")
                                        ;("app\\.js$" . ,(expand-file-name "~/Library/Application\\ Support/Titanium/mobilesdk/osx/1.8.0.1/iphone/builder.py run %F"))
               ("\\.js$" . ,(expand-file-name "~/Library/Application\\ Support/Titanium/mobilesdk/osx/1.8.0.1/iphone/builder.py run %F")
                )
                (".*Test\\.cpp$" . "echo hehe")
               ))

(add-to-list 'smart-compile-alist
             `(c-mode  . "gcc -O0 -g -Wall -o %n %f")
             )

;; (add-to-list 'smart-compile-alist
;;              `((c++-mode  . "gcc -O0 -g -Wall -o %n %f")))

(add-to-list 'smart-compile-alist
             '(c++-mode . "(mkdir build || :) && LIBRARY_PATH=/usr/local/lib g++ -std=c++11 -O2 -g -DDEBUG=1 -Wall -pedantic -Werror -Wreturn-type %f -lm -lgmpxx -lgmp -o ./build/%n && ((test -f ./%n.input && timeout 5 time ./build/%n < %n.input | tee ./build/%n.output && echo -------------------------------------------------- ) || :)")
             )


(add-to-list 'smart-compile-alist
             '(".*\\.[Cc]+[Pp]*$" . "(mkdir build || :) && LIBRARY_PATH=/usr/local/lib  g++ -std=c++11 -O2 -g -DDEBUG=1 -Wall -pedantic -Werror -Wreturn-type %f -lm -lgmpxx -lgmp -o ./build/%n && ((test -f ./%n.input && timeout 5 time ./build/%n < %n.input | tee ./build/%n.output && echo --------------------------------------------------) || :)")
             )


;; ;; (defun c++-compile-custom ()
;; ;;   "c++-mode-hook"
;; ;;   (add-hook 'after-save-hook (lambda () (smart-compile)))
;; ;;   )

;; ;; (add-hook 'c++-mode-hook 'c++-compile-custom)

;; (defun compile-on-save-start ()
;;   (let ((buffer (compilation-find-buffer)))
;;     (unless (get-buffer-process buffer) 
;;       (recompile))))

;; (define-minor-mode compile-on-save-mode
;;   "Minor mode to automatically call `recompile' whenever the
;; current buffer is saved. When there is ongoing compilation,
;; nothing happens."
;;   :lighter " CoS"
;;   (if compile-on-save-mode
;;       (progn  (make-local-variable 'after-save-hook)
;;               (add-hook 'after-save-hook (lambda ()  (smart-compile 4))))
;;     (kill-local-variable 'after-save-hook)))


(setq smart-run-alist
      '(("\\.c$"          . "./%n")
        ("\\.[Cc]+[Pp]*$" . "./%n")
        ("\\.java$"       . "java %n")
        ("\\.php$"        . "php %f")
        ("\\.m$"          . "%f")
        ("\\.scm"         . "%f")
        ("\\.tex$"        . "dvisvga %n.dvi")
        ("\\.py$"         . "python %f")
        ("\\.pl$"         . "perl \"%f\"")
        ("\\.pm$"         . "perl \"%f\"")
        ("\\.bat$"        . "%f")
        ("\\.mp$"         . "mpost %f")
        ("\\.sh$"         . "./%f")))

(add-to-list 'smart-compile-alist
             `("\\.java$"  . "javac %f && java %n")
             )



(setq smart-executable-alist
      '("%n.class"
        "%n.exe"
        "%n"
        "%n.mp"
        "%n.m"
        "%n.php"
        "%n.py"
        "%n.pl"
        "%n.pm"
        "%n.bat"
        "%n.sh"))
