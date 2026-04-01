(add-local-path (concat emacs-root "emacs/site-lisp/fsharp/"))
;;(add-to-list 'load-path/ (concat emacs-root "emacs/site-lisp/fsharp/"))
(setq auto-mode-alist (cons '("\\.fs[iylx]?$" . fsharp-mode) auto-mode-alist))
(autoload 'fsharp-mode "fsharp" "Major mode for editing F# code." t)
(autoload 'run-fsharp "inf-fsharp" "Run an inferior F# process." t)


(setq inferior-fsharp-program "mono /opt/FSharp-2.0.0.0/bin/fsi.exe --readline-")
(setq fsharp-compiler "mono /opt/FSharp-2.0.0.0/bin/fsc.exe")
