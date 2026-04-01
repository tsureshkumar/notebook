;(add-to-list 'load-path "~/notebook/emacs/site-lisp/cedet-1.0beta3b/common")
;(add-to-list 'load-path "~/notebook/emacs/site-lisp/cedet-1.0beta3b/semantic")
;(add-to-list 'load-path "~/notebook/emacs/site-lisp/cedet-1.0beta3b/eieio")
;(add-to-list 'load-path "~/notebook/emacs/site-lisp/cedet-1.0beta3b/speedbar")
;;(add-to-list 'load-path/ "~/notebook/emacs/site-lisp/cedet-1.1/")
;;(add-to-list 'load-path/ "~/notebook/emacs/site-lisp/cedet-1.1/semantic")
;(add-to-list 'load-path "~/notebook/emacs/site-lisp/cedet-1.1/eieio")
;(add-to-list 'load-path "~/notebook/emacs/site-lisp/elib-1.0")

;;;_+ semantic stuff
(require 'cedet)
(require 'eieio)
(setq semantic-load-turn-everything-on t)
;(require 'semantic-load)

;; i dislike annoying modes
;(global-semantic-show-dirty-mode -1)
;(global-semantic-show-unmatched-syntax-mode -1)

; I like the current function name at the top of file
;(semantic-load-enable-excessive-code-helpers)

;;(add-to-list 'load-path/ "~/notebook/emacs/site-lisp/ecb-2.40")

(require 'xcscope)

;;;_+ semantic stuff
(require 'cedet)
(setq semantic-load-turn-everything-on t)
;(require 'semantic-load)

;; i dislike annoying modes
;(global-semantic-show-dirty-mode -1)
;(global-semantic-show-unmatched-syntax-mode -1)

; I like the current function name at the top of file
;(semantic-load-enable-excessive-code-helpers)


;;;_- semantic stuff end

;;;_+ ecb
;;(require 'ecb-autoloads)
(require 'ecb)

;(ecb-add-source-path "~/quick/southpaw/work/novellvpn" "novellvpn" t)
;(ecb-add-source-path "~/quick/southpaw/work/turnpike" "turnpike" t)

;(semantic-stickyfunc-mode t)

;;;_- ecb end
