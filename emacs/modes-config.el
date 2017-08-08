
(add-to-list 'auto-mode-alist '("\\.rdf$" . sgml-mode))

(add-to-list 'auto-mode-alist '("\\.diff$" . unidiff-mode))
(add-to-list 'auto-mode-alist '("\\.php" . php-mode))
(autoload 'php-mode "php-mode" nil t)

(add-to-list 'auto-mode-alist '("\\.\\([pP][Llm]\\|al\\)\\'" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("perl" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("perl5" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("miniperl" . cperl-mode))

(autoload 'ruby-mode "ruby-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))

(autoload 'csharp-mode "chsarp-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.cs$'" . csharp-mode))

;; C style mode

;; use a modified Ellemtel style for all C like languages
(eval-after-load 'cc-mode
  '(progn
     (c-set-offset (quote substatement-open) 0 nil)))

;; c mode defaults
;; (defun linux-c-mode ()
;;   "C mode with adjusted defaults for use with the Linux kernel."
;;   (interactive)
;;   (c-mode)
;;   (setq c-indent-level 8)
;;   (setq c-brace-imaginary-offset 0)
;;   (setq c-brace-offset -8)
;;   (setq c-argdecl-indent 8)
;;   (setq c-label-offset -8)
;;   (setq c-continued-statement-offset 8)
;;   (setq indent-tabs-mode t)
;;   (setq tab-width 8))

;(eval-after-load 'c-mode
;  '(progn
;     (linux-c-mode)))

(add-hook 'c-mode-common-hook
          (lambda ()
            (c-set-style "gnu")))

;(require 'twiki)
;(add-to-list 'auto-mode-alist '("\\.twiki$" . twiki-mode))
