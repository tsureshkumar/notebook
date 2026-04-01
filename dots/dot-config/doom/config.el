;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; PERSONAL INFO
(setq user-full-name "Suresh Kumar"
      user-mail-address "sureshkumar@example.com")

;; PATHS
(defvar my-vault-root "~/vault/my/notebook/")

;; FONTS & THEME
(setq doom-font (font-spec :family "Source Code Pro" :size 13 :weight 'normal)
      doom-variable-pitch-font (font-spec :family "Source Code Pro" :size 13))
(setq doom-theme 'doom-one)

;; UI
(setq display-line-numbers-type 'relative)
(setq doom-modeline-height 25)

;; EVIL CONFIG
(setq evil-escape-key-sequence "jk")
(setq evil-escape-delay 0.1)

;; Match Neovim splits
(map! :nv "ss" #'evil-window-split
      :nv "sv" #'evil-window-vsplit)

;; Center on search
(map! :n "n" (λ! (evil-ex-search-next) (evil-scroll-line-to-center nil))
      :n "N" (λ! (evil-ex-search-previous) (evil-scroll-line-to-center nil)))

;; ORG MODE
(setq org-directory "~/org/")
(setq org-agenda-files (list org-directory (concat my-vault-root "gtd/")))

(after! org
  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "PROGRESS(p)" "WAITING(w)" "|" "DONE(d)" "REJECTED(r)" "CANCELLED(c)")))

  (setq org-capture-templates
        `(("t" "Todo [inbox]" entry
           (file+headline ,(concat org-directory "refile.org") "Tasks")
           "* TODO %i%?")
          ("T" "Tickler" entry
           (file+headline ,(concat my-vault-root "org/tickler.org") "Tickler")
           "* %i%? \n %U")))

  (setq org-refile-targets
        `((,(concat org-directory "gtd.org") :maxlevel . 3)
          (,(concat org-directory "someday.org") :level . 1)
          (,(concat my-vault-root "org/tickler.org") :maxlevel . 2))))

;; LANGUAGE SPECIFIC
(add-hook 'd2-mode-hook (lambda () (setq-local d2-ascii-preview t)))

;; TREEMACS
(setq treemacs-width 35)

;; MACOS
(when (featurep :system 'macos)
  (setq mac-command-modifier 'meta
        mac-option-modifier 'alt))

;; GLOBAL KEYBINDINGS
(map! :leader
      :desc "D2: Live Browser Preview" "d" (λ! (shell-command (format "d2 --watch %s" (buffer-file-name))))
      :desc "Clear Search Highlights" "<space>" #'evil-ex-nohighlight)

;; CUSTOM FUNCTIONS
(defun insert-date ()
  "Insert date at point."
  (interactive)
  (insert (format-time-string "%A, %B %e, %Y %k:%M:%S %z")))
