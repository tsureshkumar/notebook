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
(setq org-directory (concat my-vault-root "gtd/"))
(setq org-agenda-files
      (directory-files-recursively org-directory "\\.org$"))


(after! org
  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "PROGRESS(p)" "WAITING(w)" "|" "DONE(d)" "REJECTED(r)" "CANCELLED(c)")))

  (setq org-capture-templates
        `(("t" "Todo [inbox]" entry
           (file+headline ,(concat org-directory "0-Inbox/inbox.org") "Tasks")
           "* TODO %i%?")
          ("T" "Tickler" entry
           (file+headline ,(concat org-directory "GTD-System/tickler.org") "Tickler")
           "* %i%? \n %U")))

  (setq org-refile-targets
        `((,(concat org-directory "GTD-System/next-actions.org") :maxlevel . 3)
          (,(concat org-directory "GTD-System/someday-maybe.org") :level . 1)
          (,(concat org-directory "GTD-System/waiting-for.org") :maxlevel . 2)
          (,(concat org-directory "GTD-System/tickler.org") :maxlevel . 2)))

  (setq org-agenda-custom-commands
        `(("P" "Projects" tags "PROJECT")
          ("H" "Home & Office"
           ((agenda "" ((org-agenda-span 'day)))
            (tags-todo "OFFICE")
            (tags-todo "HOME")
            (tags-todo "COMPUTER")
            (tags-todo "READING")))
          ("D" "Daily Action List" agenda "" ((org-agenda-span 'day)))
          ("W" "Weekly Review & Plan" 
           ((agenda "" ((org-agenda-span 'week)))
            (tags ,(format-time-string "%B"))))
          ("M" "Monthly Review" 
           ((agenda "" ((org-agenda-span 'month)))
            (tags ,(format-time-string "%B"))))
          ("Q" "Quarterly Review" 
           ((agenda "" ((org-agenda-span 90)))
            (tags ,(format-time-string "%B"))))
          ("r" "Recently Added Projects"
           tags "PROJECT"
           ((org-agenda-overriding-header "Recently Added Projects")
            (org-agenda-sorting-strategy '(ts-down))))
          ("E" "Empty Projects (No Actions)"
           tags "PROJECT"
           ((org-agenda-overriding-header "Empty Projects (No active tasks in subtree)")
            (org-agenda-skip-function
             (lambda ()
               (let ((subtree-end (save-excursion (org-end-of-subtree t))))
                 (if (re-search-forward (concat "\\*\\s-+\\(" (mapconcat 'regexp-quote '("TODO" "NEXT" "PROGRESS" "WAITING") "\\|") "\\)") subtree-end t)
                     subtree-end
                   nil))))))
          ("S" "Stuck Projects (No NEXT)"
           tags "PROJECT"
           ((org-agenda-overriding-header "Stuck Projects (Missing NEXT action in subtree)")
            (org-agenda-skip-function
             (lambda ()
               (let ((subtree-end (save-excursion (org-end-of-subtree t))))
                 (if (re-search-forward "\\*\\s-+NEXT" subtree-end t)
                     subtree-end
                   nil))))))
          ("o" "At the office" tags-todo "@office"))))

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


(setq evil-esc-delay 0)
