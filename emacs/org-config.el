(setq org-agenda-files '("~/my/notebook-private/org/"))

; use org mode from org mode melpa site
(require 'package)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)


(setq org-agenda-exporter-settings
      '((ps-number-of-columns 1)
        (ps-landscape-mode t)
        (htmlize-output-type 'css)))

(setq org-agenda-custom-commands
'(

("P" "Projects"   
((tags "PROJECT")))

("H" "Office and Home Lists"
     ((agenda)
          (tags-todo "OFFICE")
          (tags-todo "HOME")
          (tags-todo "COMPUTER")
          (tags-todo "DVD")
          (tags-todo "READING")))

("D" "Daily Action List"
     (
          (agenda "" ((org-agenda-ndays 1)
                      (org-agenda-sorting-strategy
                       (quote ((agenda time-up priority-down tag-up) )))
                      (org-deadline-warning-days 0)
                      ))))
)
)

(defun gtd ()
    (interactive)
    (find-file "~/my/notebook-private/org/Inbox.org")
)
(global-set-key (kbd "C-c g") 'gtd)



;; (setq org-publish-project-alist
;;       '(("blog"
;;          :base-directory "~/"
;;          :html-extension "html"
;;          :base-extension "org"
;;          :publishing-directory "~/public_html/"
;;          :publishing-function (org-html-publish-to-html)
;;          :html-preamble nil
;;          :html-postamble nil)))


(setq org-publish-project-alist
      '(("blog-articles1"
         :base-directory "~/"
         :base-extension "org"
    
         :publishing-directory "public_html/blog/www/blog/"
         :publishing-function org-html-publish-to-html
         )))


(setq org-default-notes-file (concat my-root "/notebook-private/org/Inbox.org"))
(global-set-key (kbd "C-c C-c") 'org-capture)
(defvar gtd-home (concat my-root "notebook-private/org"))
(setq org-capture-templates '(("t" "Todo [inbox]" entry
                               (file+headline (concat gtd-home "/inbox.org") "Tasks")
                               "* TODO %i%?")
                              ("T" "Tickler" entry
                               (file+headline (concat gtd-home "/tickler.org") "Tickler")
                               "* %i%? \n %U")))
(setq org-refile-targets '(((concat gtd-home "/gtd.org") :maxlevel . 3)
                           ((concat gtd-home "/someday.org") :level . 1)
                           ((concat gtd-home "/tickler.org") :maxlevel . 2)))

(setq org-todo-keywords '((sequence "TODO(t)" "|" "WAITING(w)" "|" "DONE(d)" "|" "CANCELLED(c)")))

(setq org-agenda-custom-commands 
      '(("o" "At the office" tags-todo "@office"
         ((org-agenda-overriding-header "Office")
          (org-agenda-skip-function #'my-org-agenda-skip-all-siblings-but-first)))))

(defun my-org-agenda-skip-all-siblings-but-first ()
  "Skip all but the first non-done entry."
  (let (should-skip-entry)
    (unless (org-current-is-todo)
      (setq should-skip-entry t))
    (save-excursion
      (while (and (not should-skip-entry) (org-goto-sibling t))
        (when (org-current-is-todo)
          (setq should-skip-entry t))))
    (when should-skip-entry
      (or (outline-next-heading)
          (goto-char (point-max))))))
		  
(defun org-current-is-todo ()
  (string= "TODO" (org-get-todo-state)))
