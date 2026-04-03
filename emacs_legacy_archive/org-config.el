(setq org-directory "~/vault/my/notebook/gtd/")
(setq org-agenda-files (directory-files-recursively org-directory "\\.org$"))

(setq org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "PROGRESS(p)" "WAITING(w)" "|" "DONE(d)" "REJECTED(r)" "CANCELLED(c)")))


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


(setq org-default-notes-file (concat org-directory "0-Inbox/inbox.org"))
(global-set-key (kbd "C-c C-c") 'org-capture)

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
