(setq org-agenda-files '("~/my/notebook/org/"))

; use org mode from org mode melpa site
(require 'package)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)


(setq org-remember-templates
     '(
      ("Todo" ?t "* TODO %^{Brief Description} %^g\n%?\nAdded: %U" "~/my/notebook/org/Inbox.org" "Tasks")
      ("Private" ?p "\n* %^{topic} %T \n%i%?\n" "~/my/notebook/org/privnotes.org")
      ("WordofDay" ?w "\n* %^{topic} \n%i%?\n" "~/my/notebook/org/wotd.org")
      ))


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
    (find-file "~/my/notebook/org/Inbox.org")
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
