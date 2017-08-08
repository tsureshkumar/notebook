(require 'ox-html)
(require 'ox-rss)
(setq org-export-html-coding-system 'utf-8-unix)
(setq org-html-viewport nil)

(setq my-blog-extra-head
      (concat
       "<link rel='stylesheet' href='/../res/code.css' />\n"
       "<link rel='stylesheet' href='/../res/main.css' />"))

(setq my-blog-header-file "~/public_html/blog/header.html")
(defun my-blog-header (arg)
  (with-temp-buffer
    (insert-file-contents my-blog-header-file)
    (buffer-string)))

(setq my-blog-footer
      "<hr />\n
<p><span style=\"float: left;\"><a href= \"/blog.xml\">RSS</a></span>
License: <a href= \"https://creativecommons.org/licenses/by-sa/4.0/\">CC BY-SA 4.0</a></p>\n
<p><a href= \"/contact.html\"> Contact</a></p>\n")


(defun my-blog-org-export-format-drawer (name content)
  (concat "<div class=\"drawer " (downcase name) "\">\n"
    "<h6>" (capitalize name) "</h6>\n"
    content
    "\n</div>"))

(setq my-blog-local-mathjax
      '((path "/res/mj/MathJax.js?config=TeX-AMS-MML_HTMLorMML")
        (scale "100") (align "center") (indent "2em") (tagside "right")
        (mathml nil)))

(defun my-blog-get-preview (file)
  "The comments in FILE have to be on their own lines, prefereably before and after paragraphs."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (let ((beg (+ 1 (re-search-forward "^#\\+BEGIN_PREVIEW$")))
          (end (progn (re-search-forward "^#\\+END_PREVIEW$")
                      (match-beginning 0))))
      (buffer-substring beg end))))

(defun my-blog-sitemap (project &optional sitemap-filename)
  "Generate the sitemap for my blog."
  (let* ((project-plist (cdr project))
         (dir (file-name-as-directory
               (plist-get project-plist :base-directory)))
         (localdir (file-name-directory dir))
         (exclude-regexp (plist-get project-plist :exclude))
         (files (nreverse
                 (org-publish-get-base-files project exclude-regexp)))
         (sitemap-filename (concat dir (or sitemap-filename "sitemap.org")))
         (sitemap-sans-extension
          (plist-get project-plist :sitemap-sans-extension))
         (visiting (find-buffer-visiting sitemap-filename))
         file sitemap-buffer)
    (with-current-buffer
        (let ((org-inhibit-startup t))
          (setq sitemap-buffer
                (or visiting (find-file sitemap-filename))))
      (erase-buffer)
      ;; loop through all of the files in the project
      (while (setq file (pop files))
        (let ((fn (file-name-nondirectory file))
              (link ;; changed this to fix links. see postprocessor.
               (file-relative-name file (file-name-as-directory
                                         (expand-file-name (concat (file-name-as-directory dir) "..")))))
              (oldlocal localdir))
          (when sitemap-sans-extension
            (setq link (file-name-sans-extension link)))
          ;; sitemap shouldn't list itself
          (unless (equal (file-truename sitemap-filename)
                         (file-truename file))
            (let (;; get the title and date of the current file
                  (title (org-publish-format-file-entry "%t" file project-plist))
                  (date (org-publish-format-file-entry "%d" file project-plist))
                  ;; get the preview section from the current file
                  (preview (my-blog-get-preview file))
                  (regexp "\\(.*\\)\\[\\([^][]+\\)\\]\\(.*\\)"))
              ;; insert a horizontal line before every post, kill the first one
              ;; before saving
              (insert "-----\n")
              (cond ((string-match-p regexp title)
                     (string-match regexp title)
                     ;; insert every post as headline
                     (insert (concat"* " (match-string 1 title)
                                    "[[file:" link "]["
                                    (match-string 2 title)
                                    "]]" (match-string 3 title) "\n")))
                    (t (insert (concat "* [[file:" link "][" title "]]\n"))))
              ;; add properties for `ox-rss.el' here
              (let ((rss-permalink (concat (file-name-sans-extension link) ".html"))
                    (rss-pubdate (format-time-string
                                  (car org-time-stamp-formats)
                                  (org-publish-find-date file))))
                (org-set-property "RSS_PERMALINK" rss-permalink)
                (org-set-property "PUBDATE" rss-pubdate))
              ;; insert the date, preview, & read more link
              (insert (concat date "\n\n"))
              (insert preview)
              (insert (concat "[[file:" link "][Read More...]]\n"))))))
      ;; kill the first hrule to make this look OK
      (goto-char (point-min))
      (let ((kill-whole-line t)) (kill-line))
      (save-buffer))
    (or visiting (kill-buffer sitemap-buffer))))

(setq my-blog-emacs-config-name "emacsconfig.org")
(setq my-blog-process-emacs-config t)

(defun my-blog-pages-preprocessor ()
  "Move a fresh version of the settings.org file to the pages directory."
  (when my-blog-process-emacs-config
    (let* ((cfg-file (expand-file-name (concat (file-name-as-directory user-emacs-directory)
                                               "settings.org")))
           (destdir (file-name-as-directory (plist-get project-plist :base-directory)))
           (cfg-file-dest (expand-file-name (concat destdir my-blog-emacs-config-name))))
      (copy-file cfg-file cfg-file-dest t))))

(defun my-blog-pages-postprocessor ()
  (message "In the pages postprocessor."))

(defun my-blog-articles-preprocessor ()
  (message "In the articles preprocessor."))

(defun my-blog-articles-postprocessor ()
  "Massage the sitemap file and move it up one directory.

for this to work, we have already fixed the creation of the
relative link in the sitemap-publish function"
  (let* ((sitemap-fn (concat (file-name-sans-extension (plist-get project-plist :sitemap-filename)) ".html"))
         (sitemap-olddir (plist-get project-plist :publishing-directory))
         (sitemap-newdir (expand-file-name (concat (file-name-as-directory sitemap-olddir) "..")))
         (sitemap-oldfile (expand-file-name sitemap-fn sitemap-olddir))
         (sitemap-newfile (expand-file-name (concat (file-name-as-directory sitemap-newdir) sitemap-fn))))
    (with-temp-buffer
      (goto-char (point-min))
      (insert-file-contents sitemap-oldfile)
      ;; massage the sitemap if wanted

      ;; delete the old file and write the correct one
      (delete-file sitemap-oldfile)
      (write-file sitemap-newfile))))

(defun my-blog-minify-css ()
  (let* ((csstidy "csstidy")
         (csstidy-args " --template=highest --silent=true")
         (css-dir (expand-file-name (plist-get project-plist :publishing-directory)))
         (css-files (directory-files css-dir t "^.*\\.css$")))
    (dolist (file css-files)
      (with-temp-buffer
        (insert (shell-command-to-string (concat csstidy " " file csstidy-args)))
        (write-file file)))))

(setq org-publish-project-alist
      `(("blog"
         :components ("blog-articles", "blog-pages", "blog-rss", "blog-res", "blog-images", "blog-dl"))
        ("blog-articles"
         :base-directory "~/public_html/blog/blog/"
         :base-extension "org"
         :publishing-directory "~/public_html/blog/www/blog/"
         :publishing-function org-html-publish-to-html
         ;:preparation-function my-blog-articles-preprocessor
         ;:completion-function my-blog-articles-postprocessor
         :htmlized-source t ;; this enables htmlize, which means that I can use css for code!

         :with-author t
         :with-creator nil
         :with-date t

         :headline-level 4
         :section-numbers nil
         :with-toc nil
         ;:with-drawers t
         :with-sub-superscript nil ;; important!!

         ;; the following removes extra headers from HTML output -- important!
         ;:html-link-home "/"
         ;:html-head nil ;; cleans up anything that would have been in there.
         ;;:html-head-extra ,my-blog-extra-head
         ;:html-head-include-default-style nil
         ;:html-head-include-scripts nil
         ;:html-viewport nil

         ;:html-format-drawer-function my-blog-org-export-format-drawer
         ;:html-home/up-format ""
         ;:html-mathjax-options ,my-blog-local-mathjax
         ;:html-mathjax-template "<script type=\"text/javascript\" src=\"%PATH\"></script>"
         ;:html-footnotes-section "<div id='footnotes'><!--%s-->%s</div>"
         ;:html-link-up ""
         ;:html-link-home ""
         ;:html-preamble my-blog-header
         ;:html-postamble ,my-blog-footer

         ;; sitemap - list of blog articles
         ;:auto-sitemap t
         ;:sitemap-filename "blog.org"
         ;:sitemap-title "Blog"
         ;; custom sitemap generator function
         ;:sitemap-function my-blog-sitemap
         ;:sitemap-sort-files anti-chronologically
         ;:sitemap-date-format "Published: %a %b %d %Y"
         )
        ("blog-pages"
         :base-directory "~/public_html/blog/pages/"
         :base-extension "org"
         :publishing-directory "~/public_html/blog/www/"
         :publishing-function org-html-publish-to-html
         :preparation-function my-blog-pages-preprocessor
         :completion-function my-blog-pages-postprocessor
         :htmlized-source t

         :with-author t
         :with-creator nil
         :with-date t

         :headline-level 4
         :section-numbers nil
         :with-toc nil
         :with-drawers t
         :with-sub-superscript nil ;; important!!
         :html-viewport nil ;; hasn't worked yet

         ;; the following removes extra headers from HTML output -- important!
         :html-link-home "/"
         :html-head nil ;; cleans up anything that would have been in there.
         :html-head-extra ,my-blog-extra-head
         :html-head-include-default-style nil
         :html-head-include-scripts nil

         :html-format-drawer-function my-blog-org-export-format-drawer
         :html-home/up-format ""
         :html-mathjax-options ,my-blog-local-mathjax
         :html-mathjax-template "<script type=\"text/javascript\" src=\"%PATH\"></script>"
         :html-footnotes-section "<div id='footnotes'><!--%s-->%s</div>"
         :html-link-up ""
         :html-link-home ""

         :html-preamble my-blog-header
         :html-postamble ,my-blog-footer)
        ("blog-rss"
         :base-directory "~/public_html/blog/blog/"
         :base-extension "org"
         :publishing-directory "~/public_html/blog/www/"
         :publishing-function org-rss-publish-to-rss

         :html-link-home "https://ogbe.net/"
         :html-link-use-abs-url t

         :title "Dennis Ogbe"
         :rss-image-url "https://ogbe.net/img/feed-icon-28x28.png"
         :section-numbers nil
         :exclude ".*"
         :include ("blog.org")
         :table-of-contents nil)
        ("blog-res"
         :base-directory "~/public_html/blog/res/"
         :base-extension ".*"
         :publishing-directory "~/public_html/blog/www/res/"
         :publishing-function org-publish-attachment
         :completion-function my-blog-minify-css)
        ("blog-images"
         :base-directory "~/public_html/blog/img/"
         :base-extension ".*"
         :publishing-directory "~/public_html/blog/www/img/"
         :publishing-function org-publish-attachment
         :recursive t)
        ("blog-dl"
         :base-directory "~/public_html/blog/dl/"
         :base-extension ".*"
         :publishing-directory "~/public_html/blog/www/dl/"
         :publishing-function org-publish-attachment
         :Recursive t)))

(add-to-list 'org-structure-template-alist
             '("b" "#+TITLE: ?
#+AUTHOR: Sureshkumar T
#+EMAIL: tsureshkumar@gmail.com
#+DATE:
#+STARTUP: showall
#+STARTUP: inlineimages
#+BEGIN_PREVIEW\n\n#+END_PREVIEW\n"))


