;(add-to-list 'load-path "/usr/src/cvs/bbdb/lisp")
;(add-to-list 'load-path "/home/tsureshkumar/notebook/emacs/lisp/tar/bbdb-2.35/lisp")
;(add-to-list 'load-path "/usr/share/emacs/site-lisp/bbdb/")
;(add-to-list 'load-path "/usr/share/emacs21/site-lisp/bbdb/")

(require 'bbdb)
;(require 'bbdb-autoloads)
(require 'bbdb-com)

;(load "bbdb-com" t)
(bbdb-initialize 'gnus 'message)
(bbdb-mua-auto-update-init 'message) ;; use 'gnus for incoming messages too
(setq bbdb-mua-auto-update-p 'query) ;; or 'create to create without asking
;; What do we do when invoking bbdb interactively
(setq bbdb-mua-update-interactive-p '(query . create))
;; Make sure we look at every address in a message and not only the
;; first one
(setq bbdb-message-all-addresses t)
;; use ; on a message to invoke bbdb interactively
(add-hook
 'gnus-summary-mode-hook
 (lambda ()
    (define-key gnus-summary-mode-map (kbd ";") 'bbdb-mua-edit-field)))
;; (bbdb-insinuate-sc)

;; I want to see the BBDB buffer only when I ask for it.
(setq bbdb-use-pop-up nil)

;; Sometimes people have work and personal e-mail addresses.
(setq bbdb-complete-name-allow-cycling t)

;; Ignore subnets
(setq bbdb-canonicalize-redundant-nets-p t)

;; Always save
(setq bbdb-offer-save 1)

;; My screen is not that wide, so I need information displayed on multiple
;; lines
(setq bbdb-display-layout 'multi-line)

;; The window should be as small as possible, though.
(setq bbdb-pop-up-target-lines 1)

;; Who uses north American numbers, anyway? ;)
(setq bbdb-north-american-phone-numbers-p nil)

;; I love my Gnus
(add-hook 'gnus-startup-hook 'bbdb-insinuate-gnus)

(put 'subjects 'field-separator "\n")

(defun sacha/bbdb-canonicalize-net-hook (addr)
  "Do not notice member@orkut.com or noreplyaddresses."
  (cond ((null addr) addr)
        ((string-match "member@orkut\\.com" addr) nil)
        ((string-match "noreply" addr) nil)
        ((string-match "NO-REPLY" addr) nil)
        (t addr)))
(setq bbdb-canonicalize-net-hook 'sacha/bbdb-canonicalize-net-hook)

(setq bbdb-auto-notes-alist
      (quote (("Organization" (".*" company 0 nil))
	      ("To"
	       ("w3o" . "w3o")
	       ("plug" . "plug")
	       ("linux" . "linux")
	       ("emacs-commit" . "emacs commit")
	       ("emacs" . "emacs")
               ("emacs-wiki-discuss" . "planner")
	       ("pinoyjug" . "pinoyjug")
	       ("digitalfilipino" . "digitalfilipino")
	       ("sacha" . "personal mail")
	       ("handhelds" . "handhelds")
	       ("debian-edu" . "debian-edu")
	       ("sigcse" . "sigcse")
	       ("debian" . "debian"))
	      ("Cc"
	       ("w3o" . "w3o")
	       ("plug" . "plug")
	       ("linux" . "linux")
	       ("emacs-commit" . "emacs commit")
	       ("emacs" . "emacs")
	       ("sigcse" . "sigcse")
	       ("pinoyjug" . "pinoyjug")
	       ("digitalfilipino" . "digitalfilipino")
	       ("sacha" . "personal mail")
	       ("debian-edu" . "debian-edu")
	       ("debian" . "debian")
	       ("handhelds" . "handhelds"))
	      ("From"
	       ("admu" company "Ateneo de Manila University")
               ("orkut" . "orkut")))))

(setq bbdb-auto-notes-ignore '((("Organization" . "^Gatewayed from\\\\|^Source only")
                                ("Path" . "main\\.gmane\\.org")
                                ("From" . "NO-REPLY"))))
(setq bbdb-auto-notes-ignore-all nil)
(setq bbdb-check-zip-codes-p nil)
(setq bbdb-default-area-code 632)
(setq bbdb-default-country "Philippines")
(setq bbdb-ignore-most-messages-alist (quote (("To" . "sacha") ("Cc" . "sacha") ("To" "emacs-wiki-discuss"))))
(add-hook 'bbdb-notice-hook 'bbdb-auto-notes-hook)
(setq bbdb-quiet-about-name-mismatches 0)
(setq bbdb/mail-auto-create-p 'bbdb-ignore-most-messages)
(setq bbdb/news-auto-create-p nil)
(put 'notes 'field-separator "; ")

(add-to-list 'bbdb-auto-notes-alist
             (list "x-face"
                   (list (concat "[ \t\n]*\\([^ \t\n]*\\)"
                                 "\\([ \t\n]+\\([^ \t\n]+\\)\\)?"
                                 "\\([ \t\n]+\\([^ \t\n]+\\)\\)?"
                                 "\\([ \t\n]+\\([^ \t\n]+\\)\\)?")
                         'face
                         "\\1\\3\\5\\7")))

(defun sacha/bbdb-create-factoid (title &optional text)
  "Store a factoid named TITLE with associated TEXT into my BBDB.
If PREFIX is non-nil, get TEXT from the buffer instead."
  (interactive (list (read-string "Title: ") (read-string "Text:")))
  (unless text
    (setq text (read-string "Text: ")))
  (bbdb-create-internal title "factoid" nil nil nil text))

;; Stolen from http://www.esperi.demon.co.uk/nix/xemacs/personal/dot-gnus-bbdb.html
(defadvice bbdb/gnus-update-records (around nix-bbdb-use-summary-buffer-news-auto-create-p activate preactivate)
  "Propagate the value of news-auto-create-p from the Summary buffer.
This allows it to be buffer-local there, so that we can have different values of
this variable in different simultaneously active Summary buffers."
  (let ((bbdb/news-auto-create-p
         (with-current-buffer gnus-summary-buffer
           bbdb/news-auto-create-p))
        (bbdb/mail-auto-create-p
         (with-current-buffer gnus-summary-buffer
           bbdb/mail-auto-create-p)))
     ad-do-it))

(defun sacha/bbdb-should-not-truncate ()
  "Do not truncate lines in BBDB buffers."
  (setq truncate-lines nil))
(add-hook 'bbdb-list-hook 'sacha/bbdb-should-not-truncate)

(defalias 'bbdb-vcard-export-record-insert-vcard 'sacha/bbdb-vcard-export-record-insert-vcard)
(defun sacha/bbdb-vcard-export-record-insert-vcard (record)
  "Insert a vcard formatted version of RECORD into the current buffer"
  (let ((name (bbdb-record-name record))
	(first-name (bbdb-record-firstname record))
	(last-name (bbdb-record-lastname record))
	(aka (bbdb-record-aka record))
	(company (bbdb-record-company record))
	(notes (bbdb-record-notes record))
	(phones (bbdb-record-phones record))
	(addresses (bbdb-record-addresses record))
        (blog (bbdb-record-getprop record 'blog))
        (web (bbdb-record-getprop record 'web))
	(net (bbdb-record-net record))
	(categories (bbdb-record-getprop
		     record
		     bbdb-define-all-aliases-field)))
    (insert "begin:vcard\n"
	    "version:3.0\n")
    ;; Specify the formatted text corresponding to the name of the
    ;; object the vCard represents.  The property MUST be present in
    ;; the vCard object.
    (insert "fn:" (bbdb-vcard-export-escape name) "\n")
    ;; Family Name, Given Name, Additional Names, Honorific
    ;; Prefixes, and Honorific Suffixes
    (when (or last-name first-name)
      (insert "n:"
	      (bbdb-vcard-export-escape last-name) ";"  
	      (bbdb-vcard-export-escape first-name) ";;;\n"))
    ;; Nickname of the object the vCard represents.  One or more text
    ;; values separated by a COMMA character (ASCII decimal 44).
    (when aka
      (insert "nickname:" (bbdb-vcard-export-several aka) "\n"))
    ;; FIXME: use face attribute for this one.
    ;; PHOTO;ENCODING=b;TYPE=JPEG:MIICajCCAdOgAwIBAgICBEUwDQYJKoZIhvcN
    ;; AQEEBQAwdzELMAkGA1UEBhMCVVMxLDAqBgNVBAoTI05ldHNjYXBlIENvbW11bm
    ;; ljYXRpb25zIENvcnBvcmF0aW9uMRwwGgYDVQQLExNJbmZvcm1hdGlvbiBTeXN0

    ;; FIXME: use birthday attribute if there is one.
    ;; BDAY:1996-04-15
    ;; BDAY:1953-10-15T23:10:00Z
    ;; BDAY:1987-09-27T08:30:00-06:00

    ;; A single structured text value consisting of components
    ;; separated the SEMI-COLON character (ASCII decimal 59).  But
    ;; BBDB doesn't use this.  So there's just one level:
    (when company
      (insert "org:" (bbdb-vcard-export-escape company) "\n"))
    (when blog
      (insert "URL:" blog "\n"))
    (when web
      (insert "URL:" web "\n"))
    (when notes
      (insert "note:" (bbdb-vcard-export-escape notes) "\n"))
    (dolist (phone phones)
      (insert "tel;type=" (bbdb-vcard-export-escape (bbdb-phone-location phone)) ":"
	      (bbdb-vcard-export-escape (bbdb-phone-string phone)) "\n"))
    (dolist (address addresses)
      (insert (bbdb-vcard-export-address-string address) "\n"))
    (dolist (mail net)
      (insert "email;type=internet:" (bbdb-vcard-export-escape mail) "\n"))
    ;; Use CATEGORIES based on mail-alias.  One or more text values
    ;; separated by a COMMA character (ASCII decimal 44).
    (when categories
      (insert "categories:" 
	      (bbdb-join (mapcar 'bbdb-vcard-export-escape
				 (bbdb-split categories ",")) ",") "\n"))
    (insert "end:vcard\n")))

(defun sacha/bbdb-records-postal ()
  (sort
   (delq nil
         (mapcar
          (lambda (item)
            (and (car (bbdb-record-addresses item)) item))
          (bbdb-records)))
   (lambda (a b)
     (string< (bbdb-address-country (car (bbdb-record-addresses a)))
              (bbdb-address-country (car (bbdb-record-addresses b)))))))

;(setq sacha/contact-list (sacha/bbdb-records-postal))

;  (bbdb-display-records sacha/contact-list)

;; FIXME: This is a workaround for a new Emacs bug
(bbdb-records)
          
(provide 'bbdb-config)

(defun bbdb-canonicalize-address (net)
  (if (functionp bbdb-canonicalize-net-hook)
      (setq bbdb-canonicalize-net-hook (list bbdb-canonicalize-net-hook)))

  (while (not (equal net
		     (dolist (hook bbdb-canonicalize-net-hook net)
		       (setq net (funcall hook net))))))
  net)


(defsubst bbdb-search-simple (name net)
  "name is a string; net is a string or list of strings."
  (if (eq 0 (length name)) (setq name nil))
  (if (eq 0 (length net)) (setq net nil))
  (bbdb-records) ; make sure db is parsed
  (or (and name (bbdb-gethash (downcase name)))
      (and net
	   (if (stringp net)
	       (bbdb-gethash (downcase net))
	     (let ((answer nil))
	       (while (and net (null answer))
		 (setq answer (bbdb-gethash (downcase (car net)))
		       net (cdr net)))
	       answer)))))

(add-hook 'mail-setup-hook 'bbdb-mail-aliases)
(add-hook 'message-setup-hook 'bbdb-mail-aliases)
