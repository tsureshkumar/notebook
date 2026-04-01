;;(add-to-list 'load-path/ "/usr/src/cvs/erc")
;;(add-to-list 'load-path/ (concat suresh-cvs-root "erc"))

(defun suresh/erc-start ()
  "Connect to Freenode and Undernet."
  (interactive)
;  (suresh/erc-undernet)
  (suresh/erc-freenode)
; (suresh/erc-freenet)
;  (suresh/erc-bitlbee)
;  (suresh/erc-perl)
  (suresh/erc-gimpnet)
)

(defvar suresh/erc-tunnel-flag nil "*Non-nil means use tunnel")

(defun suresh/erc-undernet () "Connect to Undernet."
  (interactive)
  (if suresh/erc-tunnel-flag
      (erc "localhost" 26667 "suresh00" "Sureshkumar T" t)
    (erc "eu.undernet.org" 6666 "suresh00" "Sureshkumar T" t)))

(defun suresh/erc-freenode () "Connect to Freenode."
  (interactive)
  (if suresh/erc-tunnel-flag
      (erc "localhost" 16667 "suresh00" "Sureshkumar T" t)
    (erc "irc.freenode.net" 6666 "suresh00" "Sureshkumar T" t)))

(defun suresh/erc-freenet () "Connect to Freenet."
  (interactive)
  (if suresh/erc-tunnel-flag
      (erc "localhost" 36666 "suresh00" "Sureshkumar T" t)
  (erc "irc.free.net.ph" 6666 "suresh00" "Sureshkumar T" t)))


(defun suresh/erc-bitlbee () "Connect to Bitlbee."
  (interactive)
  (if suresh/erc-tunnel-flag
      (erc "localhost" 46667 "suresh00" "Sureshkumar T" t)   
    (erc "testing.bitlbee.org" 6666 "suresh00" "Sureshkumar T" t)))

(defun suresh/erc-perl () "Connect to irc.perl.org."
  (interactive)
  (erc "irc.perl.org" 6666 "suresh00" "Sureshkumar T" t))
  ;(erc "localhost" 56667 "suresh00" "Sureshkumar T" t)) 

(defun suresh/erc-gimpnet () "Connect to gimpnet."
  (interactive)
  (if suresh/erc-tunnel-flag
      (erc "localhost" 66667 "suresh00" "Sureshkumar T" t)
    (erc "irc.gimp.org" 6666 "suresh00" "Sureshkumar T" t)))

(defun suresh/erc-susenet () "Connect to gimpnet."
  (interactive)
  (if suresh/erc-tunnel-flag
      (erc "localhost" 76667 "suresh00" "Sureshkumar T" t)
    (erc "irc.suse.de" 6667 "suresh00" "Sureshkumar T" t)))


(setq erc-modules '(fill track completion ring button autojoin noncommands irccontrols))
(require 'erc)

(setq erc-auto-query t
      erc-bbdb-auto-create-on-whois-p nil
      erc-fill-column (- (window-width) 2)
      erc-keywords '("suresh")
      erc-pals '("harinath")
      erc-notify-list erc-pals)
(require 'erc-dcc)
(require 'erc-bbdb)
(suresh/home-vs-office
 (require 'erc-nickserv)
 (require 'erc-services) ;; from cvs, above has been renamed
)
     
(defun suresh/erc-setup-mode ()
  (erc-button-mode 1)
  (erc-bbdb-mode 1)
  ;;  (erc-track-mode 1)
  (erc-match-mode 1)
  (erc-pcomplete-mode 1)
  (erc-ring-mode 1)
  (erc-services-mode 1)
                                        ;(flyspell-mode 1)
  (define-key erc-mode-map [(control a)] 'erc-bol)
  (define-key erc-mode-map "\M-/" 'hippie-expand)
  (define-key erc-mode-map "\C-c\C-r" 'erc-reply))
     
(add-hook 'erc-mode-hook 'suresh/erc-setup-mode)
     
(when (fboundp 'erc-pcomplete-ordering-PRIVMSG-handler)
  (add-hook 'erc-server-PRIVMSG-hook 'erc-pcomplete-ordering-PRIVMSG-handler))
     
(setq erc-prompt
      (lambda ()
        (erc-propertize (concat
                         (if (erc-channel-p (buffer-name (current-buffer)))
                             (buffer-name (current-buffer))
                           "ERC")
                         ">")
                        'read-only t 'rear-nonsticky t 'front-nonsticky t)))
     
;; From http://www.emacswiki.org/cgi-bin/wiki.pl?BitlBee
(add-hook 'erc-after-connect 'suresh/identify)
(defun suresh/identify (server nick)
  "Send the identify command to the #bitlbee channel."
  (mapcar
   (lambda (item)
     (when (string-match (car item) server)
       (erc-message "PRIVMSG" (cdr item))))
   suresh/secret/erc-identification))
     
;; From http://www.emacswiki.org/cgi-bin/wiki.pl?BitlBee
(defun erc-cmd-ICQWHOIS (uin)
  "Query icq-user with UIN and returns the result."
  (let* ((result (myerc-query-icq-user uin))
         (fname (cdr (assoc 'fname result)))
         (lname (cdr (assoc 'lname result)))
         (nick (cdr (assoc 'nick result))))
    (erc-display-message nil 'notice (current-buffer)
                         (format "%s (%s %s)" nick fname lname))))
     
(defun erc-cmd-ICQPAGE (uin)
  "Display the ICQ webpage of UIN."
  (let* ((result (myerc-query-icq-user uin))
         (fname (cdr (assoc 'fname result)))
         (lname (cdr (assoc 'lname result)))
         (nick (cdr (assoc 'nick result))))
    (w3m (concat "http://web.icq.com/" uin))))

(defun erc-autojoin-channels (server nick)
  (dolist (l erc-autojoin-channels-alist)
    (when (string-match (car l) server)
      (dolist (chan (cdr l))
        (erc-send-command (concat "join " chan))))


    (add-hook 'erc-after-connect 'erc-autojoin-channels)))

(setq erc-away-nickname "suresh00_away")
;; (setq erc-bbdb-auto-create-on-join-p nil)
;; (setq erc-bbdb-auto-create-on-nick-p nil)
(setq erc-bbdb-auto-create-on-whois-p nil)
;; (setq erc-bbdb-popup-type nil)
(setq erc-common-server-suffixes (quote (("freenode.net$" . "Freenode") ("undernet.org$" . "Undernet"))))
(setq erc-echo-timestamps t)
(setq erc-email-userid "tsureshkumar")
(setq erc-hide-timestamps nil)
(setq erc-insert-post-hook (quote (erc-track-modified-channels)))
(setq erc-kill-buffer-on-part t)
(setq erc-kill-queries-on-quit t)
(setq erc-log-insert-log-on-open nil)
(setq erc-log-channels t)
(setq erc-auto-reconnect nil)
(setq erc-log-channels-directory "~/notebook/logs/erc")
(setq erc-minibuffer-notice nil)
(setq erc-prompt-for-nickserv-password nil)
(setq erc-nick "suresh00")
(setq erc-port "6666")
(setq erc-prompt-for-password nil)
(setq erc-reuse-buffers nil)
(setq erc-save-buffer-on-part nil)
(setq erc-save-queries-on-quit nil)
(setq erc-send-pre-hook (quote (erc-add-to-input-ring erc-send-distinguish-noncommands)))
(setq erc-server "eu.undernet.org")
(setq erc-user-full-name "Sureshkumar T")
(setq erc-server-history-list
      '("eu.undernet.org" "irc.freenode.net" "tlc.irc.free.net.ph" "localhost"))
(setq erc-keywords (quote ("suresh" "planner" "emacs-wiki")))
(setq erc-track-faces-priority-list
      '(erc-error-face erc-current-nick-face erc-keyword-face erc-pal-face
                       erc-nick-msg-face erc-direct-msg-face))
(setq erc-track-priority-faces-only '("#perl" "#linuxhelp" "#debian-devel" "#debian" "#ruby-lang"
                                      "#debian-women" "#nihongo"))
      
(defun erc-reply ()
  (interactive)
  (save-excursion
    (let ((nick (elt (erc-parse-user (elt (get-text-property (line-beginning-position) 'erc-parsed) 1)) 0)))
      (when nick
        (end-of-buffer)
        (erc-send-message (concat nick ": " (read-string (concat nick ": "))))))))

(defun erc-cmd-WALLCHOPS (line)
  "Send a message to the operators of the current channel."
  (erc-send-command (concat "WALLCHOPS "
                            (if (erc-default-target)
                                (concat (erc-default-target) ":" line)
                              line)))
  t)
(put 'erc-cmd-WALLCHOPS 'do-not-parse-args t)

(add-to-list 'erc-nickserv-alist
             '(freenet
               "NickServ!NickServ@services.irc.free.net.ph"
               "/msg\\s-NickServ@services.irc.free.net.ph\\s-IDENTIFY\\s-<password>"
               "NickServ@services.irc.free.net.ph"
               "IDENTIFY"
               nil))

;; 2003.11.21: I want to be able to BBDB from an erc-nick button.
;; (add-to-list 'erc-nick-popup-alist '("BBDB" . (bbdb nick nil)))

;; http://www.emacswiki.org/cgi-bin/wiki/ErcDoctor

(autoload 'doctor-doc "doctor")
(autoload 'make-doctor-variables "doctor")

(defun erc-cmd-DOCTOR (&rest ignore)
  "Get the last message in the channel and doctor it."
  (let ((limit (- (point) 500))
        (pos (point))
        doctor-buffer
        last-message
        last-sender
        text)
    ;; Make sure limit is not negative
    (when (< limit 0) (setq limit 0))
    ;; Search backwards for text from someone
    (while (and pos (not (let ((data (get-text-property
                                      pos 'erc-parsed)))
                           (and data (string=
                                      (aref data 3) "PRIVMSG")))))
      (setq pos (previous-single-property-change
                 pos 'erc-parsed nil limit)))
    (when pos
      (setq last-sender (car (split-string
                              (aref (get-text-property
                                     pos 'erc-parsed) 2) "!"))
            doctor-buffer (concat "*ERC Doctor: " last-sender "*")
            last-message (split-string
                          ;; Remove punctuation from end of sentence
                          (replace-regexp-in-string
                           "[ .?!;,/]+$" ""
                           (aref (get-text-property pos
                                                    'erc-parsed) 5)))
            text (mapcar (lambda (s)
                           (intern (downcase s)))
                         ;; Remove salutation if it exists
                         (if (string-match
                              (concat "^" erc-valid-nick-regexp
                                      "[:,]*$\\|[:,]+$")
                              (car last-message))
                             (cdr last-message)
                           last-message))))
    (erc-send-message
     (concat 
      ;; Only display sender if not in a query buffer
      (if (not (erc-query-buffer-p))
          (concat last-sender ": "))
      (save-excursion
        (if (get-buffer doctor-buffer)
            (set-buffer doctor-buffer)
          (set-buffer (get-buffer-create doctor-buffer))
          (make-doctor-variables))
        (erase-buffer)
        (doctor-doc text)
        (buffer-string))))))

(defcustom erc-autojoin-channels-alist nil
  "List of channels to autojoin on IRC networks.
This should be a list (entry entry entry) where each entry is of
the form (server-substring . '(chana chanb)).")

(setq erc-autojoin-channels-alist
      '((".freenode.net$" . ("#emacs" "#debian-devel" "#gnus"))
        (".gimp.org$" . ("#mono" "#mono-winforms" "#monodev" "#gnome-hackers"))
        (".undernet.org$" . ("#linuxhelp"))
        (".free.net.ph$" . ("#plug"))
        (".perl.org$" . ("#perl"))))
                                        ;(setq erc-default-coding-system '(iso-2022-jp . iso-2022-jp))
                                        ;(setq erc-encoding-coding-alist
                                        ;      (quote (("#lisp" . utf-8)
                                        ;              ("." . iso-2022-jp))))
;;          ("#truelambda" . iso-latin-1)
;;          ("#bitlbee" . iso-latin-1))))



