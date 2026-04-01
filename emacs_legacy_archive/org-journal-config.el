(require 'org-journal)

(defvar office-journal "~/my/notebook-private/office/journal/")
(defvar private-journal "~/my/notebook-private/private/journal/")

(setq org-journal-dir office-journal)

(defun suresh/switch-journal-to-office ()
  (interactive)
  (setq org-journal-dir office-journal))

(defun suresh/switch-journal-to-private ()
  (interactive)
  (setq org-journal-dir private-journal))
