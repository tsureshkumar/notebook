;; Creates an after-init hook which resizes the frame to fill
;; vertically.

(defvar *default-fonts*
  (list 
   "-outline-Courier New-normal-r-normal-normal-16-120-96-96-c-*-iso8859-1"
   "-adobe-courier-medium-r-normal--12-120-75-75-m-70-iso8859-1"
   "-misc-fixed-medium-r-normal--13-120-75-75-c-70-iso8859-1"
   "fixed"))

(defvar *default-font* (first *default-fonts*))

(defun try-frame-fonts-and-select ()
  (dolist (f *default-fonts*)
    (when (ignore-errors (set-frame-font f) t)
      (setq *default-font* f)
      (return f))))


;; ----- from pjb (well, partially) -----
(defvar *window-manager-y-offset*
  (cond ((boundp 'aquamacs-version)
         (+ 24))
        ((or (eq 'gnu/linux system-type)
             (eq 'linux system-type)
             (and (boundp 'window-system)
                  (eq window-system 'x)))
         (+ 20 3))
        ((eq system-type 'windows-nt) 0)
        (t 0))
  "The number of vertical pixels eaten by the window manager
   (window title, grow bar).")

(defvar *bottom-y-offset*
  (cond ((eq system-type 'windows-nt)
         3)
        (t 0)))

(defun max-frame-line-number (&optional frame)
  "RETURN: The maximum number of line that can be displayed on this frame
        inside this screen."
  (- (truncate
      (/ (- (x-display-pixel-height frame) *window-manager-y-offset*)
         (frame-char-height frame)))
     *bottom-y-offset*))
;; ---------------

(defvar *default-column-size* 80)
(defvar *default-x-position*
  (cond ((eq system-type 'windows-nt)
         52)
        ((or (eq system-type 'gnu/linux)
             (eq system-type 'linux))
         52)))
(defvar *default-y-position* *window-manager-y-offset*)


(add-hook 'after-init-hook
          (lambda ()
            (when window-system
              (try-frame-fonts-and-select)
              (add-to-list 'default-frame-alist
                           (cons 'font *default-font*))
              (set-frame-position (selected-frame)
                                  *default-x-position*
                                  *default-y-position*)
              (set-frame-size (selected-frame)
                              *default-column-size*
                              (max-frame-line-number (selected-frame))))))

