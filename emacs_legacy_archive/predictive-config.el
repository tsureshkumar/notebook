
;http://www.dr-qubit.org/emacs.php
(add-to-list 'load-path "/home/tsureshkumar/.emacs.d/predictive")
(add-to-list 'load-path "/home/tsureshkumar/.emacs.d/predictive/html")
(add-to-list 'load-path "/home/tsureshkumar/.emacs.d/predictive/latex")
(add-to-list 'load-path "/home/tsureshkumar/.emacs.d/predictive/texinfo")
(add-to-list 'load-path "/home/tsureshkumar/.emacs.d/predictive/misc")
(require 'predictive)


(autoload 'predictive-mode "predictive" "predictive" t)
(set-default 'predictive-auto-add-to-dict t)
(setq predictive-main-dict 'rpg-dictionary
      predictive-auto-learn t
      predictive-add-to-dict-ask nil
      predictive-use-auto-learn-cache nil
      predictive-which-dict t)
