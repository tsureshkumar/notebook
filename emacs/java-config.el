(add-hook 'java-mode-hook (function cscope:hook))

(require 'javadoc-lookup)
(global-set-key (kbd "C-h j") 'javadoc-lookup)

