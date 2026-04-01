(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

(defvar prelude-packages
  '(
    edit-server
    ack-and-a-half
    scala-mode
    yasnippet
    haskell-mode
    auctex
    clojure-mode
    coffee-mode deft
    expand-region
    gist
    groovy-mode
    haml-mode
    haskell-mode
    inf-ruby
    markdown-mode
    paredit
    projectile
    python
    sass-mode
    rainbow-mode
    scss-mode
    solarized-theme
    volatile-highlights
    yaml-mode
    yari
    zenburn-theme
    java-snippets
    javaimp
    javap-mode
    ido-ubiquitous
    thread-dump
    web-beautify
    autodisass-java-bytecode
    bbdb
    ecb
    muse
    ;; color-theme
    color-theme-modern
    xcscope
    smex
    org-journal
    org-capture-pop-frame


    clojure-mode
    clojure-mode-extra-font-locking
    cider
    clomacs
    clj-refactor

    smart-compile

    bash-completion
    
    dash
    magit
    s ; string manipulation library
    popup
    helm
    async
    web-mode
    js2-mode
    multiple-cursors
    ido-vertical-mode
    paradox

    cyberpunk-theme
    )
  "A list of packages to ensure are installed at launch.")

(defun prelude-packages-installed-p ()
  (loop for p in prelude-packages
        when (not (package-installed-p p)) do (return nil)
        finally (return t)))

(unless (prelude-packages-installed-p)
  ;; check for new packages (package versions)
  (message "%s" "Emacs Prelude is now refreshing its package database...")
  (package-refresh-contents)
  (message "%s" " done.")
  ;; install the missing packages
  (dolist (p prelude-packages)
    (when (not (package-installed-p p))
      (package-install p))))


(provide 'prelude-packages)


;; (unless (package-installed-p 'yasnippet)
;;   (package-refresh-contents)) (package-install 'yasnippet)
