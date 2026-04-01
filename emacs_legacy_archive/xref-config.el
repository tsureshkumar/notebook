(setq xrefdir "/usr/share/xref")
(setq exec-path (cons xrefdir exec-path))
(setq load-path (cons (concat xrefdir "/emacs") load-path))
(load "xrefactory")
