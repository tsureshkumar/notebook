;; Sets up exec-path-from shell
;; https://github.com/purcell/exec-path-from-shell
(when (memq window-system '(mac ns))
  (load-library "exec-path-from-shell")
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-envs
   '("PATH")))
