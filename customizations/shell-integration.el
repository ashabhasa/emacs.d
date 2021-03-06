;; Sets up exec-path-from shell
;; https://github.com/purcell/exec-path-from-shell
(when (memq window-system '(mac ns x))
  (setq-default exec-path-from-shell-arguments nil)
  (exec-path-from-shell-initialize))

(exec-path-from-shell-copy-envs
 '("PATH"
   "AUTH0_ISSUER"
   "AUTH0_CLIENT_ID"
   "AUTH0_CLIENT_SECRET"))
