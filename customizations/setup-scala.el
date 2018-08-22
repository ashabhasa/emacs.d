;; scala
;;(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)
;; For complex scala files
(setq max-lisp-eval-depth 50000)
(setq max-specpdl-size 5000)

;; enable prettify
;;(setq prettify-symbols-alist scala-prettify-symbols-alist)
;;(prettify-symbols-mode)

;; Simple buffer only completion bypassing company mode and server
(bind-key "C-<tab>" 'company-or-dabbrev-complete)

(add-hook 'scala-mode-hook
          (lambda ()
            (show-paren-mode)
            (smartparens-mode)
            (yas-minor-mode)
            (git-gutter-mode)
            (company-mode)
            (ensime-mode)
            (scala-mode:goto-start-of-code)))

