;; scala
;;(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)
;; For complex scala files
(setq max-lisp-eval-depth 50000)
(setq max-specpdl-size 5000)

(require 'smartparens-config)
;; enable prettify
;;(setq prettify-symbols-alist scala-prettify-symbols-alist)
;;(prettify-symbols-mode)

;;specific ensime backend for expand-region
;;(require 'ensime-expand-region)

;; Simple buffer only completion bypassing company mode and server
(bind-key "C-<tab>" 'company-or-dabbrev-complete)


(require 'ensime)

(add-hook 'scala-mode-hook
          (lambda ()
            (show-paren-mode)
            (smartparens-mode)
            (yas-minor-mode)
            (git-gutter-mode)
            (company-mode)
            (ensime-mode)
            (scala-mode:goto-start-of-code)))

;; Add some whitespace padding depending on what you
;; type after `(` or `{` parenthesis
(sp-local-pair 'scala-mode "(" nil :post-handlers '(("||\n[i]" "RET")))
(sp-local-pair 'scala-mode "{" nil :post-handlers '(("||\n[i]" "RET") ("| " "SPC")))

;; restrict the smartparens navigation commands
;; to just the brackets (ignoring Emacs’ s-expression
;; interpretation of the Scala language as provided by scala-mode)
(defun sp-restrict-c (sym)
  "Smartparens restriction on `SYM' for C-derived parenthesis."
  (sp-restrict-to-pairs-interactive "{([" sym))

(bind-key "s-<delete>"    (sp-restrict-c 'sp-kill-sexp)          smartparens-mode-map)
(bind-key "s-<backspace>" (sp-restrict-c 'sp-backward-kill-sexp) smartparens-mode-map)
(bind-key "s-<home>"      (sp-restrict-c 'sp-beginning-of-sexp)  smartparens-mode-map)
(bind-key "s-<end>"       (sp-restrict-c 'sp-end-of-sexp)        smartparens-mode-map)

