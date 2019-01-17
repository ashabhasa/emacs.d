;; haskell
(require 'haskell-mode)
(require 'hindent)
(require 'haskell-interactive-mode)
(require 'haskell-process)
(require 'company)


; Make Emacs look in Stack directory for binaries
(let ((my-path (expand-file-name "/usr/local/bin")))
  (setenv "PATH" (concat my-path ":" (getenv "PATH")))
(add-to-list 'exec-path my-path))

(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(add-hook 'haskell-mode-hook #'hindent-mode)
(add-hook 'haskell-mode-hook
          (lambda ()
            (show-paren-mode)
            (smartparens-mode)
            (yas-minor-mode)
            (git-gutter-mode)
            (company-mode +1)
            (global-flycheck-mode +1)
            (set (make-local-variable 'company-backends)
                 (append '((company-capf company-dabbrev-code))
                         company-backends))
            )
)

;; enable minor mode which activates keybindings associated with interactive mode
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)

(add-to-list 'company-backends 'company-ghc)

(eval-after-load 'haskell-mode
  '(define-key haskell-mode-map (kbd "C-c C-o") 'haskell-compile))

;; Functions

(defun haskell-insert-doc ()
  "Insert the documentation syntax."
  (interactive)
  (unless (= (line-beginning-position)
             (line-end-position))
    (shm/backward-paragraph))
  (unless (= (line-beginning-position)
             (line-end-position))
    (save-excursion (insert "\n")))
  (insert "-- | "))

(defun haskell-insert-undefined ()
  "Insert undefined."
  (interactive)
  (if (and (boundp 'structured-haskell-mode)
           structured-haskell-mode)
      (shm-insert-string "undefined")
    (insert "undefined")))

(defun haskell-move-right ()
  (interactive)
  (haskell-move-nested 1))

(defun haskell-move-left ()
  (interactive)
(haskell-move-nested -1))


(define-key haskell-mode-map (kbd "C-c i")       'hindent-reformat-decl)
(define-key haskell-mode-map (kbd "C-c f b")     'hindent-reformat-buffer)
(define-key haskell-mode-map [f8]                'haskell-navigate-imports)
(define-key haskell-mode-map (kbd "C-c C-u")     'haskell-insert-undefined)
(define-key haskell-mode-map (kbd "C-c C-a")     'haskell-insert-doc)
(define-key haskell-mode-map (kbd "C-s-<right>") 'haskell-move-right)
(define-key haskell-mode-map (kbd "C-s-<left>")  'haskell-move-left)

(define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-or-reload)
(define-key haskell-mode-map (kbd "C-`")     'haskell-interactive-bring)
(define-key haskell-mode-map (kbd "C-c C-t") 'haskell-process-do-type)
(define-key haskell-mode-map (kbd "C-c C-i") 'haskell-process-do-info)
(define-key haskell-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
(define-key haskell-mode-map (kbd "C-c C-k") 'haskell-interactive-mode-clear)

(custom-set-variables
 '(company-ghc-show-info t)
 '(haskell-process-type 'stack-ghci
                        '(haskell-process-type 'chosen-process-type))
 '(haskell-process-args-ghci '())
 '(haskell-notify-p t)
 '(haskell-stylish-on-save nil)
 '(haskell-tags-on-save t)
 '(haskell-process-suggest-remove-import-lines t)
 '(haskell-process-auto-import-loaded-modules t)
 '(haskell-process-log t)
 '(haskell-process-reload-with-fbytecode nil)
 '(haskell-process-use-presentation-mode t)
 '(haskell-interactive-mode-include-file-name nil)
 '(haskell-interactive-mode-eval-pretty nil)
 '(shm-use-hdevtools t)
 '(shm-use-presentation-mode t)
 '(shm-auto-insert-skeletons t)
 '(shm-auto-insert-bangs t)
 '(haskell-process-show-debug-tips nil)
 '(haskell-process-suggest-hoogle-imports nil)
 '(haskell-process-suggest-haskell-docs-imports t)
 '(hindent-style "gibiansky"))


