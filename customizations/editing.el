;; Customizations relating to editing a buffer.

;; Key binding to use "hippie expand" for text autocompletion
;; http://www.emacswiki.org/emacs/HippieExpand
(global-set-key (kbd "M-/") 'hippie-expand)

;; Lisp-friendly hippie expand
(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol))

;; Highlights matching parenthesis
(show-paren-mode 1)

;; Highlight current line
(global-hl-line-mode 1)

;; Interactive search key bindings. By default, C-s runs
;; isearch-forward, so this swaps the bindings.
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

;; Don't use hard tabs
(setq-default indent-tabs-mode nil)

;; When you visit a file, point goes to the last place where it
;; was when you previously visited the same file.
;; http://www.emacswiki.org/emacs/SavePlace
(require 'saveplace)
(setq-default save-place t)
;; keep track of saved places in ~/.emacs.d/places
(setq save-place-file (concat user-emacs-directory "places"))

;; Emacs can automatically create backup files. This tells Emacs to
;; put all backups in ~/.emacs.d/backups. More info:
;; http://www.gnu.org/software/emacs/manual/html_node/elisp/Backup-Files.html
(setq backup-directory-alist `(("." . ,(concat user-emacs-directory
                                               "backups"))))
(setq auto-save-default nil)

;; yasnippet
;; http://www.emacswiki.org/emacs/Yasnippet
(require 'yasnippet)
(yas-global-mode 1)

;; comments
(defun toggle-comment-on-line ()
  "comment or uncomment current line"
  (interactive)
  (comment-or-uncomment-region (line-beginning-position) (line-end-position)))
(global-set-key (kbd "C-;") 'toggle-comment-on-line)

;; yay rainbows!
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;; use 2 spaces for tabs
(defun die-tabs ()
  (interactive)
  (set-variable 'tab-width 2)
  (mark-whole-buffer)
  (untabify (region-beginning) (region-end))
  (keyboard-quit))

;; fix weird os x kill error
(defun ns-get-pasteboard ()
  "Returns the value of the pasteboard, or nil for unsupported formats."
  (condition-case nil
      (ns-get-selection-internal 'CLIPBOARD)
    (quit nil)))

(setq electric-indent-mode nil)

(global-set-key (kbd "s-g") 'magit-status)
(global-set-key (kbd "s-b") 'magit-blame)

;; crux
(require 'crux)

;;smartparens
(require 'smartparens-config)
(setq sp-interactive-dwim t)
(sp-use-smartparens-bindings)
(sp-pair "(" ")" :wrap "C-(") ;; how do people live without this?
(sp-pair "[" "]" :wrap "s-[") ;; C-[ sends ESC
(sp-pair "{" "}" :wrap "C-{")

;; WORKAROUND https://github.com/Fuco1/smartparens/issues/543
(bind-key "C-<left>" nil smartparens-mode-map)
(bind-key "C-<right>" nil smartparens-mode-map)

(bind-key "s-<delete>" 'sp-kill-sexp smartparens-mode-map)
(bind-key "s-<backspace>" 'sp-backward-kill-sexp smartparens-mode-map)
(bind-key "s-<home>" 'sp-beginning-of-sexp smartparens-mode-map)
(bind-key "s-<end>" 'sp-end-of-sexp smartparens-mode-map)
(bind-key "s-<up>" 'sp-beginning-of-previous-sexp smartparens-mode-map)
;; sp-next-sp could be better https://github.com/Fuco1/smartparens/issues/541
(bind-key "s-<down>" 'sp-next-sexp smartparens-mode-map)
(bind-key "s-<left>" 'sp-backward-up-sexp smartparens-mode-map)
(bind-key "s-<right>" 'sp-down-sexp smartparens-mode-map)

