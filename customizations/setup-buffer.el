;; Don't let the cursor go into minibuffer prompt
(let ((default (eval (car (get 'minibuffer-prompt-properties 'standard-value))))
      (dont-touch-prompt-prop '(cursor-intangible t)))
  (setq minibuffer-prompt-properties
        (append default dont-touch-prompt-prop))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode))

;; Allow to read from minibuffer while in minibuffer.
(setq enable-recursive-minibuffers t)

;; Show the minibuffer depth (when larger than 1)
(minibuffer-depth-indicate-mode 1)

(setq history-length 1000               ; Store more history
      use-dialog-box nil)               ; Never use dialogs for minibuffer input

(use-package savehist                   ; Save minibuffer history
  :init (savehist-mode t)
  :config
  (setq savehist-autosave-interval 180
        savehist-save-minibuffer-history t))

;; Don't ask for confirmation
(setq kill-buffer-query-functions
      (delq 'process-kill-buffer-query-function
            kill-buffer-query-functions))

(setq frame-resize-pixelwise t
      frame-title-format '(:eval (if (buffer-file-name)
                                     (abbreviate-file-name (buffer-file-name))
                                   "%b")))

;; Configure `display-buffer' behaviour for some special buffers
(setq display-buffer-alist
      `(;; Messages, errors, processes, Calendar in the bottom side window
        (,(rx bos (or "*Apropos"                   ; Apropos buffers
                      "*Man"                       ; Man buffers
                      "*Help"                      ; Help buffers
                      "*Warnings*"                 ; Emacs warnings
                      "*Process List*"             ; Processes
                      "*Proced"                    ; Proced processes list
                      "*Compile-Log*"              ; Emacs byte compiler log
                      "*compilation"               ; Compilation buffers
                      "*Flycheck errors*"          ; Flycheck error list
                      "*Calendar"                  ; Calendar window
                      "*env-info"                  ; Environment information
                      "*Cargo"                     ; Cargo process buffers
                      "*Word"                      ; WordNut buffers
                      "*ivy-occur"                 ; ivy-occur search results
                      (and (1+ nonl) " output*"))) ; AUCTeX command output
         (display-buffer-reuse-window display-buffer-in-side-window)
         (side . bottom)
         (reusable-frames . visible)
         (window-height . 0.45))
        ;; REPLs on the bottom half
        (,(rx bos (or "*cider-repl"     ; CIDER REPL
                      "*intero"         ; Intero REPL
                      "*idris-repl"     ; Idris REPL
                      "*ielm"           ; IELM REPL
                      "*SQL"))          ; SQL REPL
         (display-buffer-reuse-window display-buffer-in-side-window)
         (side . bottom)
         (reusable-frames . visible)
         (window-height . 0.50))
        ;; Open shell in a single window
        (,(rx bos "*shell")
         (display-buffer-same-window)
         (reusable-frames . nil))
        ;; Open PDFs in the right side window
        (,(rx bos "*pdf")
         (display-buffer-reuse-window display-buffer-in-side-window)
         (side . right)
         (reusable-frames . visible)
         (window-width . 0.5))
        ;; Let `display-buffer' reuse visible frames for all buffers. This must
        ;; be the last entry in `display-buffer-alist', because it overrides any
        ;; previous entry with more specific actions.
        ("." nil (reusable-frames . visible))))

(use-package uniquify                   ; Unique buffer names
  :config (setq uniquify-buffer-name-style 'post-forward
                uniquify-ignore-buffers-re "^\\*"
                uniquify-separator " • "))

(use-package ibuf-ext                   ; Extensions for Ibuffer
  :config (setq-default ibuffer-show-empty-filter-groups nil))

(use-package ibuffer                    ; Buffer management
  :bind (("C-x C-b" . arber-ibuffer-open)
         ([remap list-buffers] . ibuffer)
         :map ibuffer-mode-map
         ("q" . arber-pop-window-configuration))
  :config
  (setq ibuffer-expert t         ; Do not prompt when on kill buffers operations
        ibuffer-filter-group-name-face 'font-lock-doc-face)

  (defun arber-ibuffer-open ()
    "Save window configuration and call `ibuffer'."
    (interactive)
    (arber-save-wins-then-call 'ibuffer))


  (defvar arber-saved-window-configuration nil)

  (defun arber-save-wins-then-call (func &optional args)
    "Save current window configuration, then call FUNC optionally with ARGS."
    (interactive)
    (push (current-window-configuration) arber-saved-window-configuration)
    (cond
     ;; We have arguments for the function
     ((bound-and-true-p args) (funcall func args))
     ;; The function expects exactly one argument, and we want it to be nil
     ((equal args "nil") (funcall func nil))
     ;; The function does not expect arguments
     (t (funcall func))))

  ;; Use a single full frame for ibuffer
  ;; (with-eval-after-load 'ibuffer
  ;;   (fullframe ibuffer arber-pop-window-configuration))

  (defun arber-ibuffer-set-up-preferred-filters ()
    (ibuffer-vc-set-filter-groups-by-vc-root)
    (unless (eq ibuffer-sorting-mode 'filename/process)
      (ibuffer-do-sort-by-filename/process)))

  (add-hook 'ibuffer-hook #'arber-ibuffer-set-up-preferred-filters)

  ;; Use human readable Size column instead of original one
  (define-ibuffer-column size-h
    (:name "Size" :inline t)
    (file-size-human-readable (buffer-size)))

  ;; Modify the default ibuffer-formats (toggle with `)
  (setq ibuffer-formats
        '((mark modified read-only vc-status-mini " "
                (name 22 22 :left :elide)
                " "
                (size-h 9 -1 :right)
                " "
                (mode 16 16 :left :elide)
                " "
                vc-relative-file)
          (mark modified read-only vc-status-mini " "
                (name 22 22 :left :elide)
                " "
                (size-h 9 -1 :right)
                " "
                (mode 16 16 :left :elide)
                " "
                (vc-status 16 16 :left)
                " "
                vc-relative-file))))

(use-package ibuffer-vc                 ; Group buffers by VC project and status
  :ensure t
  :init (add-hook 'ibuffer-hook
                  (lambda ()
                    (ibuffer-vc-set-filter-groups-by-vc-root)
                    (unless (eq ibuffer-sorting-mode 'filename/process)
                      (ibuffer-do-sort-by-filename/process)))))

;; Use `emacs-lisp-mode' instead of `lisp-interaction-mode' for scratch buffer
(setq initial-major-mode 'emacs-lisp-mode)

;;; Utilities and key bindings
;; Don't kill the important buffers
(defconst arber-do-not-kill-buffer-names '("*scratch*" "*Messages*")
  "Names of buffers that should not be killed.")

;;;###autoload
(defun arber-do-not-kill-important-buffers ()
  "Inhibit killing of important buffers.
Add this to `kill-buffer-query-functions'."
  (if (not (member (buffer-name) arber-do-not-kill-buffer-names))
      t
    (message "Not allowed to kill %s, burying instead" (buffer-name))
    (bury-buffer)
    nil))

;; Don't kill important buffers
(add-hook 'kill-buffer-query-functions #'arber-do-not-kill-important-buffers)

(bind-key "C-x C-k" #'kill-this-buffer)  ; Kill only the current buffer

(provide 'setup-buffer)
