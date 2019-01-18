;; configurations for modeline
;; (use-package minions
;;   :ensure t
;;   :init (minions-mode)
;;   :config 
;;    (validate-setq
;;     minions-mode-line-lighter "#"
;;     minions-direct '(flycheck-mode
;;                      cider-mode)))

(use-package minions
 :ensure t
 :init (minions-mode) 
 :config
     (setq minions-direct '(cider-mode flycheck-mode overwrite-mode)
           minions-mode-line-lighter "#"))

(use-package moody
  :ensure t
  :config
  (setq x-underline-at-descent-line t)
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode))

(use-package dimmer                     ; Highlight selected buffer
  :ensure t
  :init (dimmer-mode)
  :config
  (setq-default dimmer-fraction 0.20)
  (advice-add 'frame-set-background-mode
              :after (lambda (&rest args) (dimmer-process-all))))


;; disply line numbers
(global-display-line-numbers-mode)

;;; The mode line
(line-number-mode)
(column-number-mode)

;; Show buffer position percentage starting from top
(setq mode-line-percent-position '(-3 "%o"))

;; (setq-default mode-line-format
;;               '("%e"
;;                 ;; arber-eyebrowse-mode-line
;;                 mode-line-front-space
;;                 mode-line-mule-info
;;                 mode-line-client
;;                 mode-line-modified
;;                 mode-line-remote
;;                 mode-line-frame-identification
;;                 mode-line-buffer-identification " " mode-line-position
;;                 (vc-mode vc-mode)
;;                 (multiple-cursors-mode mc/mode-line)
;;                 " " mode-line-modes
;;                 mode-line-end-spaces))


(let ((line (face-attribute 'mode-line :underline)))
    (set-face-attribute 'mode-line          nil :overline   line)
    (set-face-attribute 'mode-line-inactive nil :overline   line)
    (set-face-attribute 'mode-line-inactive nil :underline  line)
    (set-face-attribute 'mode-line          nil :box        nil)
    (set-face-attribute 'mode-line-inactive nil :box        nil)
    (set-face-attribute 'mode-line-inactive nil :background "#282a2e"))


;; Slightly decrease the font in the mode line
(set-face-attribute 'mode-line nil
                    :family "PragmataPro"
                    :height 145
                    :weight 'regular)
