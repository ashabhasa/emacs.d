;; configurations for modeline

;; Enable minions mode and configure it
;;
(minions-mode t)
(setq minions-direct '(flycheck-mode)
      minions-mode-line-lighter "#")

;; Enable and configure Dimmer
;;
(dimmer-mode t)
 (setq-default dimmer-fraction 0.20)
(advice-add 'frame-set-background-mode
              :after (lambda (&rest args) (dimmer-process-all)))
;;; The mode line
(line-number-mode)
(column-number-mode)

;; Show buffer position percentage starting from top
(setq mode-line-percent-position '(-3 "%o"))

;; Increase mode-line size with a border (box) of the same colour and
;; reduce font size by tweaking height
;; (set-face-attribute 'mode-line nil
;;                     :inverse-video nil
;;                     :height 0.2
;;                     :box '(:line-width 1 :color "#373b41" :style nil))

;; (set-face-attribute 'mode-line-inactive nil
;;                     :inverse-video nil
;;                     :box '(:line-width 1 :color "#282a2e" :style nil))


;; ;; Slightly decrease the font in the mode line
;; (set-face-attribute 'mode-line nil
;;                     :family "PragmataPro"
;;                     :height 145
;;                     :weight 'regular)
