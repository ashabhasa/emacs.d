;; These customizations change the way emacs looks and disable/enable
;; some user interface elements. Some useful customizations are
;; commented out, and begin with the line "CUSTOMIZE". These are more
;; a matter of preference and may require some fiddling to match your
;; preferences

;; Turn off the menu bar at the top of each frame because it's distracting
(menu-bar-mode -1)

;; Disable startup messages
(setq initial-scratch-message nil
      inhibit-startup-screen t
      ring-bell-function #'ignore)

;; You can uncomment this to remove the graphical toolbar at the top. After
;; awhile, you won't need the toolbar.
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

;; Don't show native OS scroll bars for buffers because they're redundant
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

(if (boundp 'fringe-mode)
    (fringe-mode '(4 . 0)))
(global-git-gutter-mode +1)

;; Configure the mode line
;; (setq-default mode-line-format
;;               '((:eval (propertize " %b " 'face
;;                                    (if (buffer-modified-p)
;;                                        '(:background "grey" :foreground "black" :weight bold)
;;                                      'mode-line-highlight)))
;;                 " %l:%c %p %m"
;;                 (:propertize (vc-mode vc-mode) face (:weight normal))))


;; disply line numbers
(global-display-line-numbers-mode)

;; typography
(setq-default line-spacing nil)
(setq mac-allow-anti-aliasing t)
(global-prettify-symbols-mode 1) ;; this causes alignment issues

;; Increase size for my poor eyes
;;(set-face-attribute 'default nil :font "Fira Code-17")
;; (set-face-attribute 'default nil :font "PragmataPro-17")


;;; Fonts setup
(set-face-attribute 'default nil
                    :family "PragmataPro"
                    :height 165
                    :weight 'regular)
(set-face-attribute 'fixed-pitch nil
                    :family "PragmataPro"
                    :height 165
                    :weight 'regular)
(set-face-attribute 'variable-pitch nil
                    :family "Fira Sans"
                    :height 165
                    :weight 'regular)


;; Better scrolling
(setq redisplay-dont-pause t
      scroll-margin 1
      scroll-step 1
      scroll-conservatively 10000
      scroll-preserve-screen-position 1)

(setq mouse-wheel-follow-mouse 't)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))

;; Color Themes
;; Read http://batsov.com/articles/2012/02/19/color-theming-in-emacs-reloaded/
;; for a great explanation of emacs color themes.
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Custom-Themes.html
;; for a more technical explanation.
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(add-to-list 'load-path "~/.emacs.d/themes")

;; (require 'doom-themes)

;; Global settings (defaults)
;; (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
;;       doom-themes-enable-italic t) ; if nil, italics is universally disabled

;; Load the theme (doom-one, doom-molokai, etc); keep in mind that each theme
;; may have their own settings.
(load-theme 'doom-tomorrow-night t)
;; (load-theme 'doom-one t)

;; Enable flashing mode-line on errors
(doom-themes-visual-bell-config)

;; Enable custom neotree theme
(doom-themes-neotree-config)  ; all-the-icons fonts must be installed!

;; Corrects (and improves) org-mode's native fontification.
(doom-themes-org-config)

;; (load-theme 'sanityinc-tomorrow-night t)

;; Uncomment the lines below by removing semicolons and play with the
;; values in order to set the width (in characters wide) and height
;; (in lines high) Emacs will have whenever you start it
(setq initial-frame-alist '((top . 0) (left . 0)
                            (width . 120) (height . 40)))

;; These settings relate to how emacs interacts with your operating system
(setq ;; makes killing/yanking interact with the clipboard
      x-select-enable-clipboard t

      ;; I'm actually not sure what this does but it's recommended?
      x-select-enable-primary t

      ;; Save clipboard strings into kill ring before replacing them.
      ;; When one selects something in another program to paste it into Emacs,
      ;; but kills something in Emacs before actually pasting it,
      ;; this selection is gone unless this variable is non-nil
      save-interprogram-paste-before-kill t

      ;; Shows all options when running apropos. For more info,
      ;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Apropos.html
      apropos-do-all t

      ;; Mouse yank commands yank at point instead of at click.
      mouse-yank-at-point t)

;; No cursor blinking, it's distracting
(blink-cursor-mode 0)

;; full path in title bar
(setq-default frame-title-format "%b (%f)")

;; don't pop up font menu
(global-set-key (kbd "s-t") '(lambda () (interactive)))

;; no bell
(setq ring-bell-function 'ignore)

;; open files in existing frame
(setq ns-pop-up-frames nil)

(hl-line-mode t)
(set-face-attribute 'hl-line nil :inherit nil :background "gray6")


;; bind super to command key
(setq mac-command-modifier 'super)

;; bind hyper to fn key
(setq ns-function-modifier 'hyper)
