;; This file contains utilities related to xml 

;; Format xml using xmlint
;; it is required that the xml2-utils is installed in the system
(defun arber-xml-format ()
  "Format an XML buffer with `xmllint'."
  (interactive)
  (shell-command-on-region (point-min) (point-max)
                           "xmllint -format --noblanks --nonet --nsclean -"
                           (current-buffer) t
                           "*Xmllint Error Buffer*" t))


(defun arber-xml-validate ()
 "Validate an XML buffer with `xmlint'."
  (interactive)
  (shell-command-on-region (point-min) (point-max)
                           "xmllint -valid --noblanks --nonet --nsclean -"
                           (current-buffer) t
                           "*Xmllint Error Buffer*" t))


;; bind xml formating and validation to key combination
(use-package nxml-mode                
  :mode "\\.xml\\'"
  :bind (:map nxml-mode-map
              ("C-c m f" . arber-xml-format)
              ("C-c m v" . arber-xml-validate)))




(provide 'setup-xml)

