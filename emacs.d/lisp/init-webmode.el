;;; init-webmode.el -- Webmode configuration
;;; Commentary:
;;
;; It's just a matter of code.
;;
;;; Code:
(use-package web-mode
  :ensure t
  :config
    (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
    (setq web-mode-engines-alist
        '(("django"    . "\\.html\\'")))
    (setq web-mode-ac-sources-alist
        '(("css" . (ac-source-css-property))
            ("html" . (ac-source-words-in-buffer ac-source-abbrev))))
    (setq web-mode-enable-current-element-highlight t)
    (setq web-mode-enable-current-column-highlight t)
  )

(provide 'init-webmode)
;;; init-webmode ends here
