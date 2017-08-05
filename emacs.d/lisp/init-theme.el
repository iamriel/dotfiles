;;; init-theme.el -- Theme configuration
;;; Commentary:
;;
;; It's just a matter of code.
;;
;;; Code:
(use-package apropospriate-theme :ensure t :defer t)
(use-package gruvbox-theme :ensure t :defer t)
(use-package monokai-theme :ensure t :defer t)
(use-package solarized-theme
  :ensure t
  :defer t
  :config
    ;; make the fringe stand out from the background
    (setq solarized-distinct-fringe-background t)
    ;; Don't change the font for some headings and titles
    (setq solarized-use-variable-pitch nil)
    ;; make the modeline high contrast
    (setq solarized-high-contrast-mode-line t)
    ;; Use less bolding
    (setq solarized-use-less-bold t)
    ;; Use more italics
    (setq solarized-use-more-italic t)
    ;; Use less colors for indicators such as git:gutter, flycheck and similar
    (setq solarized-emphasize-indicators nil)
    ;; Don't change size of org-mode headlines (but keep other size-changes)
    (setq solarized-scale-org-headlines nil)
    ;; Avoid all font-size changes
    (setq solarized-height-minus-1 1.0)
    (setq solarized-height-plus-1 1.0)
    (setq solarized-height-plus-2 1.0)
    (setq solarized-height-plus-3 1.0)
    (setq solarized-height-plus-4 1.0)
  )
(use-package zenburn-theme :ensure t :defer t)

(load-theme 'apropospriate-dark t)
;; (load-theme 'zenburn)
;; (load-theme 'solarized-dark)
;; (load-theme 'solarized-dark)

(provide 'init-theme)
;;; init-theme.el ends here
