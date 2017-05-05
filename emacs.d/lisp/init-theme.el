(use-package gruvbox-theme :ensure t :defer t)
(use-package solarized-theme :ensure t)
(use-package zenburn-theme :ensure t :defer t)
(use-package monokai-theme :ensure t :defer t)

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


;(load-theme 'monokai)
(load-theme 'zenburn)
;(load-theme 'solarized-dark)

(use-package powerline
  :ensure t)

(use-package smart-mode-line-powerline-theme
  :ensure t)

(use-package smart-mode-line
  :ensure t
  :config
  (require 'powerline)
  ;(setq powerline-default-separator 'arrow-fade)
  (setq sml/theme 'powerline)
  (sml/setup)
  ;; These colors are more pleasing (for gruvbox)
  (custom-theme-set-faces
   'user
   '(powerline-evil-normal-face ((t (:inherit powerline-evil-base-face :background "chartreuse3"))))
   '(sml/folder ((t (:inherit sml/global :background "grey22" :foreground "grey50" :weight normal))) t)
   '(sml/git ((t (:background "grey22" :foreground "chartreuse3"))) t)))

(provide 'init-theme)
