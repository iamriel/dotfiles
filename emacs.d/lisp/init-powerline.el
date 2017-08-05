;;; init-powerline.el -- Powerline config
;;; Commentary:
;;; Code:

(use-package powerline
  :ensure t)

(use-package smart-mode-line-powerline-theme
  :ensure t)

(use-package smart-mode-line
  :ensure t
  :config
  (require 'powerline)
  (setq powerline-default-separator-dir '(right . left))
  (setq sml/theme 'powerline)
  (sml/setup)
  ;; These colors are more pleasing (for gruvbox)
  (custom-theme-set-faces
    'user
    '(powerline-evil-normal-face ((t (:inherit powerline-evil-base-face :background "chartreuse3"))))
    '(sml/folder ((t (:inherit sml/global :background "grey22" :foreground "PaleGreen" :weight normal))) t)
    '(sml/modes ((t (:inherit sml/global :background "grey22" :foreground "LightSkyBlue" :weight normal))) t)
    '(sml/git ((t (:background "grey22" :foreground "chartreuse"))) t)
    '(sml/vc ((t (:inherit sml/git :foreground "AliceBlue"))) t)
    '(sml/vc-edited ((t (:background "grey22" :foreground "LightPink"))) t)
    '(sml/position-percentage ((t (:background "grey22" :foreground "LightPink"))) t)
    ;; '(sml/client ((t (:background "grey22" :foreground "chartreuse"))) t)
  ))
(provide 'init-powerline)
;;; init-powerline.el ends here
