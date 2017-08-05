;;; init-doom-themes.el -- Doom Theme configuration
;;; Commentary:
;;
;; It's just a matter of code.
;;
;;; Code:
(use-package doom-themes
  :ensure t
  :defer t
  :config
    ;;; Settings (defaults)
    ;; if nil, bolding are universally disabled
    (setq doom-enable-bold t)
    ;; if nil, italics are universally disabled
    (setq doom-enable-italic t)
    ;; doom-one specific settings
    (setq doom-one-brighter-modeline nil)
    (setq doom-one-brighter-comments nil)
  )

(load-theme 'doom-one t)

;;; OPTIONAL
;; brighter source buffers (that represent files)
(add-hook 'find-file-hook 'doom-buffer-mode-maybe)
;; ...if you use auto-revert-mode
(add-hook 'after-revert-hook 'doom-buffer-mode-maybe)
;; And you can brighten other buffers (unconditionally) with:
(add-hook 'ediff-prepare-buffer-hook 'doom-buffer-mode)

;; brighter minibuffer when active
(add-hook 'minibuffer-setup-hook 'doom-brighten-minibuffer)

;; Enable custom neotree theme
(doom-themes-neotree-config)  ; all-the-icons fonts must be installed!

;; Enable nlinum line highlighting
(doom-themes-nlinum-config)   ; requires nlinum and hl-line-mode

;; Necessary for org-mode
(setq org-fontify-whole-heading-line t
      org-fontify-done-headline t
      org-fontify-quote-and-verse-blocks t)

(provide 'init-doom-themes)
;;; init-doom-themes.el ends here
