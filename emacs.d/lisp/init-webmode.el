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
    (add-to-list 'auto-mode-alist '("\\.php?\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.css?\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.scss?\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.js?\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.vue?\\'" . web-mode))
    (setq web-mode-engines-alist
        '(("django"    . "\\.html\\'")
            ("php"    . "\\.php\\'"))
    )
    (setq web-mode-ac-sources-alist
        '(("css" . (ac-source-css-property))
            ("html" . (ac-source-words-in-buffer ac-source-abbrev))))
    ;; (setq web-mode-content-types-alist
    ;;     '(("jsx" . "\\.js[x]?\\'")))
  )

(eval-after-load "web-mode"
  '(set-face-underline 'web-mode-current-element-highlight-face t))

(defun my-web-mode-hook ()
    (setq web-mode-enable-current-column-highlight t)
    (setq web-mode-enable-auto-indentation t)
    (setq web-mode-attr-indent-offset 4)
    (setq web-mode-code-indent-offset 4)
    (setq web-mode-css-indent-offset 4)
    (setq web-mode-indent-style 4)
    (setq web-mode-markup-indent-offset 4)
    (setq web-mode-sql-indent-offset 4)
    (setq web-mode-script-padding 4)
)
(add-hook 'web-mode-hook  'my-web-mode-hook)

(use-package emmet-mode
  :ensure t
  :config
  (add-hook 'web-mode-hook 'emmet-mode)
  (add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
  (add-hook 'css-mode-hook  'emmet-mode) ;; enable Emmet's css abbreviation.
  (setq emmet-move-cursor-between-quotes t) ;; default nil
  )

;;; Emmet mode:
(add-hook 'emmet-mode-hook
  (lambda ()
    (evil-define-key 'insert emmet-mode-keymap (kbd "C-l") 'emmet-next-edit-point)
    (evil-define-key 'insert emmet-mode-keymap (kbd "C-h") 'emmet-prev-edit-point)
  ))

;; for better jsx syntax-highlighting in web-mode
;; - courtesy of Patrick @halbtuerke
(defadvice web-mode-highlight-part (around tweak-jsx activate)
  (if (equal web-mode-content-type "jsx")
    (let ((web-mode-enable-part-face nil))
      ad-do-it)
    ad-do-it))

(provide 'init-webmode)
;;; init-webmode ends here
