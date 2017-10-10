;;; init-angular.el -- Angular configuration
;;; Commentary:
;;
;; It's just a matter of code.
;;
;;; Code:

(use-package ng2-mode
  :ensure t)

(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  ;; company is an optional dependency. You have to
  ;; install it separately via package-install
  ;; `M-x package-install [ret] company`
  (company-mode +1))

;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)
(setq flycheck-check-syntax-automatically '(save mode-enabled))
(setq typescript-indent-level 4)
(setq tide-format-options '(:indentSize 4 :tabSize 4))
(setq company-tooltip-align-annotations t)

;; formats the buffer before saving
;; (add-hook 'before-save-hook 'tide-format-before-save)

(add-hook 'typescript-mode-hook #'setup-tide-mode)

(provide 'init-angular)
;;; init-angular.el ends here
