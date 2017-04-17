;;; init-python.el -- Python configuration
;;; Commentary:
;;
;; It's just a matter of code.
;;
;;; Code:
(use-package jedi :ensure t)
(add-hook 'python-mode-hook 'jedi:setup)
;(setq jedi:complete-on-dot t)

(use-package virtualenvwrapper
  :ensure t
  :config
  (venv-initialize-interactive-shells)
  (venv-initialize-eshell)
  (setq venv-location
        (expand-file-name "~/.virtualenvs/")))

(use-package company-jedi :ensure t)

(add-hook 'python-mode-hook
  (lambda ()
  ;; I'm rudely redefining this function to do a comparison of `point'
  ;; to the end marker of the `comint-last-prompt' because the original
  ;; method of using `looking-back' to match the prompt was never
  ;; matching, which hangs the shell startup forever.
  (defun python-shell-accept-process-output (process &optional timeout regexp)
      "Redefined to actually work."
      (let ((regexp (or regexp comint-prompt-regexp)))
      (catch 'found
          (while t
          (when (not (accept-process-output process timeout))
              (throw 'found nil))
          (when (= (point) (cdr (python-util-comint-last-prompt)))
              (throw 'found t))))))

  ;; Additional settings follow.
  ;(add-to-list 'write-file-functions 'delete-trailing-whitespace)
  (add-to-list 'company-backends 'company-jedi)
  ))

(provide 'init-python)
;;; init-python.el ends here
