(use-package dired
  :config
  (require 'dired-x)
  (setq dired-omit-files "^\\.?#\\|^\\.[^.].*")

  (defun air-dired-buffer-dir-or-home ()
    "Open dired to the current buffer's dir, or $HOME."
    (interactive)
    (let ((cwd (or (file-name-directory (or (buffer-file-name) ""))
                   (expand-file-name "~"))))
      (dired cwd)))

  (defun my-dired-create-file (file)
    "Create a file called FILE.
    If FILE already exists, signal an error."
    (interactive
    (list (read-file-name "Create file: " (dired-current-directory))))
    (let* ((expanded (expand-file-name file))
       (try expanded)
       (dir (directory-file-name (file-name-directory expanded)))
       new)
      (if (file-exists-p expanded)
          (error "Cannot create file %s: file exists" expanded))
      ;; Find the topmost nonexistent parent dir (variable `new')
      (while (and try (not (file-exists-p try)) (not (equal new try)))
      (setq new try
              try (directory-file-name (file-name-directory try))))
      (when (not (file-exists-p dir))
      (make-directory dir t))
      (write-region "" nil expanded t)
      (when new
      (dired-add-file new)
      (dired-move-to-filename))))

  (add-hook 'dired-mode-hook (lambda ()
                               (dired-omit-mode t)))
  (define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file)
  (define-key dired-mode-map (kbd "^")   (lambda () (interactive) (find-alternate-file "..")))
  (define-key dired-mode-map (kbd "C-.") 'dired-omit-mode)
  ;(define-key dired-mode-map (kbd "c")   'find-file)
  (define-key dired-mode-map (kbd "c")   'my-dired-create-file)
  (define-key dired-mode-map (kbd "/")   'evil-search-forward)
  (define-key dired-mode-map (kbd "?")   'evil-search-backward))

(provide 'init-dired)
