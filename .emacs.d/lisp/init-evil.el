;;; init-evil.el -- My evil mode configuration.
;;; Commentary:
;;; Code:
(defun air--config-evil-leader ()
  "Configure evil leader mode."
  (evil-leader/set-leader ",")
  (evil-leader/set-key
    ","  'jedi:goto-definition
    "."  'jedi:goto-definition-pop-marker
    ":"  'eval-expression
    "?"  'jedi:show-doc
    "b"  'helm-mini             ;; Switch to another buffer
    "B"  'magit-blame-toggle
    "c"  'comment-line
    "C"  'comment-or-uncomment-region
    "d"  'kill-this-buffer
    "f"  'helm-imenu            ;; Jump to function in buffer
    "h"  'fontify-and-browse    ;; HTML-ize the buffer and browse the result
    "im" 'highlight-indentation-mode
    "mb" 'magit-branch-and-checkout
    "mc" 'magit-checkout
    "mC" 'magit-commit
    "ml" 'magit-log
    "mm" 'magit-merge
    "ms" 'magit-status
    "l"  'whitespace-mode       ;; Show invisible characters
    "o"  'delete-other-windows  ;; C-w o
    "p"  'magit-push-to-remote
    "P"  'magit-push
    "s"  'helm-do-ag-project-root            ;; Ag search from project's root
    "R"  (lambda () (interactive) (font-lock-fontify-buffer) (redraw-display))
    "S"  'delete-trailing-whitespace
    "t"  'elpy-test-django-runner
    "v"  'venv-workon
    "w"  'save-buffer
    "x"  'helm-M-x
    "y"  'yank-to-x-clipboard))

(defun air--config-evil ()
  "Configure evil mode."
  ;(line-number-mode 1)

  ;; Use Emacs state in these additional modes.
  (dolist (mode '(ag-mode
                  dired-mode
                  eshell-mode
                  flycheck-error-list-mode
                  git-rebase-mode
                  octopress-mode
                  octopress-server-mode
                  octopress-process-mode
                  sunshine-mode
                  term-mode))
    (add-to-list 'evil-emacs-state-modes mode))

  (delete 'term-mode evil-insert-state-modes)
  (delete 'eshell-mode evil-insert-state-modes)

  ;; Use insert state in these additional modes.
  (dolist (mode '(twittering-edit-mode
                  magit-log-edit-mode))
    (add-to-list 'evil-insert-state-modes mode))

  (add-to-list 'evil-buffer-regexps '("\\*Flycheck"))

  (evil-add-hjkl-bindings occur-mode-map 'emacs
    (kbd "/")       'evil-search-forward
    (kbd "n")       'evil-search-next
    (kbd "N")       'evil-search-previous
    (kbd "C-d")     'evil-scroll-down
    (kbd "C-u")     'evil-scroll-up
    (kbd "C-w C-w") 'other-window)

  (evil-define-key 'insert global-map (kbd "C-j") (simulate-key-press "<down>"))
  (evil-define-key 'insert global-map (kbd "C-k") (simulate-key-press "<up>"))
  (evil-define-key 'normal global-map (kbd ";") (simulate-key-press ":"))

  ;; Global bindings.
  (evil-define-key 'normal global-map (kbd "<down>")  'evil-next-visual-line)
  (evil-define-key 'normal global-map (kbd "<up>")    'evil-previous-visual-line)
  (evil-define-key 'normal global-map (kbd "-")       'helm-find-files)
  ;; (evil-define-key 'normal global-map (kbd "C--")     'air-dired-buffer-dir-or-home)
  ;; (evil-define-key 'normal global-map (kbd "C--")     'my-dired-create-file)
  (evil-define-key 'normal global-map (kbd "C-`")     (lambda ()
                                                        (interactive)
                                                        (dired (expand-file-name "~"))))
  (evil-define-key 'normal global-map (kbd "C-]")     'gtags-find-tag-from-here)
  (evil-define-key 'normal global-map (kbd "g/")      'occur-last-search)
  (evil-define-key 'normal global-map (kbd "[i")      'show-first-occurrence)
  (evil-define-key 'normal global-map (kbd "S-SPC")   'air-pop-to-org-agenda-default)
  (evil-define-key 'normal global-map (kbd "C-p")     'helm-projectile)
  (evil-define-key 'normal global-map (kbd "C-S-p")   'helm-projectile-switch-project)
  (evil-define-key 'insert global-map (kbd "s-d")     'eval-last-sexp)
  (evil-define-key 'normal global-map (kbd "s-d")     'eval-defun)
  (evil-define-key 'normal global-map (kbd "C-t")     'air-open-eshell)
  (evil-define-key 'normal global-map (kbd "z d")     'dictionary-lookup-definition)

  (evil-define-key 'insert global-map (kbd "C-;")     'evil-normal-state)
  (evil-define-key 'normal global-map (kbd "C-h")     'evil-window-left)
  (evil-define-key 'normal global-map (kbd "C-j")     'evil-window-down)
  (evil-define-key 'normal global-map (kbd "C-k")     'evil-window-up)
  (evil-define-key 'normal global-map (kbd "C-l")     'evil-window-right)

  (defun minibuffer-keyboard-quit ()
    "Abort recursive edit.
In Delete Selection mode, if the mark is active, just deactivate it;
then it takes a second \\[keyboard-quit] to abort the minibuffer."
    (interactive)
    (if (and delete-selection-mode transient-mark-mode mark-active)
        (setq deactivate-mark  t)
      (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
      (abort-recursive-edit)))

  ;; Make escape quit everything, whenever possible.
  (define-key evil-normal-state-map [escape] 'keyboard-escape-quit)
  (define-key evil-visual-state-map [escape] 'keyboard-quit)
  (define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)

  ;; My own Ex commands.
  (evil-ex-define-cmd "om" 'octopress-status))

(use-package evil
  :ensure t
  :config
  (add-hook 'evil-mode-hook 'air--config-evil)
  (evil-mode 1)

  (use-package evil-leader
    :ensure t
    :config
    (global-evil-leader-mode)
    (air--config-evil-leader))
)

(provide 'init-evil)
;;; init-evil.el ends here
