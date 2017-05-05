;;; init.el -- My Emacs configuration
;-*-Emacs-Lisp-*-

;;; Commentary:
;;
;; I have nothing substantial to say here.
;;
;;; Code:

;; Leave this here, or package.el will just add it again.
(package-initialize)

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; Also add all directories within "lisp"
;; I use this for packages I'm actively working on, mostly.
(let ((files (directory-files-and-attributes "~/.emacs.d/lisp" t)))
  (dolist (file files)
    (let ((filename (car file))
          (dir (nth 1 file)))
      (when (and dir
                 (not (string-suffix-p "." filename)))
        (add-to-list 'load-path (car file))))))

(add-to-list 'custom-theme-load-path (expand-file-name "themes" user-emacs-directory))
(add-to-list 'exec-path "/usr/local/bin")

;; Don't litter my init file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)

(add-to-list 'package-archives '("elpy" . "http://jorgenschaefer.github.io/packages/"))

(require 'init-bell)
(require 'init-utils)
(require 'init-elpa)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))


(setq inhibit-splash-screen t
      inhibit-startup-message t
      inhibit-startup-echo-area-message t
      scroll-step 1
      scroll-conservatively 1000)

(menu-bar-mode -1)
(tool-bar-mode -1)
(when (boundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(show-paren-mode 1)
;(global-linum-mode 1)
(setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))
(setq-default left-fringe-width nil)
(setq-default indicate-empty-lines t)
(setq-default indent-tabs-mode nil)

;; Why did I do this? Perhaps to keep vc from meddling with things
;; that Magit does, but it's convenient to be able to lean on vc for
;; certain things, so let's try it again with this turned on.
;; (eval-after-load "vc" '(setq vc-handled-backends nil))

(setq vc-follow-symlinks t)
(setq large-file-warning-threshold nil)
(setq split-width-threshold nil)
(setq custom-safe-themes t)
;(column-number-mode t)
(setq tab-width 4)


(defun my-minibuffer-setup-hook ()
  "Increase GC cons threshold."
  (setq gc-cons-threshold most-positive-fixnum))

(defun my-minibuffer-exit-hook ()
  "Set GC cons threshold to its default value."
  (setq gc-cons-threshold 1000000))

(add-hook 'minibuffer-setup-hook #'my-minibuffer-setup-hook)
(add-hook 'minibuffer-exit-hook #'my-minibuffer-exit-hook)

(defvar backup-dir "~/.emacs.d/backups/")
(setq backup-directory-alist (list (cons "." backup-dir)))
(setq make-backup-files nil)

;; Allow confusing functions
(put 'narrow-to-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)

;;; Larger package-specific configurations.
(require 'diminish)
(require 'init-angular)
(require 'init-fonts)
(require 'init-flycheck)
(require 'init-dired)
(require 'init-epc)
(require 'init-evil)
(require 'init-neotree)
;(require 'init-gtags)
(require 'init-theme)
(require 'init-tdd)
(require 'init-python)
(require 'python-test)

(use-package yaml-mode :ensure t :defer t)

(use-package elpy
  :init
  (elpy-enable)
  :config
  (setq elpy-rpc-backend "jedi")
  (setq elpy-test-django-runner-command '("./manage.py" "test"))
  (setq elpy-test-django-with-manage t)
  (setq python-shell-interpreter "ipython")
  )

(add-hook 'compilation-filter-hook 'python-test-track-pdb-prompt)

(use-package emmet-mode
  :ensure t
  :config
  (add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
  (add-hook 'css-mode-hook  'emmet-mode) ;; enable Emmet's css abbreviation.
  (setq emmet-move-cursor-between-quotes t) ;; default nil
  )

;;; Emmet mode:
(add-hook 'emmet-mode-hook
  (lambda ()
    (evil-define-key 'insert emmet-mode-keymap (kbd "C-S-l") 'emmet-next-edit-point)
    (evil-define-key 'insert emmet-mode-keymap (kbd "C-S-h") 'emmet-prev-edit-point)
  ))

(use-package ansible
  :ensure t
  :config
  (setq ansible::vault-password-file "/Users/light/.config/ansible/vault_pass.txt")
  (add-hook 'yaml-mode-hook '(lambda () (ansible 1)))
  (add-hook 'ansible-hook 'ansible::auto-decrypt-encrypt)
  )

;; Utilities
(use-package s
  :ensure t
  :defer 1)
(use-package dash :ensure t)

(use-package evil-escape
  :ensure t
  :config
  (evil-escape-mode 1)
  (setq evil-escape-key-sequence "jk"))

(use-package visual-fill-column :ensure t)

(use-package helm-ag
  :ensure t
  :commands (helm-do-grep-ag))

(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-env "PYTHONPATH")
  )

(use-package helm
  :ensure t
  :diminish helm-mode
  :commands helm-mode
  :config
  (helm-mode 1)
  (setq helm-buffers-fuzzy-matching t)
  (setq helm-autoresize-mode t)
  (setq helm-buffer-max-length 40)
  ;; (setq helm-split-window-in-side-p t)
  (define-key helm-map (kbd "S-SPC") 'helm-toggle-visible-mark)
  (define-key helm-map (kbd "C-j") 'helm-next-line)
  (define-key helm-map (kbd "C-k") 'helm-previous-line))

(use-package company
  :ensure t
  :defer t
  :init
  (global-company-mode)
  :config
  ; (setq company-tooltip-common-selection ((t (:inherit company-tooltip-selection :background "yellow2" :foreground "#c82829"))))
  ; (setq company-tooltip-selection ((t (:background "yellow2"))))
  (setq company-idle-delay 0.2)
  (setq company-selection-wrap-around t)
  ;; aligns annotation to the right hand side
  (setq company-tooltip-align-annotations t)
  (define-key company-active-map [tab] 'company-complete)
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous))

(use-package dictionary :ensure t)

(use-package flycheck
  :ensure t
  :commands flycheck-mode)

(use-package projectile
  :ensure t
  :defer 1
  :config
  (projectile-mode)
  (setq projectile-enable-caching t))

(use-package helm-projectile
  :commands (helm-projectile helm-projectile-switch-project)
  :ensure t)

(use-package magit
  :ensure t
  :defer t
  :config
  (setq magit-branch-arguments nil)
  (setq magit-push-always-verify nil)
  (setq magit-last-seen-setup-instructions "1.4.0")
  (magit-define-popup-switch 'magit-log-popup ?f "first parent" "--first-parent"))

;;; Magit mode (which does not open in evil-mode):
(add-hook 'magit-mode-hook
          (lambda ()
            (define-key magit-mode-map (kbd ",o") 'delete-other-windows)))

;;; Git Commit Mode (a Magit minor mode):
(add-hook 'git-commit-mode-hook 'evil-insert-state)

;;; Treat underscore as part of the word
(with-eval-after-load 'evil
    (defalias #'forward-evil-word #'forward-evil-symbol))

;; Global Keys
(global-set-key (kbd "C-c b") 'switch-to-prev-buffer)
(global-set-key (kbd "C-c n") 'switch-to-next-buffer)
(global-set-key (kbd "C-h") 'windmove-left)
(global-set-key (kbd "C-j") 'windmove-down)
(global-set-key (kbd "C-k") 'windmove-up)
(global-set-key (kbd "C-l") 'windmove-right)


(provide 'init)
;;; init.el ends here
