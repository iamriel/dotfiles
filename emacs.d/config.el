(setq user-full-name "Rieljun Liguid"
      user-mail-address "me@iamriel.com")

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

(defun air--config-evil-leader ()
  "Configure evil leader mode."
  (evil-leader/set-leader ",")
  (evil-leader/set-key
    ","  'jedi:goto-definition
    "."  'jedi:goto-definition-pop-marker
    ":"  'eval-expression
    "?"  'jedi:show-doc
    "b"  'balance-windows
    "B"  'magit-blame-toggle
    "c"  'comment-line
    "C"  'comment-or-uncomment-region
    "d"  'kill-this-buffer
    "D"  'magit-discard
    "es" 'eshell
    "f"  'helm-imenu            ;; Jump to function in buffer
    "F"  'set-frame-font
    "h"  'fontify-and-browse    ;; HTML-ize the buffer and browse the result
    "ic" 'projectile-invalidate-cache
    "im" 'highlight-indentation-mode
    "mb" 'magit-branch-and-checkout
    "mc" 'magit-checkout
    "mC" 'magit-commit
    "mf" 'magit-fetch
    "ml" 'magit-log
    "mm" 'magit-merge
    "ms" 'magit-status
    "n"  'neotree-toggle
    "N"  'neotree-project-dir
    "l"  'whitespace-mode       ;; Show invisible characters
    "o"  'delete-other-windows  ;; C-w o
    "p"  'magit-push-to-remote
    "P"  'magit-push
    "s"  'helm-do-ag-project-root            ;; Ag search from project's root
    "S"  'helm-resume
    "r"  'split-window-right
    "T"  'elpy-test-django-runner
    "te" 'web-mode-dom-errors-show
    "th" 'web-mode-tag-highlight
    "tm" 'web-mode-tag-match
    "tn" 'web-mode-tag-next
    "tp" 'web-mode-tag-previous
    "tw" 'delete-trailing-whitespace
    "v"  'venv-workon
    "w"  'save-buffer
    "x"  'helm-M-x
    "y"  'yank-to-x-clipboard))

(defun air--config-evil ()
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

  (evil-define-key 'insert global-map (kbd "C-y") 'yas-expand)
  (evil-define-key 'insert global-map (kbd "C-j") (simulate-key-press "<down>"))
  (evil-define-key 'insert global-map (kbd "C-k") (simulate-key-press "<up>"))
  (evil-define-key 'normal global-map (kbd ";") (simulate-key-press ":"))

  (evil-set-initial-state 'magit-mode 'insert)
  (evil-set-initial-state 'magit-status-mode 'insert)
  (evil-set-initial-state 'magit-diff-mode 'insert)
  (evil-set-initial-state 'magit-log-mode 'insert)
  (evil-define-key 'normal magit-status-mode-map (kbd "j") (simulate-key-press "<down>"))
  (evil-define-key 'insert magit-status-mode-map (kbd "j") (simulate-key-press "<down>"))
  (evil-define-key 'normal magit-status-mode-map (kbd "k") (simulate-key-press "<up>"))
  (evil-define-key 'insert magit-status-mode-map (kbd "k") (simulate-key-press "<up>"))
  (evil-define-key 'insert magit-status-mode-map (kbd "C-SPC") (simulate-key-press "<escape>"))

  ;; Global bindings.
  (evil-define-key 'normal global-map (kbd "<down>")  'evil-next-visual-line)
  (evil-define-key 'normal global-map (kbd "<up>")    'evil-previous-visual-line)
  (evil-define-key 'normal global-map (kbd "-")       'helm-find-files)
  (evil-define-key 'normal global-map (kbd "C-`")     (lambda ()
                                                        (interactive)
                                                        (dired (expand-file-name "~"))))
  (evil-define-key 'normal global-map (kbd "C-p")     'helm-projectile)
  (evil-define-key 'normal global-map (kbd "C-S-p")   'helm-projectile-switch-project)

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

(use-package evil-visualstar
  :ensure t
  :config
  (global-evil-visualstar-mode))

(use-package evil-escape
  :ensure t
  :config
  (evil-escape-mode 1)
  (setq evil-escape-key-sequence "jk"))

(use-package solarized-theme
  :defer 10
  :init
  (setq solarized-use-variable-pitch nil)
  :ensure t)

(add-to-list 'default-frame-alist '(font . "Source Code Pro for Powerline"  ))
(set-face-attribute 'default t :font "Source Code Pro for Powerline" )

(defcustom sanityinc/force-default-font-for-symbols nil
  "When non-nil, force Emacs to use your default font for symbols."
  :type 'boolean)

(defun sanityinc/maybe-use-default-font-for-symbols ()
  "Force Emacs to render symbols using the default font, if so configured."
  (when sanityinc/force-default-font-for-symbols
    (set-fontset-font "fontset-default" 'symbol (face-attribute 'default :family))))

(add-hook 'after-init-hook 'sanityinc/maybe-use-default-font-for-symbols)

;;; Changing font sizes

(require 'cl)

(defun sanityinc/font-name-replace-size (font-name new-size)
  (let ((parts (split-string font-name "-")))
    (setcar (nthcdr 7 parts) (format "%d" new-size))
    (mapconcat 'identity parts "-")))

(defun sanityinc/set-frame-font-size (size)
    (set-frame-font (sanityinc/font-name-replace-size (face-font 'default) size) t t))

(defun sanityinc/increment-default-font-height (delta)
  "Adjust the default font height by DELTA on every frame.
Emacs will keep the pixel size of the frame approximately the
same.  DELTA should be a multiple of 10, to match the units used
by the :height face attribute."
  (let* ((new-height (+ (face-attribute 'default :height) delta))
         (new-point-height (/ new-height 10)))
    (sanityinc/set-frame-font-size new-point-height)
    (set-face-attribute 'default nil :height new-height)
    (message "Default font size is now %d" new-point-height)))

(defun sanityinc/increase-default-font-height ()
  (interactive)
  (sanityinc/increment-default-font-height 10)
  (if (fboundp 'powerline-reset)
      (powerline-reset)))

(defun sanityinc/decrease-default-font-height ()
  (interactive)
  (sanityinc/increment-default-font-height -10)
  (if (fboundp 'powerline-reset)
      (powerline-reset)))

(bind-key (kbd "C-=") 'sanityinc/increase-default-font-height)
(bind-key (kbd "C--") 'sanityinc/decrease-default-font-height)

(let ((font (if (= emacs-major-version 25)
                "Symbola"
              (cond ((string-equal system-type "darwin")    "Apple Color Emoji")
                    ((string-equal system-type "gnu/linux") "Symbola")))))
  (set-fontset-font t 'unicode font nil 'prepend))

;; Turning off ad-handle-definition the warnings
(setq ad-redefinition-action 'accept)

;; These functions are useful. Activate them.
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)

;; Answering just 'y' or 'n' will do
(defalias 'yes-or-no-p 'y-or-n-p)

;; Keep all backup and auto-save files in one directory
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/autosave/" t)))

;; UTF-8 please
(setq locale-coding-system 'utf-8) ; pretty
(set-terminal-coding-system 'utf-8) ; pretty
(set-keyboard-coding-system 'utf-8) ; pretty
(set-selection-coding-system 'utf-8) ; please
(prefer-coding-system 'utf-8) ; with sugar on top

;; Turn off the blinking cursor
(blink-cursor-mode -1)

;; (setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))
(setq-default left-fringe-width nil)
(setq-default indent-tabs-mode nil)
(setq-default indicate-empty-lines t)
(setq org-src-fontify-natively t)

;; Why did I do this? Perhaps to keep vc from meddling with things
;; that Magit does, but it's convenient to be able to lean on vc for
;; certain things, so let's try it again with this turned on.
;; (eval-after-load "vc" '(setq vc-handled-backends nil))

(setq vc-follow-symlinks t)
(setq large-file-warning-threshold nil)
(setq split-width-threshold nil)
(setq custom-safe-themes t)
(setq exec-path-from-shell-check-startup-files nil)

;; Don't count two spaces after a period as the end of a sentence.
;; Just one space is needed.
(setq sentence-end-double-space nil)

;; delete the region when typing, just like as we expect nowadays.
(delete-selection-mode t)

(show-paren-mode t)

(column-number-mode t)

;; (global-visual-line-mode)
;; (diminish 'visual-line-mode)

(setq uniquify-buffer-name-style 'forward)

;; -i gets alias definitions from .bash_profile
(setq shell-command-switch "-ic")

;; Don't beep at me
(setq visible-bell t)

(defun occur-dwim ()
  "Call `occur' with a sane default."
  (interactive)
  (push (if (region-active-p)
            (buffer-substring-no-properties
             (region-beginning)
             (region-end))
          (thing-at-point 'symbol))
        regexp-history)
  (call-interactively 'occur))

(bind-key "M-s o" 'occur-dwim)

(use-package page-break-lines
  :ensure t)

(when (string-equal system-type "darwin")
  ;; delete files by moving them to the trash
  (setq delete-by-moving-to-trash t)
  (setq trash-directory "~/.Trash")

  ;; Don't make new frames when opening a new file with Emacs
  (setq ns-pop-up-frames nil)

  ;; set the Fn key as the hyper key
  (setq ns-function-modifier 'hyper)

  ;; Use Command-` to switch between Emacs windows (not frames)
  (bind-key "s-`" 'other-window)
  
  ;; Use Command-Shift-` to switch Emacs frames in reverse
  (bind-key "s-~" (lambda() () (interactive) (other-window -1)))

  ;; Because of the keybindings above, set one for `other-frame'
  (bind-key "s-1" 'other-frame)

  ;; Fullscreen!
  (setq ns-use-native-fullscreen nil) ; Not Lion style
  (bind-key "<s-return>" 'toggle-frame-fullscreen)

  ;; buffer switching
  (bind-key "s-{" 'previous-buffer)
  (bind-key "s-}" 'next-buffer)

  ;; Compiling
  (bind-key "H-c" 'compile)
  (bind-key "H-r" 'recompile)
  (bind-key "H-s" (defun save-and-recompile () (interactive) (save-buffer) (recompile)))

  ;; disable the key that minimizes emacs to the dock because I don't
  ;; minimize my windows
  ;; (global-unset-key (kbd "C-z"))

  (defun open-dir-in-finder ()
    "Open a new Finder window to the path of the current buffer"
    (interactive)
    (start-process "mai-open-dir-process" nil "open" "."))
  (bind-key "C-c o f" 'open-dir-in-finder)

  (defun open-dir-in-iterm ()
    "Open the current directory of the buffer in iTerm."
    (interactive)
    (let* ((iterm-app-path "/Applications/iTerm.app")
           (iterm-brew-path "/opt/homebrew-cask/Caskroom/iterm2/1.0.0/iTerm.app")
           (iterm-path (if (file-directory-p iterm-app-path)
                           iterm-app-path
                         iterm-brew-path)))
      (start-process "mai-open-dir-process" nil "open" "-a" iterm-path ".")))
  (bind-key "C-c o t" 'open-dir-in-iterm)

  ;; Not going to use these commands
  (put 'ns-print-buffer 'disabled t)
  (put 'suspend-frame 'disabled t))

(let* ((cmd "sw_vers -productVersion")
       (macos-version (string-to-int
                     (cadr (split-string
                            (shell-command-to-string cmd)
                            "\\."))))
       (elcapitan-version 11))
  (when (>= macos-version elcapitan-version)
    (setq visible-bell nil)
    (setq ring-bell-function 'ignore)

    ;; El Capitan full screen animation is quick and delightful (enough to start using it).
    (setq ns-use-native-fullscreen t)))

;; make ibuffer the default buffer lister.
(defalias 'list-buffers 'ibuffer)

(add-hook 'dired-mode-hook 'auto-revert-mode)

;; Also auto refresh dired, but be quiet about it
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

(use-package recentf
  :bind ("C-x C-r" . helm-recentf)
  :config
  (recentf-mode t)
  (setq recentf-max-saved-items 200))

(use-package org
  :ensure org-plus-contrib)

(bind-key "C-c l" 'org-store-link)
(bind-key "C-c c" 'org-capture)
(bind-key "C-c a" 'org-agenda)

(setq org-agenda-files
      (delq nil
            (mapcar (lambda (x) (and (file-exists-p x) x))
                    '("~/Dropbox/Agenda"))))

(bind-key "C-c c" 'org-capture)
(setq org-default-notes-file "~/Dropbox/Notes/notes.org")

(setq org-use-speed-commands t)

(setq org-image-actual-width 550)

(setq org-highlight-latex-and-related '(latex script entities))

(setq org-tags-column 45)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)
   (C . t)
   (calc . t)
   (latex . t)
   (java . t)
   (ruby . t)
   (lisp . t)
   (scheme . t)
   (shell . t)
   (sqlite . t)
   (js . t)))

(defun my-org-confirm-babel-evaluate (lang body)
  "Do not confirm evaluation for these languages."
  (not (or (string= lang "C")
           (string= lang "java")
           (string= lang "python")
           (string= lang "emacs-lisp")
           (string= lang "sqlite"))))
(setq org-confirm-babel-evaluate 'my-org-confirm-babel-evaluate)

(setq org-src-fontify-natively t
      org-src-window-setup 'current-window
      org-src-strip-leading-and-trailing-blank-lines t
      org-src-preserve-indentation t
      org-src-tab-acts-natively t)

(bind-key "s-C-<left>"  'shrink-window-horizontally)
(bind-key "s-C-<right>" 'enlarge-window-horizontally)
(bind-key "s-C-<down>"  'shrink-window)
(bind-key "s-C-<up>"    'enlarge-window)

(defun vsplit-other-window ()
  "Splits the window vertically and switches to that window."
  (interactive)
  (split-window-vertically)
  (other-window 1 nil))
(defun hsplit-other-window ()
  "Splits the window horizontally and switches to that window."
  (interactive)
  (split-window-horizontally)
  (other-window 1 nil))

(bind-key "C-x 2" 'vsplit-other-window)
(bind-key "C-x 3" 'hsplit-other-window)

(use-package winner
  :config
  (winner-mode t)
  :bind (("M-s-<left>" . winner-undo)
         ("M-s-<right>" . winner-redo)))

(use-package transpose-frame
  :ensure t
  :bind ("H-t" . transpose-frame))

(use-package ido
  :init
  (setq ido-enable-flex-matching t)
  (setq ido-everywhere t)
  (ido-mode t)
  (use-package ido-vertical-mode
    :ensure t
    :defer t
    :init (ido-vertical-mode 1)
    (setq ido-vertical-define-keys 'C-n-and-C-p-only)))

(use-package whitespace
  :bind ("s-<f10>" . whitespace-mode))

(use-package ng2-mode :ensure t)

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

(use-package ansible
  :ensure t
  :config
  (setq ansible::vault-password-file "/Users/light/.config/ansible/vault_pass.txt")
  (add-hook 'yaml-mode-hook '(lambda () (ansible 1)))
  (add-hook 'ansible-hook 'ansible::auto-decrypt-encrypt))

(use-package company
  :ensure t
  :defer t
  :config
  (global-company-mode)
  ; (setq company-tooltip-common-selection ((t (:inherit company-tooltip-selection :background "yellow2" :foreground "#c82829"))))
  ; (setq company-tooltip-selection ((t (:background "yellow2"))))
  (setq company-idle-delay 0.2)
  (setq company-selection-wrap-around t)
  ;; aligns annotation to the right hand side
  (setq company-tooltip-align-annotations t)
  (define-key company-active-map [tab] 'company-complete)
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous))

(use-package company-jedi
  :ensure t
  :config
  (add-hook 'python-mode-hook 'jedi:setup)
  (setq jedi:complete-on-dot t))

(use-package dash-at-point
  :ensure t
  :bind (("s-D"     . dash-at-point)
         ("C-c e"   . dash-at-point-with-docset)))

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

(use-package emmet-mode
  :ensure t
  :config
  (add-hook 'web-mode-hook 'emmet-mode)
  (add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
  (add-hook 'css-mode-hook  'emmet-mode) ;; enable Emmet's css abbreviation.
  (setq emmet-move-cursor-between-quotes t) ;; default nil
  )

(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-env "PYTHONPATH")
  (setq exec-path-from-shell-check-startup-files nil)
  )

(use-package flycheck
  :ensure t
  :config
  (add-hook 'after-init-hook 'global-flycheck-mode)

  ;; Flycheck mode:
  (add-hook 'flycheck-mode-hook
            (lambda ()
              (when (maybe-require-package 'evil)
                (evil-define-key 'normal flycheck-mode-map (kbd "]e") 'flycheck-next-error)
                (evil-define-key 'normal flycheck-mode-map (kbd "[e") 'flycheck-previous-error))
              (when (maybe-require-package 'evil-leader)
                (evil-leader/set-key (kbd "E") 'flycheck-list-errors))))

  ;; Override default flycheck triggers
  (setq flycheck-emacs-lisp-load-path 'inherit
        flycheck-check-syntax-automatically '(save idle-change mode-enabled)
        flycheck-idle-change-delay 0.8
        flycheck-disabled-checkers '(php-phpmd)
        flycheck-phpcs-standard "CSNStores")

  (setq flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-list))

(use-package helm
  :ensure t
  :diminish helm-mode
  :init (progn
          (require 'helm-config)
          (use-package helm-projectile
            :ensure t
            :commands (helm-projectile helm-projectile-switch-project))
          (use-package helm-ag
            :defer 10
            :ensure t
            :commands (helm-do-grep-ag))
          (setq helm-locate-command "mdfind -interpret -name %s %s"
                helm-ff-newfile-prompt-p nil
                helm-M-x-fuzzy-match t)
          (helm-mode))
  :config
    (helm-mode 1)
    (setq helm-buffers-fuzzy-matching t)
    (setq helm-autoresize-mode t)
    (setq helm-buffer-max-length 40)
    ;; (setq helm-split-window-in-side-p t)
    (define-key helm-map (kbd "S-SPC") 'helm-toggle-visible-mark)
    (define-key helm-map (kbd "C-j") 'helm-next-line)
    (define-key helm-map (kbd "C-k") 'helm-previous-line))

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

(use-package all-the-icons :ensure t)

(use-package neotree
  :ensure t
  :config
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
  (add-hook 'neotree-mode-hook
    (lambda ()
      (define-key evil-normal-state-local-map (kbd "q") 'neotree-hide)
      (define-key evil-normal-state-local-map (kbd "I") 'neotree-hidden-file-toggle)
      (define-key evil-normal-state-local-map (kbd "z") 'neotree-stretch-toggle)
      (define-key evil-normal-state-local-map (kbd "R") 'neotree-refresh)
      (define-key evil-normal-state-local-map (kbd "m") 'neotree-rename-node)
      (define-key evil-normal-state-local-map (kbd "c") 'neotree-create-node)
      (define-key evil-normal-state-local-map (kbd "d") 'neotree-delete-node)

      (define-key evil-normal-state-local-map (kbd "s") 'neotree-enter-vertical-split)
      (define-key evil-normal-state-local-map (kbd "S") 'neotree-enter-horizontal-split)

      (define-key evil-normal-state-local-map (kbd "RET") 'neotree-enter))))

(use-package find-file-in-project :ensure t)
(defun neotree-project-dir ()
  "Open NeoTree using the git root."
  (interactive)
  (let ((project-dir (ffip-project-root))
        (file-name (buffer-file-name)))
    (if project-dir
        (progn
        (neotree-dir project-dir)
        (neotree-find file-name))
    (message "Could not find git project root."))))

(provide 'init-neotree)

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

(use-package projectile
  :ensure t
  :defer 1
  :config
  (projectile-mode)
  (setq projectile-enable-caching t))

(use-package s
  :ensure t
  :defer 1)

(use-package scratch
  :ensure t
  :commands scratch)

(use-package smooth-scrolling
  :ensure t)

(use-package vimrc-mode :ensure t :defer t)
(add-to-list 'auto-mode-alist '("\\.vim\\(rc\\)?\\'" . vimrc-mode))

(use-package virtualenvwrapper
  :ensure t
  :config
  (venv-initialize-interactive-shells)
  (venv-initialize-eshell)
  (setq venv-location
        (expand-file-name "~/.virtualenvs/")))

(use-package visual-fill-column :ensure t)

(use-package web-mode
  :ensure t
  :config
    (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.php?\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.css?\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.scss?\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.js?\\'" . web-mode))
    (setq web-mode-engines-alist
        '(("django"    . "\\.html\\'")
            ("php"    . "\\.php\\'"))
    )
    (setq web-mode-ac-sources-alist
        '(("css" . (ac-source-css-property))
            ("html" . (ac-source-words-in-buffer ac-source-abbrev))))
    (setq web-mode-content-types-alist
        '(("jsx" . "\\.js[x]?\\'")))
  )

(eval-after-load "web-mode"
  '(set-face-underline 'web-mode-current-element-highlight-face t))

(use-package yaml-mode :ensure t :defer t)

(use-package yasnippet
  :ensure t
  :defer t
  :diminish yas-minor-mode
  :config
  (yas-global-mode)
  (yas-reload-all))

(require 'subr-x) ;; #'string-trim
(defvar mai/user-settings-dir nil
  "The directory with user-specific Emacs settings for this
  user.")

;; Settings for currently logged in user
(setq mai/user-settings-dir
      (concat user-emacs-directory
              "users/"
              (string-trim (shell-command-to-string "hostname -s"))))
(add-to-list 'load-path mai/user-settings-dir)

;; Load settings specific for the current user
(when (file-exists-p mai/user-settings-dir)
  (mapc 'load (directory-files mai/user-settings-dir nil "^[^#].*el$")))

(require 'epc)
(when noninteractive
  (load "subr")
  (load "byte-run"))
(eval-when-compile (require 'cl))

(message "Start EPC")

(defvar my-epc-server-py
  (expand-file-name "my-server.py"
                    (file-name-directory
                     (or load-file-name buffer-file-name))))

(defvar my-epc (epc:start-epc (or (getenv "PYTHON") "python")
                              (list my-epc-server-py)))

(message "Start request")

(deferred:$
  (epc:call-deferred my-epc 'echo '(10))
  (deferred:nextc it
    (lambda (x) (message "Return : %S" x))))


(message "Return : %S" (epc:call-sync my-epc 'echo '(10 40)))

(loop for i from 1 to 5
      do (deferred:$
           (epc:call-deferred my-epc 'echo (list i))
           (deferred:nextc it
             (lambda (x) (message "Return : %S" x)))))

(message "Return : %S"
         (epc:sync my-epc (epc:query-methods-deferred my-epc)))

;;; init-tdd.el --- run tests on save and indicate success in the mode line

;; Copyright (C) 2014  Jorgen Schaefer <contact@jorgenschaefer.de>

;; Author: Jorgen Schaefer <contact@jorgenschaefer.de>
;; URL: https://github.com/jorgenschaefer/emacs-tdd
;; Version: 1.0
;; Keywords: tools, processes

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; After enabling `tdd-mode', any command to save a file will run
;; `recompile' (or a customisable function) in the background. The
;; mode line shows the status of the last compilation process.

;; This is meant to be used with test-driven development:

;; - Write a test and save the file
;; - Watch the test fail as the status line indicator turns red
;; - Write code and save the file until the status line turns green
;; - Repeat

;;; Code:

(require 'compile)

(defgroup tdd nil
  "Test-Driven Development Indicator."
  :prefix "tdd-"
  :group 'productivity)

(defvar tdd-mode-line-map (let ((map (make-sparse-keymap)))
                            (define-key map [mode-line mouse-1]
                              'tdd-display-buffer)
                            map)
  "Keymap used on the mode line indicator.")

(defcustom tdd-test-function #'recompile
  "Test function to run.
It will be run without arguments, whenever a buffer is saved. It
should run in compilation major mode, because checking for
success or failure depends the mode hooks.
The default is (recompile)"
  :type 'function
  :group 'tdd)

(defcustom tdd-success-symbol "✔"
  "Mode line symbol to show when tests passed."
  :type 'string
  :group 'tdd)

(defcustom tdd-success-face 'compilation-mode-line-exit
  "Face to use for `tdd-success-symbol'."
  :type 'face
  :group 'tdd)

(defcustom tdd-failure-symbol "✖"
  "Mode line symbol to show when tests failed."
  :type 'string
  :group 'tdd)

(defcustom tdd-failure-face 'compilation-mode-line-fail
  "Face to use for `tdd-failure-symbol'."
  :type 'face
  :group 'tdd)

(defcustom tdd-waiting-symbol "✱"
  "Mode line symbol to show when tests are running."
  :type 'string
  :group 'tdd)

(defcustom tdd-waiting-face 'compilation-mode-line-run
  "Face to use for `tdd-waiting-symbol'."
  :type 'face
  :group 'tdd)

(defvar tdd-mode-line-format ""
  "The mode line entry for the TDD indicator.")
(put 'tdd-mode-line-format 'risky-local-variable
     'do-show-properties-in-mode-line)

(defvar tdd-compilation-in-progress nil
  "Non-nil if we already started a compilation process.
Sadly, `get-buffer-process' does not work for preventing
duplicate compilation runs.")

;;;###autoload
(define-minor-mode tdd-mode
  "Test-driven development global minor mode.
Runs `tdd-test-function' every time a buffer is saved, and
adjusts a mode line indicator depending on the success or failure
of that compilation command."
  :global t
  (cond
   (tdd-mode
    (tdd-add-mode-line-format)
    (tdd-success)
    (add-hook 'compilation-finish-functions 'tdd-compilation-finish)
    (add-hook 'compilation-start-hook 'tdd-compilation-start)
    (add-hook 'after-save-hook 'tdd-after-save))
   (t
    (tdd-remove-mode-line-format)
    (setq tdd-mode-line-format "")
    (remove-hook 'compilation-finish-functions 'tdd-compilation-finish)
    (remove-hook 'compilation-start-hook 'tdd-compilation-start)
    (remove-hook 'after-save-hook 'tdd-after-save))))

(defun tdd-success ()
  "Set the TDD indicator to green."
  (interactive)
  (setq tdd-mode-line-format
        (propertize tdd-success-symbol
                    'face tdd-success-face
                    'keymap tdd-mode-line-map
                    'mouse-face 'mode-line-highlight
                    'help-echo (concat "Tests succeeded\n"
                                       "mouse-1: Switch to test buffer"))))

(defun tdd-failure ()
  "Set the TDD indicator to red."
  (interactive)
  (setq tdd-mode-line-format
        (propertize tdd-failure-symbol
                    'face tdd-failure-face
                    'keymap tdd-mode-line-map
                    'mouse-face 'mode-line-highlight
                    'help-echo (concat "Tests running\n"
                                       "mouse-1: Switch to test buffer"))))

(defun tdd-waiting ()
  "Set the TDD indicator to mark an ongoing compilation run."
  (interactive)
  (setq tdd-mode-line-format
        (propertize tdd-waiting-symbol
                    'face tdd-waiting-face
                    'keymap tdd-mode-line-map
                    'mouse-face 'mode-line-highlight
                    'help-echo (concat "Tests failed\n"
                                       "mouse-1: Switch to test buffer"))))

(defun tdd-display-buffer ()
  "Display the compilation buffer."
  (interactive)
  (let ((compilation-buffer (get-buffer
                             (compilation-buffer-name "compilation"
                                                      nil nil))))
    (when compilation-buffer
      (display-buffer compilation-buffer))))

(defun tdd-add-mode-line-format ()
  "Add `tdd-mode-line-format' to `mode-line-format'."
  (let ((global-mode-line (default-value 'mode-line-format)))
    (when (not (memq 'tdd-mode-line-format global-mode-line))
      (setq-default mode-line-format
                    (cons (car global-mode-line)
                          (cons 'tdd-mode-line-format
                                (cdr global-mode-line)))))))

(defun tdd-remove-mode-line-format ()
  "Add `tdd-mode-line-format' to `mode-line-format'."
  (let ((global-mode-line (default-value 'mode-line-format)))
    (when (memq 'tdd-mode-line-format global-mode-line)
      (setq-default mode-line-format
                    (delq 'tdd-mode-line-format
                          global-mode-line)))))

(defun tdd-after-save ()
  "Function run in `after-save-hook' to start the compilation."
  (when (not tdd-compilation-in-progress)
    (setq tdd-compilation-in-progress t)
    (let ((compilation-ask-about-save nil)
          (compilation-save-buffers-predicate (lambda () nil)))
      (save-window-excursion
        (funcall tdd-test-function)))))

(defun tdd-compilation-start (proc)
  "Function run from `compilation-start-hook'."
  (setq tdd-compilation-in-progress t)
  (tdd-waiting))

(defun tdd-compilation-finish (buf msg)
  "Function run from `compilation-finish-functions'."
  (setq tdd-compilation-in-progress nil)
  (if (string-match "exited abnormally" msg)
      (tdd-failure)
    (tdd-success)))

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

;;; python-test.el --- Python testing integration     -*- lexical-binding: t -*-

;; Copyright (c) 2015 Mario Rodas <marsam@users.noreply.github.com>

;; Author: Mario Rodas <marsam@users.noreply.github.com>
;; URL: https://github.com/emacs-pe/python-test.el
;; Keywords: convenience, tools, processes
;; Version: 0.1
;; Package-Requires: ((emacs "25.1"))

;; This file is NOT part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; `python-test.el' allows the execution of python tests from Emacs.
;;
;; Setup:
;;
;; Python objects:
;;
;; | objects      | function                 |
;; | -------------|--------------------------|
;; | class        | `python-test-class'      |
;; | method       | `python-test-method'     |
;; | function     | `python-test-function'   |
;; | file         | `python-test-file'       |
;; | project      | `python-test-project'    |
;;
;; Test frameworks:
;;
;; + [nose][]
;; + [pytest][]
;; + [unittest][]: Needs the command-line interface available since python >=2.7
;;
;; [nose]: https://nose.readthedocs.org/
;; [pytest]: https://pytest.org/
;; [unittest]: https://docs.python.org/library/unittest.html "Unit testing framework"

;;; Code:
(eval-when-compile
  (require 'cl-lib)
  (require 'cl-generic))

(require 'python)
(require 'compile)

(defgroup python-test nil
  "Python testing integration"
  :prefix "python-test-"
  :group 'compilation)

(defcustom python-test-reuse-buffer t
  "Whether to reuse python test buffer."
  :type 'boolean
  :safe #'booleanp
  :group 'python-test)

(defcustom python-test-disable-warnings nil
  "Whether to disable warnings."
  :type 'boolean
  :safe #'booleanp
  :group 'python-test)

(defcustom python-test-command nil
  "Command to execute as python test."
  :type 'string
  :group 'python-test)
;;;###autoload(put 'python-test-command 'safe-local-variable (lambda (a) (and (stringp a) (or (not (boundp 'compilation-read-command)) compilation-read-command))))

(defcustom python-test-project-root nil
  "Root of a python project."
  :type 'string
  :safe #'stringp
  :group 'python-test)

(defcustom python-test-project-root-files
  '("setup.py"                          ; Setuptools file
    "setup.cfg"                         ; Setuptools file
    "tox.ini"                           ; Tox file
    "pytest.ini"                        ; Py.test file
    "requirements.txt"                  ; Pip file
    )
  "A list of files considered to mark the root of a project.
The topmost match has precedence."
  :type '(repeat string)
  :group 'python-test)


;; Backends
(defvar python-test-backends nil)

(defcustom python-test-backend 'unittest
  "Default backend for `python-test'."
  :type (append '(choice)
                (mapcar (lambda (x) (list 'const x)) python-test-backends))
  :safe #'python-test-registered-backend-p
  :group 'python-test)

(defvar python-test-command-history nil)


;; Faces
(defface python-test-description
  '((t :inherit warning))
  "Face for python-test error description lines."
  :group 'python-test)

(defface python-test-error
  '((t :inherit error))
  "Face for python-test error lines."
  :group 'python-test)

(defface python-test-info
  '((t :inherit success))
  "Face for test information."
  :group 'python-test)


(eval-and-compile
  (defconst python-test-rx-constituents
    `((defun       . ,(rx symbol-start "def" symbol-end))
      (class       . ,(rx symbol-start "class" symbol-end))
      (symbol-name . ,(rx (any letter ?_) (* (any word ?_)))))
    "Additional specific sexps for `python-test-rx'")

  (defmacro python-test-rx (&rest regexps)
    "Python Test specialized rx macro."
    (let ((rx-constituents (append python-test-rx-constituents rx-constituents)))
      (cond ((null regexps)
             (error "No regexp"))
            ((cdr regexps)
             (rx-to-string `(and ,@regexps) t))
            (t
             (rx-to-string (car regexps) t))))))

(defconst python-test-beg-defun-regexp
  (python-test-rx line-start (* space) defun (+ space) (group symbol-name))
  "Regexp for python function definition.")

(defconst python-test-beg-class-regexp
  (python-test-rx line-start (* space) class (+ space) (group symbol-name))
  "Regexp for python class definition.")


;;; Internal functions
(defun python-test-registered-backend-p (backend)
  "Determine whether `org-sync' BACKEND is registered."
  (memq backend python-test-backends))

(defun python-test-as-symbol (string-or-symbol)
  "If STRING-OR-SYMBOL is already a symbol, return it.  Otherwise convert it to a symbol and return that."
  (if (symbolp string-or-symbol) string-or-symbol (intern string-or-symbol)))

(defun python-test-read-command (command)
  "Read python test COMMAND."
  (read-shell-command "Test command: " command
                      (if (equal (car python-test-command-history) command)
                          '(python-test-command-history . 1)
                        'python-test-command-history)))

(defun python-test-internal-capture ()
  "Capture python function internal.
It predates `python-nav-beginning-of-defun-regexp' to search a function definition."
  (save-excursion
    (when (or (python-info-looking-at-beginning-of-defun)
              (python-nav-beginning-of-defun 1))
      (re-search-forward python-nav-beginning-of-defun-regexp (point-at-eol) t)
      (match-string-no-properties 1))))

(defun python-test-capture-defun ()
  "Capture python function."
  (let ((python-nav-beginning-of-defun-regexp python-test-beg-defun-regexp))
    (or (python-test-internal-capture) (error "Python function name not found"))))

(defun python-test-capture-class ()
  "Capture python function."
  (let ((python-nav-beginning-of-defun-regexp python-test-beg-class-regexp))
    (or (python-test-internal-capture) (error "Python class name not found"))))

(defun python-test-locate-root-file (directory)
  "Locate project-root using from DIRECTORY."
  (cl-loop for file in python-test-project-root-files
           when (locate-dominating-file directory file)
           return it))

(defun python-test-project-root ()
  "Calculate project root."
  (or python-test-project-root
      (python-test-locate-root-file default-directory)))

(defun python-test-read-backend ()
  "Read the backend which will be used."
  (list (or (and (not current-prefix-arg) python-test-backend)
            (completing-read "Backend: " python-test-backends nil t nil))))

(defun python-test-capture-path (&optional project-root)
  "Return current file relative from PROJECT-ROOT."
  (if buffer-file-name
      (file-relative-name buffer-file-name project-root)
    (user-error "Not available here")))

(defun python-test-quote-command (command &rest args)
  "Quote COMMAND and ARGS."
  (mapconcat #'shell-quote-argument (cons command args) " "))

(defun python-test-resolve-executable (value)
  "Resolve executable VALUE."
  (cl-etypecase value
    (stringp value)
    (symbolp (symbol-value value))))

(defun python-test-path-module (file)
  "Convert a FILE path to a python module."
  (subst-char-in-string ?/ ?. (file-name-sans-extension file)))

(defun python-test-context-command (backend context)
  "Return BACKEND command from CONTEXT."
  (cl-assert (python-test-registered-backend-p backend) nil "Unregistered python-test backend: %s" backend)
  (let ((args (cl-ecase context
                (project   (python-test-args-project backend))
                (file      (python-test-args-file    backend))
                (class     (python-test-args-class   backend))
                (method    (python-test-args-method  backend))
                (defun     (python-test-args-defun   backend))))
        (executable (python-test-resolve-executable (python-test-executable backend))))
    (apply #'python-test-quote-command executable args)))

(defmacro python-test-with-project-root (&rest body)
  "Execute BODY in python project root."
  (declare (indent defun) (debug (body)))
  (let ((project-root (cl-gensym "project-root")))
    `(let ((,project-root (python-test-project-root)))
       (let ((default-directory (if ,project-root
                                    (file-name-as-directory ,project-root)
                                  (or python-test-disable-warnings (lwarn 'python-test :warning "Could not locate python project root: %s" default-directory))
                                  default-directory)))
         ,@body))))

(defmacro python-test-with-environment (&rest body)
  "Modify shell environment for python support of BODY."
  (declare (indent 0) (debug (body)))
  (if (fboundp 'python-shell-with-environment)
      `(python-shell-with-environment ,@body)
    `(let ((process-environment (python-shell-calculate-process-environment))
           (exec-path (python-shell-calculate-exec-path)))
       ,@body)))

;;; Backends
(cl-defgeneric python-test-executable (backend)
  "Backend args to test a project.")

(cl-defgeneric python-test-args-project (backend)
  "Backend args to test a project.")

(cl-defgeneric python-test-args-file (backend)
  "Backend args to test a file.")

(cl-defgeneric python-test-args-class (backend)
  "Backend args to test a class.")

(cl-defgeneric python-test-args-method (backend)
  "Backend args to test a class method.")

(cl-defgeneric python-test-args-defun (backend)
  "Backend args to test a function.")

;; unittest
(add-to-list 'python-test-backends 'unittest)

(cl-defmethod python-test-executable ((_backend (eql unittest)))
  "Python unitest executable is python itself, given that unittest is executed as module."
  python-shell-interpreter)

(cl-defmethod python-test-args-project ((_backend (eql unittest)))
  (list "-m" "unittest" "discover"))

(cl-defmethod python-test-args-file ((_backend (eql unittest)))
  (list "-m" "unitest" (python-test-path-module (python-test-capture-path))))

(cl-defmethod python-test-args-class ((_backend (eql unittest)))
  (list "-m" "unitest" (format "%s.%s"
                               (python-test-path-module (python-test-capture-path))
                               (python-test-capture-class))))

(cl-defmethod python-test-args-method ((_backend (eql unittest)))
  (list "-m" "unitest" (format "%s.%s.%s"
                               (python-test-path-module (python-test-capture-path))
                               (python-test-capture-class)
                               (python-test-capture-defun))))

(cl-defmethod python-test-args-defun ((_backend (eql unittest)))
  (user-error "Python unittest doesn't support testing functions"))


;; py.test
(add-to-list 'python-test-backends 'pytest)

(cl-defmethod python-test-executable ((_backend (eql pytest)))
  "Py.test executable name."
  "py.test")

(cl-defmethod python-test-args-project ((_backend (eql pytest)))
  (list))

(cl-defmethod python-test-args-file ((_backend (eql pytest)))
  (list (python-test-capture-path)))

(cl-defmethod python-test-args-class ((_backend (eql pytest)))
  (list (format "%s::%s" (python-test-capture-path) (python-test-capture-class))))

(cl-defmethod python-test-args-method ((_backend (eql pytest)))
  (list (format "%s::%s::%s" (python-test-capture-path) (python-test-capture-class) (python-test-capture-defun))))

(cl-defmethod python-test-args-defun ((_backend (eql pytest)))
  (list (format "%s::%s" (python-test-capture-path) (python-test-capture-defun))))


;; nosetests
(add-to-list 'python-test-backends 'nose)

(cl-defmethod python-test-executable ((_backend (eql nose)))
  "Nosetests executable."
  "nosetests")

(cl-defmethod python-test-args-project ((_backend (eql nose)))
  (list))

(cl-defmethod python-test-args-file ((_backend (eql nose)))
  (list (python-test-capture-path)))

(cl-defmethod python-test-args-class ((_backend (eql nose)))
  (list (format "%s:%s" (python-test-capture-path) (python-test-capture-class))))

(cl-defmethod python-test-args-method ((_backend (eql nose)))
  (list (format "%s:%s.%s" (python-test-capture-path) (python-test-capture-class) (python-test-capture-defun))))

(cl-defmethod python-test-args-defun ((_backend (eql nose)))
  (list (format "%s:%s" (python-test-capture-path) (python-test-capture-defun))))


;;; python-test-mode
(defvar python-test-compilation-regexp-alist-alist
  `((python-tracebacks
     ;; XXX: "INTERNALERROR> " also make py.test error constituent errors
     ,(rx line-start (? "INTERNALERROR>") (1+ (any " \t")) "File \""
          (group (1+ (not (any "\"<")))) ; avoid `<stdin>' &c
          "\", line " (group (1+ digit)))
     1 2)
    (pytest
     ,(rx line-start (group (+ any)) ":" (group (1+ digit)) ": " (? (group (+ not-newline))))
     1 2 3))
  "Specifications for matching errors in py.test invocations.
See `compilation-error-regexp-alist' for help on their format.")

(defvar python-test-compilation-regexp-alist (mapcar 'car python-test-compilation-regexp-alist-alist))

(defvar python-test-mode-font-lock-keywords
  `( ;; py.test error traces
    (,(rx line-start ">" (+ space) (+ not-newline) line-end)
     (0 'python-test-description))
    (,(rx line-start "E" (+ space) (+ not-newline) line-end)
     (0 'python-test-error))
    ;; py.test information separators
    (,(rx line-start (+ (in "-" "!" "=" "_")) (? space (+ not-newline) space) (+ (in "-" "!" "=" "_")) line-end)
     (0 'python-test-info))
    ;; nosetest information
    (,(rx line-start "Ran " (+ num) " tests in " (+ not-newline) line-end)
     (0 'python-test-info))
    (,(rx line-start (or "OK" (and "FAILED (failures=" (+ num) ")")) line-end)
     (0 'python-test-info))))

(defun python-test-ansi-color-filter ()
  "Handle match highlighting escape sequences.
This function is called from `compilation-filter-hook'."
  (ansi-color-apply-on-region compilation-filter-start (point)))

;; `python.el' variables introduced in Emacs 25.1
(defvar python-shell--interpreter)
(defvar python-shell--interpreter-args)

;;;###autoload
(defun python-test-track-pdb-prompt ()
  "Change compilation to `python-inferior-mode' when a pdb prompt is detected.
This function is a hack that enables `inferior-python-mode' when
a pdb prompt is detected in `compilation-mode' buffers, and to
work is meant to be added to `compilation-filter-hook'.  To go
back to `compilation-mode' you need to call
\\[python-test-back-to-compilation]."
  (let ((output (ignore-errors (buffer-substring-no-properties compilation-filter-start (point)))))
    (when (and output (string-match-p python-shell-prompt-pdb-regexp output))
      (message "Entering pdb...")
      (setq buffer-read-only nil)
      (let ((python-shell--interpreter nil)
            (python-shell--interpreter-args nil))
        (set-process-filter (get-buffer-process (current-buffer)) 'comint-output-filter)
        (inferior-python-mode)
        (run-hook-with-args 'comint-output-filter-functions output)))))

(defun python-test-back-to-compilation ()
  "Go back to compilation mode.
See `python-test-track-pdb-prompt' documentation for more
information."
  (interactive)
  (let ((process (get-buffer-process (current-buffer))))
    (when process
      (message "Enabling compilation mode... ")
      (set-process-filter process 'compilation-filter)
      (compilation-mode))))

(define-compilation-mode python-test-mode "python-test"
  "Python test results compilation mode"
  (setq-local compilation-mode-font-lock-keywords
              python-test-mode-font-lock-keywords)
  (setq-local compilation-error-regexp-alist-alist
              python-test-compilation-regexp-alist-alist)
  (setq-local compilation-error-regexp-alist
              python-test-compilation-regexp-alist)
  (add-hook 'compilation-filter-hook 'python-test-track-pdb-prompt t t)
  (add-hook 'compilation-filter-hook 'python-test-ansi-color-filter nil t))

(define-key python-test-mode-map (kbd "p") #'compilation-previous-error)
(define-key python-test-mode-map (kbd "n") #'compilation-next-error)


;;;###autoload
(defun python-test-project (backend)
  "Execute python project test with BACKEND."
  (interactive (python-test-read-backend))
  (python-test-with-project-root
    (python-test (python-test-context-command (python-test-as-symbol backend) 'project))))

;;;###autoload
(defun python-test-file (backend)
  "Execute python file test with BACKEND."
  (interactive (python-test-read-backend))
  (python-test-with-project-root
    (python-test (python-test-context-command (python-test-as-symbol backend) 'file))))

;;;###autoload
(defun python-test-class (backend)
  "Execute python class test with BACKEND."
  (interactive (python-test-read-backend))
  (python-test-with-project-root
    (python-test (python-test-context-command (python-test-as-symbol backend) 'class))))

;;;###autoload
(defun python-test-method (backend)
  "Execute python method test with BACKEND."
  (interactive (python-test-read-backend))
  (python-test-with-project-root
    (python-test (python-test-context-command (python-test-as-symbol backend) 'method))))

;;;###autoload
(defun python-test-function (backend)
  "Execute python function test with BACKEND."
  (interactive (python-test-read-backend))
  (python-test-with-project-root
    (python-test (python-test-context-command (python-test-as-symbol backend) 'defun))))

;;;###autoload
(defun python-test (command)
  "Execute COMMAND with python test."
  (interactive (list (let ((command (eval python-test-command)))
                       (if (or compilation-read-command current-prefix-arg)
                           (python-test-read-command command)
                         command))))
  (python-test-with-environment
    (save-some-buffers (not compilation-ask-about-save) nil)
    (compilation-start command #'python-test-mode
                       (lambda (mode)
                         (if python-test-reuse-buffer
                             (format "*%s*" (downcase mode))
                           (format "*%s: %s*" (downcase mode) command))))))

(use-package php-mode :ensure t)

(add-hook 'git-commit-mode-hook 'evil-insert-state)

(add-hook 'emmet-mode-hook
  (lambda ()
    (evil-define-key 'insert emmet-mode-keymap (kbd "C-l") 'emmet-next-edit-point)
    (evil-define-key 'insert emmet-mode-keymap (kbd "C-h") 'emmet-prev-edit-point)
  ))

(add-hook 'magit-mode-hook
          (lambda ()
            (define-key magit-mode-map (kbd ",o") 'delete-other-windows)))

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
  ;(add-to-list 'ac-sources 'ac-source-jedi-direct)
  (highlight-indentation-mode 0)
  ))

(defun my-web-mode-hook ()
    (setq web-mode-enable-current-column-highlight t)
    (setq web-mode-enable-auto-indentation t)
    (setq web-mode-attr-indent-offset 4)
    (setq web-mode-code-indent-offset 4)
    (setq web-mode-css-indent-offset 4)
    (setq web-mode-indent-style 4)
    (setq web-mode-markup-indent-offset 4)
    (setq web-mode-sql-indent-offset 4)
)
(add-hook 'web-mode-hook  'my-web-mode-hook)

(global-set-key (kbd "C-c b") 'switch-to-prev-buffer)
(global-set-key (kbd "C-c n") 'switch-to-next-buffer)
(global-set-key (kbd "C-h") 'windmove-left)
(global-set-key (kbd "C-j") 'windmove-down)
(global-set-key (kbd "C-k") 'windmove-up)
(global-set-key (kbd "C-l") 'windmove-right)

(custom-set-variables
 '(initial-frame-alist (quote ((fullscreen . maximized))))) ;; start maximized

(setq mouse-wheel-scroll-amount (quote (0.01)))
