(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives
      '(("elpa" . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (warn "use-package is not installed, trying to install")
  (package-install 'use-package))
(require 'use-package)
(defalias 'display-startup-echo-area-message #'ignore)
(defalias 'yes-or-no-p 'y-or-n-p)

;; UTF-8 everywhere for everything
(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(add-to-list 'load-path "~/.emacs.d/lisp")
(setq use-package-always-ensure t)
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

(require 'server)
(unless (server-running-p)
  (server-start))

(transient-mark-mode 1)
(tool-bar-mode       0)
(menu-bar-mode       0)
(scroll-bar-mode     0)
(show-paren-mode     1)
(line-number-mode    1)
(column-number-mode  1)
(blink-cursor-mode   1)

(setq-default indent-tabs-mode nil
              tab-width 4
              require-final-nrewline t
              cursor-type 'box
              indicate-buffer-boundaries '((top . right)
                                           (bottom . right)
                                           (t . nil)))
(global-auto-revert-mode    1)
(setq mouse-autoselect-window nil
      focus-follows-mouse nil
      load-prefer-newer t
      delete-old-versions t
      ring-bell-function 'ignore
      inhibit-splash-screen t
      initial-scratch-message nil
      inhibit-startup-message t
      inhibit-startup-buffer-menu t
      inhibit-startup-screen t
      mouse-wheel-scroll-amount '(1 ((shift) .1))
      mouse-wheel-progressive-speed nil
      scroll-step 1
      scroll-conservatively 100000
      scroll-margin 1
      fringes-outside-margins 1
      make-backup-files nil
      auto-save-default nil
      create-lockfiles nil
      frame-resize-pixelwise t
      frame-inhibit-implied-resize t
      initial-major-mode 'fundamental-mode
      make-backup-files nil
      create-lockfiles nil
      frame-title-format (list " %b ")
      read-process-output-max (* 1024 1024)
      gc-cons-threshold 100000000
      blink-cursor-blinks 7
      window-combination-resize t
      shift-select-mode nil)

(setq initial-frame-alist
      '((width . 110)
        (height . 50)))

(defvar newline-and-indent t)
(defun open-previous-line (arg)
  "Open a new line above current one ARG."
  (interactive "p")
  (beginning-of-line)
  (open-line arg)
  (when newline-and-indent
    (indent-according-to-mode)))

(global-set-key (kbd "M-o") 'open-previous-line)

(if (eq system-type 'windows-nt)
    (add-to-list 'default-frame-alist '(font . "Consolas-11"))
  (if (eq system-type 'gnu)
      (add-to-list 'default-frame-alist '(font . "DejaVu Sans Mono-12"))))

;; ir-black / zenburn / naysayer
(load-theme 'syohex t)

(defun disable-all-themes ()
  (dolist (i custom-enabled-themes)
    (disable-theme i)))
(defadvice load-theme (before disable-themes-first activate)
  (disable-all-themes))

;; -------------------------------------------------------
(use-package expand-region
  :bind ("C-=" . er/expand-region))

(use-package undo-tree)

(use-package rainbow-mode)

(use-package paredit
  :diminish paredit-mode
  :config
  (dolist (m '(clojure-mode-hook
               cider-repl-mode-hook
               clojure-mode-hook
               emacs-lisp-mode-hook
               racket-mode-hook
               racket-repl-mode-hook
               scheme-mode-hook
               slime-mode-hook
               slime-repl-mode-hook
               eval-expression-minibuffer-setup-hook))
    (add-hook m #'paredit-mode)))

(setq lazy-highlight-initial-delay 0)
(setq search-default-mode 'char-fold-to-regexp)

(use-package highlight-numbers
  :config
  (add-hook 'prog-mode-hook 'highlight-numbers-mode))

(use-package rainbow-delimiters
  :config
  (dolist (m '(clojure-mode-hook
               cider-repl-mode-hook
               emacs-lisp-mode-hook
               racket-mode-hook
               racket-repl-mode-hook
               scheme-mode-hook
               lisp-mode-hook
               eval-expression-minibuffer-setup-hook))
    (add-hook m #'rainbow-delimiters-mode)))

(use-package which-key
  :defer 1
  :diminish which-key-mode
  :config (which-key-mode))

(use-package hl-todo
  :ensure t
  :config (dolist (m '(c-mode-hook
                       c++-mode-hook
                       scheme-mode-hook
                       clojure-mode-hook
                       lisp-mode-hook))
            (add-hook m #'hl-todo-mode)))

(use-package ivy
  :demand t
  :ensure t
  :diminish
  :commands ivy-mode
  :custom ((ivy-wrap t)
           (ivy-height 8)
           (ivy-display-style 'fancy)
           (ivy-use-virtual-buffers t)
           (ivy-case-fold-search-default t)
           (ivy-re-builders-alist '((t . ivy--regex-fuzzy)))
           (enale-recursive-minibuffers t))
  :config (ivy-mode t)
  :bind (:map ivy-minibuffer-map
              ("<RET>" . ivy-alt-done)))

(use-package counsel
  :after ivy
  :ensure t
  :diminish
  :commands counsel-mode
  :init (counsel-mode t)
  :bind (("C-h f"   . counsel-describe-function)
         ("C-h v"   . counsel-describe-variable)
         ("C-c s"   . counsel-rg)
         ("C-x C-/" . counsel-org-goto-all)
         ("C-x C-]" . counsel-file-jump)))

(setq counsel-grep-base-command
      "rg -i -M 120 --no-heading --line-number --color never '%s' %s")

(use-package diminish
  :ensure t)

(use-package swiper
  :ensure t
  :bind ("C-o" . swiper))

(use-package smex
  :config (smex-initialize))

(use-package dumb-jump
  :after prog-mode
  :custom ((dumb-jump-force-searcher 'rg))
  :config
  (setq dumb-jump-aggressive t)
  :bind (:map prog-mode-map
              ("M-[" . dumb-jump-go)
              ("M-]" . dumb-jump-back)))

(use-package projectile
  :diminish
  :after ivy
  :commands projectile-mode
  :custom ((projectile-require-project-root nil)
           (projectile-switch-project-action 'projectile-dired)
           (projectile-completion-system 'default))
  :init
  (projectile-mode t)
  :bind ("C-x p" . projectile-command-map))

(use-package flycheck
  :ensure t
  :custom (flycheck-disabled-checkers '(emacs-lisp-checkdoc)))

(use-package yasnippet
  :disabled t
  :defer the
  :diminish yas-minor-mode
  :bind (("C-c C-c" . yas-insert-snippet))
  :config (yas-reload-all)
  (add-hook 'prog-mode-hook #'yas-minor-mode))

(setq hippie-expand-try-functions-list '(try-expand-dabbrev-visible
			  try-expand-dabbrev
			  try-expand-dabbrev-all-buffers
			  try-expand-dabbrev-from-kill
			  try-complete-file-name
			  try-complete-file-name-partially))
(global-set-key (kbd "M-/") 'hippie-expand )

(use-package ace-window
  :commands ace-window
  :init (bind-key "C-x o" 'ace-window)
  :config (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

(use-package google-c-style
  :ensure t)

(defun my-c-mode-common-hook ()
  "My C Mode Common Hook that force 2 spaces indent with Google's style"
  (c-set-style "google")
  (electric-pair-mode)
  (setq tab-width 2)
  (setq c-basic-offset tab-width)
  (setq indent-tabs-mode nil))
(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)
(add-hook 'c++-mode-common-hook 'my-c-mode-common-hook)

(defmacro setl (sym val)
  "Produce a lambda expression that locally sets a value"
  `(function (lambda () (setq-local ,sym ,val))))

(use-package semantic
  :config
  (add-hook 'semantic-mode-hook
			(setl completion-at-point-functions '(semantic-analyze-completion-at-point-function)))
  (setq semantic-default-submodes
		'(global-semantic-idle-scheduler-mode
		  global-semantic-idle-summary-mode
		  global-semanticdb-minor-mode
		  global-semantic-stickyfunc-mode)))

(use-package editorconfig
  :defer t
  :config (editorconfig-mode t))

;; ORG settings
(setq org-hide-emphasis-markers t)

;; -----------------------------------------------------------------------------
;; Local Variables:
;; byte-compile-warnings: (not free-vars noruntime)
;; fill-column: 80
;; End:
;;; init.el ends here
