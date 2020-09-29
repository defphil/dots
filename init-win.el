;; -*- lexical-binding: t; -*-

(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives
      '(("elpa" . "https://elpa.gnu.org/packages/")
        ("melpa" . "http://melpa.org/packages/")))

(package-initialize)
(when (not package-archive-contents)
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
              require-final-newline t
              cursor-type 'box
              indicate-buffer-boundaries '((top . right)
                                           (bottom . right)
                                           (t . nil)))
(setq-default tab-width 4)
(global-auto-revert-mode 1)
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
      frame-title-format (list "%b")
      read-process-output-max (* 1024 1024)
      gc-cons-threshold 100000000
      blink-cursor-blinks 7
      window-combination-resize t
      shift-select-mode nil)

(defvar newline-and-indent t)
(defun open-previous-line (arg)
  "Open a new line above current one ARG."
  (interactive "p")
  (beginning-of-line)
  (open-line arg)
  (when newline-and-indent
    (indent-according-to-mode)))
(global-set-key (kbd "M-o") 'open-previous-line)
;; keyboard escape quit on third ESC press annoys the hell out of me
;;(global-unset-key (kbd "ESC ESC ESC"))

(require 'server)
(or (server-running-p)
     (server-start))

;; -----------------------------------------------------------------------------
;; fonts & aesthetics
(add-hook 'c++-mode-hook #'modern-c++-font-lock-mode)

(if (eq system-type 'windows-nt)
    (set-frame-font "Consolas-10.5")
  (if (eq system-type 'gnu)
      (set-frame-font "DejaVu Sans Mono-12")))

(defun disable-all-themes ()
  (dolist (i custom-enabled-themes)
    (disable-theme i)))

(defadvice load-theme (before disable-themes-first activate)
  (disable-all-themes))

(load-theme 'alabaster t)

(defun maximize-frame ()
  "Maximize the current frame"
  (interactive)
  (w32-send-sys-command 61488))

(defun post-load-stuff ()
  (interactive)
  (maximize-frame)
  (set-cursor-color "#ff0048")
  )
(add-hook 'window-setup-hook 'post-load-stuff t)

;; nicer keybindings for I-search
(global-set-key [(control s)] 'isearch-forward-regexp)
(global-set-key [(control r)] 'isearch-backward-regexp)
(global-set-key [(meta %)] 'query-replace-regexp)
(global-set-key [(control o)] 'other-window)

;; -----------------------------------------------------------------------------

(use-package expand-region
  :bind ("C-=" . er/expand-region))

(use-package ace-window
  :commands ace-window
  :init (bind-key "C-x o" 'ace-window)
  :config (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

(use-package undo-tree
  :bind ("C-x C-u" . undo-tree-mode))

(global-set-key (kbd "C-z") 'undo)

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
               c++-mode-hook
               eval-expression-minibuffer-setup-hook))
    (add-hook m #'rainbow-delimiters-mode)))

(use-package which-key
  :defer 1
  :diminish which-key-mode
  :config (which-key-mode))

;; Replaced ivy/counsel/swiper combo with selectrum and prescient
(use-package selectrum
  :ensure t
  :diminish
  :commands selectrum-mode
  :config (setq selectrum-num-candidates-displayed 5))

(use-package prescient
  :ensure t
  :diminish)

(use-package selectrum-prescient
  :ensure t
  :after prescient
  :commands selectrum-prescient-mode
  :init (selectrum-prescient-mode +1)
         (prescient-persist-mode +1))

(use-package diminish
  :ensure t)

;; (use-package smex
;;   :config (smex-initialize))

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

(use-package flycheck-clang-tidy
  :after flycheck
  :config (setq flycheck-clang-tidy-executable "C:\\Program Files\\LLVM\\bin\\clang-tidy.exe")
  :hook (flycheck-mode . flycheck-clang-tidy-setup))

(add-hook 'before-save-hook
          (lambda ()
            (when (member major-mode '(c-mode c++-mode glsl-mode))
              (progn
                (when (locate-dominating-file "." ".clang-format")
                  (clang-format-buffer))
                ;; Return nil, to continue saving.
                nil))))

(setq hippie-expand-try-functions-list '(try-expand-dabbrev-visible
			                             try-expand-dabbrev
			                             try-expand-dabbrev-all-buffers
			                             try-expand-dabbrev-from-kill
			                             try-complete-file-name
			                             try-complete-file-name-partially))
(global-set-key (kbd "M-/") 'hippie-expand )

(use-package magit
  :defer t
  :bind ("C-x g" . magit-status)
  :custom ((magit-diff-refine-hunk t)))

(defun my-c-mode-common-hook ()
  "My C Mode Common Hook that force 2 spaces indent with Google's style"
  (c-set-style "bsd")
  (electric-pair-mode)
  (setq-default tab-width 4)
  (setq c-basic-offset 4)
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

(setq org-indent-indentation-per-level 1)
(setq org-hide-leading-stars 't)
(setq org-hide-emphasis-markers t)
(customize-set-variable 'org-blank-before-new-entry 
                        '((heading . nil)
                          (plain-list-item . nil)))
(setq org-cycle-separator-lines 1)

;; (if (eq system-type 'gnu)
;;     (defun screenshot-svg ()
;;       "Save a screenshot of the current frame as an SVG image.
;;        Saves to a temp file and puts the filename in the kill ring.
;;        Works only on Linux."
;;       (interactive)
;;       (let* ((filename (make-temp-file "Emacs" nil ".svg"))
;;              (data (x-export-frames nil 'svg)))
;;         (with-temp-file filename
;;           (insert data))
;;         (kill-new filename)
;;         (message filename))))
;; -----------------------------------------------------------------------------
;; Local Variables:
;; byte-compile-warnings: (not free-vars noruntime)
;; fill-column: 80
;; End:
;;; init.el ends here
