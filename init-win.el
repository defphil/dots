;;; -*- lexical-binding: t -*-
;; Most important packages:
;; - ripgrep w/ deadgrep          }
;; - fd w/ find-file-in-project   } both require cargo(rust)
;; - paredit
;; - selectrum w/ prescient
;; - transient
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
(setq lazy-highlight-initial-delay 0)
(setq search-default-mode 'char-fold-to-regexp)


(global-set-key (kbd "M-o") 'open-previous-line)
;; -----------------------------------------------------------------------------
;; fonts & aesthetics
(if (eq system-type 'windows-nt)
    (set-frame-font "Consolas-10.5")
  (if (eq system-type 'gnu)
      (set-frame-font "DejaVu Sans Mono-12")))

(defun disable-all-themes ()
  (dolist (i custom-enabled-themes)
    (disable-theme i)))

(defadvice load-theme (before disable-themes-first activate)
  (disable-all-themes))
(load-theme 'aftereight t)

(defun maximize-frame ()
  "Maximize the current frame"
  (interactive)
  (w32-send-sys-command 61488))

(defvar newline-and-indent t)
(defun open-previous-line (arg)
  "Open a new line above current one ARG."
  (interactive "p")
  (beginning-of-line)
  (open-line arg)
  (when newline-and-indent
    (indent-according-to-mode)))

(defun post-load-stuff ()
  (interactive)
  (maximize-frame)
  (set-cursor-color "#ff0048"))
(add-hook 'window-setup-hook 'post-load-stuff t)

(defun my-c-mode-common-hook ()
  "My C Mode Common Hook that force 2 spaces indent with Google's style"
  (c-set-style "bsd")
  (electric-pair-mode)
  (setq-default tab-width 4)
  (setq c-basic-offset 4)
  (setq indent-tabs-mode nil))
(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)
(add-hook 'c++-mode-common-hook 'my-c-mode-common-hook)

(autoload 'dired-jump "dired-x"
  "C-x C-j
Jump to Dired buffer corresponding to current buffer." t)
(autoload 'dired-jump-other-window "dired-x"
  "C-x 4 C-J
Like \\[dired-jump] (dired-jump) but in other window." t)

(defun recentf-open-files+ ()
  "Use `completing-read' to open a recent file."
  (interactive)
  (let ((files (mapcar 'abbreviate-file-name recentf-list)))
    (find-file (completing-read "Find recent file: " files nil t))))
(bind-key* (kbd "C-c C-b") #'recentf-open-files+)
(global-set-key [(control s)] 'isearch-forward-regexp)
(global-set-key [(control r)] 'isearch-backward-regexp)
(global-set-key [(meta %)] 'query-replace-regexp)
(global-set-key [(control o)] 'other-window)
(global-set-key (kbd "C-z") 'undo)
;; -----------------------------------------------------------------------------

(use-package ace-window
  :commands ace-window
  :init (bind-key "C-x o" 'ace-window)
  :config (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

(use-package undo-tree
  :bind ("C-x C-u" . undo-tree-mode))

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

(use-package dumb-jump
  :after prog-mode
  :custom ((dumb-jump-force-searcher 'rg))
  :config
  (setq dumb-jump-aggressive t)
  :bind (:map prog-mode-map
              ("M-[" . dumb-jump-go)
              ("M-]" . dumb-jump-back)))

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

(use-package deadgrep
  :ensure t
  :config (bind-key* (kbd "C-c C-p") #'deadgrep))

(use-package find-file-in-project
  :ensure t
  :config (setq ffip-use-rust-fd t)
  (bind-key* (kbd "C-c C-f") #'find-file-in-project-by-selected))
;; -----------------------------------------------------------------------------
;; Local Variables:
;; byte-compile-warnings: (not free-vars noruntime)
;; fill-column: 80
;; End:
;;; init.el ends here
