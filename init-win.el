;;; package --- Summary
;;; Commentary:
;;; Code:
(require 'package)

(package-initialize)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(when (not package-archive-contents)
  (package-refresh-contents))

(eval-when-compile
  (require 'use-package))


(defalias 'display-startup-echo-area-message #'ignore)
(defalias 'yes-or-no-p 'y-or-n-p)
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
(setq-default cursor-type 'box)
;; -------------------------------------------------------------
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
              indicate-buffer-boundaries '((top . right)
                                           (bottom . right)
                                           (t . nil)))

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
      ;;inhibit-startup-hooks t
      mouse-wheel-scroll-amount '(1 ((shift) .1))
      mouse-wheel-progressive-speed nil
      scroll-step 1
      scroll-conservatively 100000
      scroll-margin 10
      fringes-outside-margins 1
      make-backup-files nil
      auto-save-default nil
      create-lockfiles nil
      frame-resize-pixelwise t
      make-backup-files nil
      create-lockfiles nil
      frame-title-format (list "emacs - %f")
      gc-cons-threshold (* 50 1000 1000)
      blink-cursor-blinks 7
      window-combination-resize t
      ;; dont't select text with shift, use ctrL+space
      shift-select-mode nil)

(setq initial-frame-alist
      '((width . 120)
        (height . 55)))

(defvar newline-and-indent t)
(defun open-previous-line (arg)
  "Open a new line above current one ARG."
  (interactive "p")
  (beginning-of-line)
  (open-line arg)

  (when newline-and-indent
    (indent-according-to-mode)))
(global-set-key (kbd "M-o") 'open-previous-line)


(defun my-scroll-hook()
  "Increase gc-threshold before scroll and set it back after."
  (setq gc-cons-threshold most-positive-fixnum)
  (run-with-idle-timer 3 nil (lambda () (setq gc-cons-threshold (* 8 1024 1024)))))
(advice-add 'scroll-up-line :before 'my-scroll-hook)
(advice-add 'scroll-down-line :before 'my-scroll-hook)
;;(setq line-spacing 1)
(set-frame-font "Cascadia Code-11")

(load-theme '4coder t)


;; -------------------------------------------------------
(use-package expand-region
  :bind ("C-=" . er/expand-region))

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

(use-package company
  :defer 1
  :diminish company-mode
  :init
  (add-hook 'after-init-hook 'global-company-mode)
  (add-hook 'after-init-hook (lambda ()
                               (company-mode -1)))
  :config
  (setq company-auto-complete nil
        company-idle-delay .3
        company-show-numbers t
        company-echo-delay 0
        company-tooltip-flip-when-above t
        company-minimum-prefix-length 2
        company-tooltip-limit 12))


(use-package ivy
  :init (add-hook 'after-init-hook #'ivy-mode)
  :diminish (ivy-mode)
  :config
  (setq ivy-height 8
        ivy-wrap t
        ivy-display-style 'fancy
        ivy-fixed-height-minibuffer t
        ivy-use-virtual-buffers t
        enable-recursive-minibuffers t
        ivy-re-builders-alist '((counsel-M-x . ivy--regex-ignore-order)
                                (t . ivy--regex-plus)))
  (define-key ivy-minibuffer-map (kbd "C-j") #'ivy-immediate-done)
  (define-key ivy-minibuffer-map (kbd "RET") #'ivy-alt-done))
;; Making presets of buffers and panes, simple stack configuration
;; works really, really good, saves state as `virtual` buffer
(global-set-key (kbd "C-c v") 'ivy-push-view)
(global-set-key (kbd "C-c V") 'ivy-pop-view)

(use-package ivy-rich
  :after ivy
  :init (ivy-rich-mode 1)
  :config (setq ivy-virtual-abbreviate 'full
                ivy-rich-path-style 'abbrev))

;; Have different height for find-file and M-x menus

(use-package swiper
  :bind (("C-s"     . swiper)
         ("C-r"     . swiper)
         ("C-c u"   . swiper-all)
         ("C-c C-r" . ivy-resume)
         ("C-c C-o" . ivy-occur))
  :config (setq swiper-include-line-number-in-search t))

(use-package smex
  :config (smex-initialize))

(use-package counsel
  :bind (("M-x"     . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         ("C-h f"   . counsel-describe-function)
         ("C-h v"   . counsel-describe-variable)
         ("C-c C-s" . counsel-rg)
         ("M-y"     . counsel-yank-pop)))

(use-package dumb-jump
  :after prog-mode
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
  :init (global-flycheck-mode))

(use-package yasnippet
  :disabled t
  :defer t
  :diminish yas-minor-mode
  :bind (("C-c C-c" . yas-insert-snippet))
  :config (yas-reload-all)
  (add-hook 'prog-mode-hook #'yas-minor-mode))

(use-package ace-window
  :commands ace-window
  :init (bind-key "C-x o" 'ace-window)
  :config (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

;; Use Google's style for both C and C++
(use-package google-c-style
  :ensure t)
(defun my-c-mode-common-hook ()
  "My C Mode Common Hook that force 2 spaces indent with google's style."
  (c-set-style "google")
  (electric-pair-mode)
  (setq tab-width 2)
  (setq c-basic-offset tab-width)
  (setq indent-tabs-mode nil))
(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)
(add-hook 'c++-mode-common-hook 'my-c-mode-common-hook)



;; -----------------------------------------------------------------------------
;; Local Variables:
;; byte-compile-warnings: (not free-vars noruntime)
;; fill-column: 80
;; End:

(provide 'init.el)
;;; init.el ends here
