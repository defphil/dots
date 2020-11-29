;;; qgrep.el --- interface for qgrep indexed search -*- lexical-binding: t; -*-
(require 'dash)
(require 'compile)
(require 'grep)

(defgroup qgrep nil
  "qgrep"
  :group 'tools
  :group 'matching)

(defcustom qgrep-exe
  "qgrep"
  "Name of the ripgrep executable to use."
  :type 'string
  :group 'qgrep)

(defcustom qgrep-project
  (list "blender")
  "Name of project used for grepping"
  :type '(repeat (string))
  :group 'qgrep)

(defcustom qgrep-command
  (list "search" "watch")
  "Default arguments passed to qgrep."
  :type '(repeat (string))
  :group 'qgrep)


;; Faces
;; --------------------------

(defface qgrep-hit-face '((t :inherit compilation-info))
  "Face name to use for hits."
  :group 'qgrep)

(defface qgrep-match-face '((t :inherit match))
  "Face name to use for matches."
  :group 'qgrep)

(defface qgrep-error-face '((t :inherit compilation-error))
  "Face name to use for errors."
  :group 'qgrep)

(defface qgrep-context-face '((t :inherit shadow))
  "Face name to use for errors."
  :group 'qgrep)


;; Graceful termination
;; ----------------------------

(defvar qgrep-search-finished-hook nil
  "Hook run when ripgrep completes a search in a buffer.")

(defun qgrep/run-finished-hook (buffer how-finished)
  "Run the ripgrep hook to signal that the search has completed."
  (with-current-buffer buffer
    (run-hooks 'qgrep-search-finished-hook)))

(defun qgrep/kill-buffer ()
  "Kill the ripgrep search buffer."
  (interactive)
  (let ((kill-buffer-query-functions))
    (kill-buffer)))


;; Local variables
;; --------------------------
(defvar args '("i" "l")
  "Default for case insesitive literals; can be used for reges search with `r`")

(defvar qgrep-search-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map compilation-minor-mode-map)
    (define-key map "p" 'compilation-previous-error)
    (define-key map "n" 'compilation-next-error)
    (define-key map "{" 'compilation-previous-file)
    (define-key map "}" 'compilation-next-file)
    (define-key map "k" 'qgrep/kill-buffer)
    map)
  "Keymap for ripgrep-search buffers.
`compilation-minor-mode-map' is a cdr of this.")


;; Functions
;; --------------------------
(define-compilation-mode qgrep-search-mode "qgrep"
  "qgrep results."
  (set (make-local-variable 'truncate-lines) nil)
  (set (make-local-variable 'compilation-disable-input) t)
  (let ((symbol 'compilation-qgrep)
        (pattern '("^\\([^\n]+?\\):\\([0-9]+\\):" 1 2)))
    (set (make-local-variable 'compilation-error-regexp-alist) (list symbol))
    (set (make-local-variable 'compilation-error-regexp-alist-alist) (list (cons symbol pattern))))
  (set (make-local-variable 'compilation-error-face) 'qgrep-hit-face))

(defun qgrep (regexp)
  "TEST."
  (interactive
   (list (read-from-minibuffer "qgrep: " (thing-at-point 'symbol))))
  (let ()
    (compilation-start
     (mapconcat 'identity
                (append (list qgrep-exe)
                        (list (-first-item qgrep-command))
                        qgrep-project
                        args
                        (list regexp)) " ")
     'qgrep-search-mode)))

(provide 'qgrep)
;;; qgrep.el ends here
