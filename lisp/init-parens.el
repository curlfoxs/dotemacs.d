;;; init-paredit.el --- Configure paredit structured editing -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;---------------------------------------------------------------------
;; paredit-mode
;;---------------------------------------------------------------------
(require-package 'paredit)

(defun sanityinc/maybe-map-paredit-newline ()
  (unless (or (memq major-mode '(inferior-emacs-lisp-mode cider-repl-mode))
              (minibufferp))
    (local-set-key (kbd "RET") 'paredit-newline)))

(add-hook 'paredit-mode-hook 'sanityinc/maybe-map-paredit-newline)

(with-eval-after-load 'paredit
  (diminish 'paredit-mode)
  ;; Suppress certain paredit keybindings to avoid clashes, including
  ;; my global binding of M-?
  (dolist (binding '("C-<left>" "C-<right>" "C-M-<left>" "C-M-<right>" "M-s" "M-?"))
    (define-key paredit-mode-map (read-kbd-macro binding) nil))
  (keymap-unset paredit-mode-map (kbd "C-d")))


;;---------------------------------------------------------------------
;; Use paredit in the minibuffer
;;---------------------------------------------------------------------
;; TODO: break out into separate package
;; http://emacsredux.com/blog/2013/04/18/evaluate-emacs-lisp-in-the-minibuffer/
(add-hook 'minibuffer-setup-hook 'sanityinc/conditionally-enable-paredit-mode)

(defvar paredit-minibuffer-commands '(eval-expression
                                      pp-eval-expression
                                      eval-expression-with-eldoc
                                      ibuffer-do-eval
                                      ibuffer-do-view-and-eval)
  "Interactive commands for which paredit should be enabled in the minibuffer.")

(defun sanityinc/conditionally-enable-paredit-mode ()
  "Enable paredit during lisp-related minibuffer commands."
  (if (memq this-command paredit-minibuffer-commands)
      (enable-paredit-mode)))




;;---------------------------------------------------------------------
;; smartparens
;;---------------------------------------------------------------------
(require-package 'smartparens)
(require 'smartparens-config)
(setq sp-base-key-bindings 'paredit)
(setq sp-autoskip-closing-pair 'always)
(sp-use-paredit-bindings)
(show-smartparens-global-mode +1)
;; (add-hook 'prog-mode-hook (lambda () (unless (derived-mode-p 'emacs-lisp-mode)
				       ;; (smartparens-mode))))
;; I like smartparens strict mode, you could use above line
(add-hook 'prog-mode-hook (lambda () (smartparens-strict-mode)))

;; Smartparnes's M-? littile use, remain keymap to others
(with-eval-after-load 'smartparens
  (define-key smartparens-mode-map (kbd "M-?") nil))

;;---------------------------------------------------------------------
;; expand-region
;;---------------------------------------------------------------------

(require-package 'expand-region)

(global-set-key (kbd "M-/") 'hippie-expand)

(setq hippie-expand-try-functions-list
      '(try-complete-file-name-partially
        try-complete-file-name
        try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill))

(provide 'init-regions)

(provide 'init-parens)
;;; init-paredit.el ends here
