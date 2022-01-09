;;; init-lsp.el --- Insert description here -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package lsp-mode)
(use-package lsp-ui)

(require 'lsp-ui)
(require 'lsp-ui-imenu)
(require 'lsp-python-ms)

;;---------------------------------------------------------------------
;; Keybindings
;;---------------------------------------------------------------------
(define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
(define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)
(define-key lsp-ui-mode-map (kbd "C-c C-l .") 'lsp-ui-peek-find-definitions)
(define-key lsp-ui-mode-map (kbd "C-c C-l ?") 'lsp-ui-peek-find-references)
(define-key lsp-ui-mode-map (kbd "C-c C-l r") 'lsp-rename)
(define-key lsp-ui-mode-map (kbd "C-c C-l x") 'lsp-workspace-restart)
(define-key lsp-ui-mode-map (kbd "C-c C-l w") 'lsp-ui-peek-find-workspace-symbol)
(define-key lsp-ui-mode-map (kbd "C-c C-l i") 'lsp-ui-peek-find-implementation)
(define-key lsp-ui-mode-map (kbd "C-c C-l d") 'lsp-describe-thing-at-point)
(define-key lsp-ui-mode-map (kbd "C-c C-l e") 'lsp-execute-code-action)

;;---------------------------------------------------------------------
;; Configuration
;;---------------------------------------------------------------------
(setq lsp-ui-sideline-enable t)
(setq lsp-ui-sideline-show-diagnostics t)
(setq lsp-ui-sideline-show-code-actions nil)
(setq lsp-ui-doc-enable t)
(setq lsp-ui-doc-show-with-mouse t)
(setq lsp-ui-peek-enable t)
(setq lsp-ui-peek-always-show t)
(setq lsp-eldoc-enable-hover nil)
(setq lsp-headerline-breadcrumb-enable nil)
(setq lsp-modeline-diagnostics-enable t)
(setq lsp-modeline-code-actions-enable nil)

(with-eval-after-load 'lsp-mode
  (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration))

(add-hook 'prelude-python-mode-hook #'lsp-deferred)
(add-hook 'prelude-js-mode-hook #'lsp-deferred)
(add-hook 'prelude-ts-mode-hook #'lsp-deferred)
(add-hook 'vue-mode-hook #'lsp-deferred)


(provide 'init-lsp)
;;; init-lsp.el ends here
