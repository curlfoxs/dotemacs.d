;;; init-editing-tools.el --- Insert description here -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;---------------------------------------------------------------------
;; Drag-stuff
;;---------------------------------------------------------------------
(require-package 'drag-stuff)
(global-set-key (kbd "M-<up>") 'drag-stuff-up)
(global-set-key (kbd "M-<down>") 'drag-stuff-down)

;;---------------------------------------------------------------------
;; which-key
;;---------------------------------------------------------------------
(when (maybe-require-package 'which-key)
  (add-hook 'after-init-hook 'which-key-mode)
  (with-eval-after-load 'which-key
  (diminish 'which-key-mode)))

;;---------------------------------------------------------------------
;; Delete-selection
;;---------------------------------------------------------------------
(require-package 'delsel)
(add-hook 'after-init-hook 'delete-selection-mode)

(add-hook 'after-init-hook 'global-auto-revert-mode)

;; smart tab behavior - indent or complete
(setq tab-always-indent 'complete)

;;---------------------------------------------------------------------
;; smarter kill-ring navigation
;;---------------------------------------------------------------------
(require-package 'browse-kill-ring)
(browse-kill-ring-default-keybindings)
(global-set-key (kbd "s-y") 'browse-kill-ring)

;;---------------------------------------------------------------------
;; save-place-mode save file palce when reopen
;;---------------------------------------------------------------------
(add-hook 'after-init-hook 'save-place-mode)

(provide 'init-editing-tools)
;;; init-editing-tools.el ends here
