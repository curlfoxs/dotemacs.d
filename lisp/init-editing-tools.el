(require-package 'ace-window)
(require-package 'drag-stuff)
(require-package 'which-key)
(which-key-mode 1)

;;---------------------------------------------------------------------
;; Delete-selection
;;---------------------------------------------------------------------
(require-package 'delsel)
(delete-selection-mode 1)

(global-auto-revert-mode 1)

;; smart tab behavior - indent or complete
(setq tab-always-indent 'complete)

;;---------------------------------------------------------------------
;; smarter kill-ring navigation
;;---------------------------------------------------------------------
(require-package 'browse-kill-ring)
(browse-kill-ring-default-keybindings)
(global-set-key (kbd "s-y") 'browse-kill-ring)


(provide 'init-editing-tools)
