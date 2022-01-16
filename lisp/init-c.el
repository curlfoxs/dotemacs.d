;;; init-c.el --- Insert description here -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;;---------------------------------------------------------------------
;; using irony
;;---------------------------------------------------------------------
(require-package 'irony)
(add-hook 'c-mode-common-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'objc-mode-hook 'irony-mode)

(defun my-irony-mode-hook ()
  (define-key irony-mode-map [remap completion-at-point]
    'irony-completion-at-point-async)
  (define-key irony-mode-map [remap complete-symbol]
    'irony-completion-at-point-async))

(add-hook 'irony-mode-hook 'my-irony-mode-hook)
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

;; ;; ;; Clang options
;; (setq irony-additional-clang-options (quote ("-std=c++11")))

;; ;; this will read from the project's root .json or clang_complete
;; (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

;; (defun setup-cpp-clang-options()
;;   (setq irony-addiional-clang-options (quote ("-std=c++14" "-stdlib=libc++"))))
;; (add-hook 'c++-mode-hook 'setup-cpp-clang-options)


;;---------------------------------------------------------------------
;; Using company with irony
;;---------------------------------------------------------------------

;; (add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)
(setq company-backends (delete 'company-semantic company-backends))

(require-package 'company-irony)
(require-package 'company-irony-c-headers)
(eval-after-load 'company
  '(add-to-list
    'company-backends '(company-irony-c-headers company-irony)))

;;---------------------------------------------------------------------
;; rtags
;;---------------------------------------------------------------------

;; (require-package 'rtags)
;; (require-package 'company-rtags)

;; (setq rtags-completions-enabled t)
;; (eval-after-load 'company
;;   '(add-to-list
;;     'company-backends 'company-rtags))
;; (setq rtags-autostart-diagnostics t)
;; (rtags-enable-standard-keybindings)

;;---------------------------------------------------------------------
;; Flycheck rtags
;;---------------------------------------------------------------------
;; (require-package 'flycheck-rtags)

;; (defun my-flycheck-rtags-setup ()
;;   ;; (flycheck-select-checker 'rtags)
;;   (setq-local flycheck-highlighting-mode nil) ;; RTags creates more accurate overlays.
;;   (setq-local flycheck-check-syntax-automatically nil))
;; ;; c-mode-common-hook is also called by c++-mode
;; (add-hook 'c-mode-common-hook #'my-flycheck-rtags-setup)

;; (eval-after-load 'flycheck
;;   '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup))

;;---------------------------------------------------------------------
;; Cmake-ide
;;---------------------------------------------------------------------

(require-package 'cmake-ide)
(cmake-ide-setup)

(provide 'init-c)
;;; init-c.el ends here
