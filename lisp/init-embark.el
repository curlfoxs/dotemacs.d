;;; init-embark.el --- Insert description here -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require-package 'embark)
(require 'embark)
;;---------------------------------------------------------------------
;; Marginalia basic config
;;---------------------------------------------------------------------
(when (maybe-require-package 'marginalia)
    (marginalia-mode 1))

;;---------------------------------------------------------------------
;; Embark basic config
;;---------------------------------------------------------------------
(when (maybe-require-package 'embark)
  (setq prefix-help-command #'embark-prefix-help-command)
  (global-set-key (kbd "C-s-.") 'embark-act-noquit)
  (global-set-key (kbd "C-.") 'embark-act)
  (global-set-key (kbd "s-.") 'embark-dwim)
  (global-set-key (kbd "C-h B") 'embark-bindings)
  (when (and (maybe-require-package 'embark-consult)
             (maybe-require-package 'consult))
    (add-hook 'embark-coolect-mode 'consult-previw-at-point-mode)
    ))

;; ("C-s-." . embark-act-noquit)
;; ("C-." . embark-act)         ;; pick some comfortable binding
;; ("s-;" . embark-dwim)        ;; good alternative: M-.
;; ("C-h B" . embark-bindings) ;; alternative for `describe-bindings'

;;---------------------------------------------------------------------
;; Embark Customize -- tabbar
;;---------------------------------------------------------------------
(add-to-list 'marginalia-prompt-categories '("tab by name" . tab))
(defun my-select-tab-by-name (tab)
  (interactive
   (list
    (let ((tab-list (or (mapcar (lambda (tab) (cdr (assq 'name tab)))
				(tab-bar-tabs))
			(user-error "No tabs found"))))
      (consult--read tab-list
		     :prompt "Tabs: "
		     :category 'tab))))
  (tab-bar-select-tab-by-name tab))

(embark-define-keymap embark-tab-actions
  "Keymap for actions for tab-bar tabs (when mentioned by name)."
  ("s" tab-bar-select-tab-by-name)
  ("r" tab-bar-rename-tab-by-name)
  ("k" tab-bar-close-tab-by-name))

(add-to-list 'embark-keymap-alist '(tab . embark-tab-actions))


;;---------------------------------------------------------------------
;; Embark amazing wiki codes
;;---------------------------------------------------------------------
;;---------------------------------------------------------------------
;; Embark action no quit, I think it's better and set default
;;---------------------------------------------------------------------
(require 'embark) ;; Why should I load it for repeat time?
(defun embark-act-noquit ()
  "Run action but don't quit the minibuffer afterwards."
  (interactive)
  (let ((embark-quit-after-action nil))
    (embark-act)))

;;---------------------------------------------------------------------
;; Showing info about available targets and actions
;;---------------------------------------------------------------------
;; which-key-indicator, delete because of BUG
;; (defun embark-which-key-indicator ()
;;   "An embark indicator that displays keymaps using which-key.
;; The which-key help message will show the type and value of the
;; current target followed by an ellipsis if there are further
;; targets."
;;   (lambda (&optional keymap targets prefix)
;;     (if (null keymap)
;;         (which-key--hide-popup-ignore-command)
;;       (which-key--show-keymap
;;        (if (eq (plist-get (car targets) :type) 'embark-become)
;;            "Become"
;;          (format "Act on %s '%s'%s"
;;                  (plist-get (car targets) :type)
;;                  (embark--truncate-target (plist-get (car targets) :target))
;;                  (if (cdr targets) "…" "")))
;;        (if prefix
;;            (pcase (lookup-key keymap prefix 'accept-default)
;;              ((and (pred keymapp) km) km)
;;              (_ (key-binding prefix 'accept-default)))
;;          keymap)
;;        nil nil t (lambda (binding)
;;                    (not (string-suffix-p "-argument" (cdr binding))))))))
;; (setq embark-indicators
;;   '(embark-which-key-indicator
;;     embark-highlight-indicator
;;     embark-isearch-highlight-indicator))

;; (defun embark-hide-which-key-indicator (fn &rest args)
;;   "Hide the which-key indicator immediately when using the completing-read prompter."
;;   (which-key--hide-popup-ignore-command)
;;   (let ((embark-indicators
;;          (remq #'embark-which-key-indicator embark-indicators)))
;;       (apply fn args)))
;; (advice-add #'embark-completing-read-prompter
;;             :around #'embark-hide-which-key-indicator)

;;---------------------------------------------------------------------
;; Definition actions for targets
;;---------------------------------------------------------------------
;; embark-ace-action
(eval-when-compile
(defmacro my/embark-ace-action (fn)
    `(defun ,(intern (concat "my/embark-ace-" (symbol-name fn))) ()
    (interactive)
    (with-demoted-errors "%s"
        (require 'ace-window)
        (let ((aw-dispatch-always t))
        (aw-switch-to-window (aw-select nil))
        (call-interactively (symbol-function ',fn)))))))

;; embark-split-action
(eval-when-compile
  (defmacro my/embark-split-action (fn split-type)
    `(defun ,(intern (concat "my/embark-"
                             (symbol-name fn)
                             "-"
                             (car (last  (split-string
                                          (symbol-name split-type) "-"))))) ()
       (interactive)
       (funcall #',split-type)
       (call-interactively #',fn))))

(define-key embark-file-map     (kbd "o") (my/embark-ace-action find-file))
(define-key embark-buffer-map   (kbd "o") (my/embark-ace-action switch-to-buffer))
(define-key embark-bookmark-map (kbd "o") (my/embark-ace-action bookmark-jump))
(define-key embark-file-map     (kbd "2") (my/embark-split-action find-file split-window-below))
(define-key embark-buffer-map   (kbd "2") (my/embark-split-action switch-to-buffer split-window-below))
(define-key embark-bookmark-map (kbd "2") (my/embark-split-action bookmark-jump split-window-below))
(define-key embark-file-map     (kbd "3") (my/embark-split-action find-file split-window-right))
(define-key embark-buffer-map   (kbd "3") (my/embark-split-action switch-to-buffer split-window-right))
(define-key embark-bookmark-map (kbd "3") (my/embark-split-action bookmark-jump split-window-right))

;;---------------------------------------------------------------------
;; Personal customize
;;---------------------------------------------------------------------
;; Showing embark actions in keycast-mode
(defun store-action-key+cmd (cmd)
  (setq keycast--this-command-keys (this-single-command-keys)
        keycast--this-command cmd))
(advice-add 'embark-keymap-prompter :filter-return #'store-action-key+cmd)

(defun force-keycast-update (&rest _)
  (force-mode-line-update t))

(dolist (cmd '(embark-act embark-become))
  (advice-add cmd :before #'force-keycast-update))


;; Show the current Embark target types in the modeline
(defvar embark--target-mode-timer nil)
(defvar embark--target-mode-string "")

(defun embark--target-mode-update ()
  (setq embark--target-mode-string
        (if-let (targets (embark--targets))
            (format "[%s%s] "
                    (propertize (symbol-name (plist-get (car targets) :type)) 'face 'bold)
                    (mapconcat (lambda (x) (format ", %s" (plist-get x :type)))
                               (cdr targets)
                               ""))
          "")))

(define-minor-mode embark-target-mode
  "Shows the current targets in the modeline."
  :global t
  (setq mode-line-misc-info (assq-delete-all 'embark-target-mode mode-line-misc-info))
  (when embark--target-mode-timer
    (cancel-timer embark--target-mode-timer)
    (setq embark--target-mode-timer nil))
  (when embark-target-mode
    (push '(embark-target-mode (:eval embark--target-mode-string)) mode-line-misc-info)
    (setq embark--target-mode-timer
          (run-with-idle-timer 0.1 t #'embark--target-mode-update))))


(provide 'init-embark)
;;; init-embark.el ends here
