;;; init-evil.el --- Insert description here -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(maybe-require-package 'smartparens)
(maybe-require-package 'expand-region)
(maybe-require-package 'browse-kill-ring)
(require-package 'workgroups2) ;; Window and buffer manager
(workgroups-mode 1)   ; put this one at the bottom of .emacs

(when (and (maybe-require-package 'evil) (maybe-require-package 'general))
  ;;---------------------------------------------------------------------
  ;; Use emacs bindings
  ;;---------------------------------------------------------------------
  (setq evil-disable-insert-state-bindings t)
  (setq evil-move-beyond-eol t)
  (setq evil-move-cursor-back nil)
  (evil-mode 1)
  (evil-define-key 'normal 'global
    (kbd "C-v") 'scroll-up-command
    (kbd "C-k") 'sp-kill-hybrid-sexp
    (kbd "C-o") 'open-line
    (kbd "C-r") 'undo-redo
    (kbd "C-y") 'yank
    (kbd "C-e") 'move-end-of-line
    (kbd "C-n") 'next-line
    (kbd "C-p") 'previous-line
    (kbd "C-f") 'forward-char
    (kbd "C-b") 'backward-char
    (kbd "C-x C-p") 'mark-page
    (kbd "C-t") 'transpose-chars
    (kbd "C-d") 'sp-delete-char
    (kbd "C-a") 'move-beginning-of-line
    (kbd "C-w") 'kill-region
    (kbd ",") (general-simulate-key "C-x")
    (kbd ".") (general-simulate-key "C-c")
    (kbd "e") 'er/expand-region
    (kbd "H") 'sp-backward-sexp
    (kbd "L") 'sp-forward-sexp
    (kbd "K") 'kill-sexp)

  (evil-define-key 'normal 'org-mode-map
    (kbd "TAB") 'org-cycle)
  (evil-define-key 'insert 'global (kbd "C-u") 'evil-normal-state)
  (evil-define-key 'normal 'global (kbd "C-u") 'evil-normal-state)

  ;;---------------------------------------------------------------------
  ;; Evil in minor mode
  ;;---------------------------------------------------------------------
  (setq evil-want-minibuffer t)

  ;;---------------------------------------------------------------------
  ;; SPC as leader key
  ;;---------------------------------------------------------------------
  (defvar wullic-leader-map (make-sparse-keymap)
    "Keymap for \" Wullic leader key\" shortcuts")
  (define-key evil-normal-state-map  (kbd "SPC") wullic-leader-map)
  (when (maybe-require-package 'magit)
      (add-hook 'magit-mode-hook
		(lambda ()
		  (define-key magit-mode-map (kbd "SPC") wullic-leader-map)
		  (define-key magit-mode-map (kbd ",") (general-simulate-key "C-x"))
		  (define-key magit-mode-map (kbd ".") (general-simulate-key "C-c")))))
  (add-hook 'dired-mode-hook (lambda () (define-key dired-mode-map (kbd "SPC") wullic-leader-map)))
  (add-hook 'message-mode-hook (lambda () (define-key message-mode-map (kbd "SPC") wullic-leader-map)))
  (add-hook 'help-mode-hook (lambda () (define-key help-mode-map (kbd "SPC") wullic-leader-map)))
  (general-evil-setup)
  (general-auto-unbind-keys)
  (general-nmap
   :state 'normal
   :override t
   :prefix "SPC"
   :prefix-map 'wullic-leader-map
   "bb" 'previous-buffer
   "bf" 'next-buffer
   "e" 'er/expand-region
   "ff" 'find-file
   "fr" 'consult-recent-file
   "fd" 'consult-dir
   "fR" 'rename-this-file-and-buffer
   "fD" 'delete-this-file
   "h" 'help-command
   "jx" 'exchange-point-and-mark
   "mm" 'magit-status
   "mc" 'magit-clone
   "p" (general-simulate-key "C-x p")
   "q" 'save-buffers-kill-emacs
   "s" 'save-buffer
   "wo" 'wg-open-workgroup
   "wk" 'wg-kill-workgroup
   "wc" 'wg-create-workgroup
   "ww" 'ace-window
   "y" 'browse-kill-ring)
  (general-auto-unbind-keys t))



(provide 'init-evil)
;;; init-evil.el ends here
