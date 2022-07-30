;;; init-evil.el --- Insert description here -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defun wullic/evil-ret (arg)
  (interactive "p")
  (evil-insert 1)
  (newline-and-indent)
  (evil-normal-state))

(maybe-require-package 'smartparens)
(maybe-require-package 'expand-region)
(maybe-require-package 'browse-kill-ring)
(require-package 'workgroups2) ;; Window and buffer manager
(workgroups-mode 1)   ; put this one at the bottom of .emacs
(maybe-require-package 'general)
(maybe-require-package 'consult)

(when (maybe-require-package 'evil)
  ;;---------------------------------------------------------------------
  ;; Use emacs bindings
  ;;---------------------------------------------------------------------
  (setq evil-disable-insert-state-bindings t)
  (setq evil-move-beyond-eol t)
  (setq evil-move-cursor-back nil)
  (evil-mode 1)
  (evil-define-key '(normal motion) 'global
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
    (kbd "K") 'kill-sexp
    (kbd "C-m") 'wullic/evil-ret
    (kbd "m") 'set-mark-command)

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
   :state '(normal motion)
   :override t
   :prefix "SPC"
   :prefix-map 'wullic-leader-map
   ;; "bb" 'previous-buffer
   ;; "bf" 'next-buffer
   "e" 'consult-buffer
   "ff" 'find-file
   "fr" 'consult-recent-file
   "fd" 'consult-dir
   "fR" 'rename-this-file-and-buffer
   "fD" 'delete-this-file
   "gr" 'rg-project
   "gg" 'consult-git-grep
   "h" 'help-command
   "ii" 'consult-imenu
   "im" 'consult-imenu-multi
   "jx" 'exchange-point-and-mark
   "mm" 'magit-status
   "mc" 'magit-clone
   "p" (general-simulate-key "C-c p")
   "q" 'save-buffers-kill-emacs
   "rb" 'consult-bookmark
   "rm" 'bookmark-set
   "rr" 'consult-register
   "rs" 'consult-register-store
   "s" 'save-buffer
   "wo" 'wg-open-workgroup
   "wk" 'wg-kill-workgroup
   "wc" 'wg-create-workgroup
   "ww" 'ace-window
   "xd" 'xref-find-definitions
   "xs" 'xref-find-apropos
   "xr" 'xref-find-references
   "y" 'browse-kill-ring
   "SPC" 'cycle-spacing)
  (general-auto-unbind-keys t))

;;---------------------------------------------------------------------
;; consult keybindings
;;---------------------------------------------------------------------
(when (maybe-require-package 'consult)
  (general-auto-unbind-keys)
  ;; (global-set-key (kbd "C-x c p f d") 'consult-projectile-find-dir)
  ;; (global-set-key (kbd "C-x c p f f") 'consult-projectile-find-file)
  ;; (global-set-key (kbd "C-x c p f r") 'consult-projectile-recentf)
  ;; (global-set-key (kbd "C-x c p b") 'consult-project-buffer)
  ;; (global-set-key (kbd "C-x c p s") 'consult-projectile-switch-project)
  (global-set-key (kbd "C-x c i") 'consult)
  ;; (global-set-key (kbd "C-x c i m") 'consult-imenu-multi)
  (global-set-key (kbd "C-x c d") 'consult-dir)
  (global-set-key (kbd "C-x c r") 'consult-ripgrep)
  (global-set-key (kbd "C-x c g") 'consult-git-grep)
  (global-set-key (kbd "C-x c m") 'consult-bookmark)
  (global-set-key (kbd "C-x c r") 'consult-register)
  (general-auto-unbind-keys t))



(provide 'init-evil)
;;; init-evil.el ends here
