;;; init-evil.el --- Insert description here -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;---------------------------------------------------------------------
;; Commands needed in these packages
;;---------------------------------------------------------------------

(require-package 'consult)
(require-package 'avy)
(require-package 'workgroups2) ;; Window and buffer manager
(add-hook 'after-init-hook 'workgroups-mode)   ; put this one at the bottom of .emacs

;;---------------------------------------------------------------------
;; Evil package config
;;---------------------------------------------------------------------

(when (maybe-require-package 'evil)
  ;; Use emacs basic bindings
  (setq evil-disable-insert-state-bindings t)
  (setq evil-move-beyond-eol t)
  (setq evil-move-cursor-back nil)
  (setq evil-want-minibuffer t)

  (add-hook 'after-init-hook 'evil-mode)

  (defvar wullic-leader-map (make-sparse-keymap)
    "Keymap for \" Wullic leader key\" shortcuts")

  (defun wullic/evil-return (arg)
    (interactive "p")
    (evil-insert 1)
    (newline-and-indent)
    (evil-normal-state))

  (with-eval-after-load 'evil
    (evil-define-key 'normal 'org-mode-map (kbd "TAB") 'org-cycle)
    (evil-define-key '(normal insert) 'global     (kbd "C-u") 'evil-normal-state)
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

      (kbd "C-.") nil
      (kbd "M-.") nil
      (kbd "C-m") 'wullic/evil-return
      (kbd "e") 'er/expand-region
      (kbd "m") 'set-mark-command
      (kbd "M") 'exchange-point-and-mark
      (kbd "H") 'sp-backward-sexp
      (kbd "L") 'sp-forward-sexp
      (kbd "J") 'sp-backward-up-sexp
      (kbd "K") 'kill-sexp
      (kbd ",") (general-simulate-key "C-x")
      (kbd ".") (general-simulate-key "C-c")
      (kbd "SPC") wullic-leader-map)
    )


  ;; Leader key config
  (add-hook 'special-mode-hook
	    (lambda () (define-key special-mode-map (kbd ",") (general-simulate-key "C-x"))))
  (add-hook 'special-mode-hook
	    (lambda () (define-key special-mode-map (kbd ".") (general-simulate-key "C-c"))))
  (add-hook 'special-mode-hook
	    (lambda () (define-key special-mode-map (kbd "SPC") wullic-leader-map)))
  (when (maybe-require-package 'magit)
      (add-hook 'magit-mode-hook
		(lambda () (define-key magit-mode-map (kbd "SPC") wullic-leader-map))))
  (add-hook 'dired-mode-hook (lambda () (define-key dired-mode-map (kbd "SPC") wullic-leader-map)))

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
    "j;" 'avy-goto-char-timer
    "jl" 'consult-goto-line
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
    "wp" 'winner-undo
    "wn" 'winner-redo
    "xd" 'xref-find-definitions
    "xs" 'xref-find-apropos
    "xr" 'xref-find-references
    "y" 'browse-kill-ring
    "SPC" 'cycle-spacing)
  (general-auto-unbind-keys t)
)

(provide 'init-evil)
;;; init-evil.el ends here
