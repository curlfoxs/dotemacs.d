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
  (add-hook 'org-agenda-mode-hook (lambda () (define-key org-agenda-mode-map (kbd "SPC") wullic-leader-map)))

  (general-evil-setup)
  (general-auto-unbind-keys)
  (general-nmap
    :state '(normal motion visual emacs)
    :override t
    :prefix "SPC"
    :prefix-map 'wullic-leader-map
    ;; "bb" 'previous-buffer
    ;; "bf" 'next-buffer
    "0" 'delete-window
    "1" 'delete-other-windows
    "2" 'split-window-below
    "3" 'split-window-right
    "4" (general-simulate-key "C-x 4")
    "5" (general-simulate-key "C-x 5")
    ;; "c" 'my-query-replace-prefix
    ;; "a" 'org-agenda
    "d" 'dired-jump
    "ee" 'sanityinc/eval-last-sexp-or-region
    "el" 'sanityinc/load-this-file
    "ff" 'find-file
    "fr" 'consult-recent-file
    "fd" 'consult-dir
    "fR" 'rename-this-file-and-buffer
    "fD" 'delete-this-file
    "fs" 'consult-buffer
    "fp" 'previous-buffer
    "fn" 'next-buffer
    "fk" 'kill-buffer
    "gr" 'rg-project
    "ga" 'ag-project
    ;; "gl" 'consult-line-multi
    "h" 'help-command
    "i" 'consult-buffer  ;; ibuffer
    ;; "jx" 'exchange-point-and-mark
    "j;" 'avy-goto-char-timer ;; 「j」 Means jump
    "jl" 'consult-goto-line
    "jd" 'xref-find-definitions
    "jo" 'consult-outline
    "js" 'xref-find-apropos
    "jr" 'xref-find-references
    "lgg" 'conslut-git-grep ;; 「l」 Meas list， 它和consult的意思有异曲同工之妙。
    "lgr" 'consult-ripgrep
    "lm" 'consult-line-multi
    "lo" 'consult-outline
    "lii" 'consult-imenu
    "lim" 'consult-imenu-multi
    "ls" 'consult-buffer
    "mm" 'magit-status ;; 「m」 Means magit
    "mc" 'magit-clone
    "mb" 'consult-bookmark  ;; 「m」 Means mark
    "mf" 'bookmark-set ;; mark file
    "mr" 'consult-register
    "ml" 'consult-register-store  ;; mark line
    "o" 'ace-window
    "p" (general-simulate-key "C-c p")
    "q" 'save-buffers-kill-emacs
    "r" 'consult-recent-file
    "s" 'save-buffer
    "wo" 'wg-open-workgroup
    "wk" 'wg-kill-workgroup
    "wc" 'wg-create-workgroup
    "wp" 'winner-undo
    "wn" 'winner-redo
    "y" 'browse-kill-ring
    "SPC" 'cycle-spacing)
  (general-auto-unbind-keys t)
)

(provide 'init-evil)
;;; init-evil.el ends here
