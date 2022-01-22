;;; init-wullic.el --- Insert description here -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;;; Parens matching
;;---------------------------------------------------------------------
;; minor mode definition
;;---------------------------------------------------------------------

(defvar wullic-mode-map
  (let ((map (make-sparse-keymap)))
    map)
  "Keymap for wullic mode.")

;;---------------------------------------------------------------------
;; ;;; helper function
;;---------------------------------------------------------------------

(defun my-step-out-forward ()
  "Step forward out of current list or string."
  (interactive)
  (forward-char)
  (if (nth 3 (syntax-ppss (point)))
      (progn
        (forward-char)
        (while (and (not (eobp)) (nth 3 (syntax-ppss (point))))
          (forward-char)))
    (up-list)))

(defun my-step-out-backward ()
  "Step backward out of current list or string."
  (interactive)
  (backward-char)
  (backward-up-list)
  )

;; my-projectile-ibuffer
(defun my-projectile-ibuffer (prompt-for-project)
  "Split window horizontally and open projectile-ibuffer"
  (interactive "P")
  (let ((width (/ (window-total-width) 2)))
    (split-window-horizontally (- width))
    (other-window 1)
    (projectile-ibuffer-by-project (projectile-project-root))))

;; my-eshell
(defun my-eshell ()
  "Split window vertically and open eshell"
  (interactive)
  (let ((height (/ (window-total-height) 4)))
    (split-window-vertically (- height))
    (other-window 1)
    (eshell)))

(defun my-vterm ()
  "Split window vertically and open eshell"
  (interactive)
  (let ((height (/ (window-total-width) 3)))
    (split-window-horizontally (- height))
    (other-window 1)
    (vterm)))
;;---------------------------------------------------------------------
;; Magit
;;---------------------------------------------------------------------

(define-key wullic-mode-map (kbd "s-m m") 'magit-status)
(define-key wullic-mode-map (kbd "s-m j") 'magit-dispatch)
(define-key wullic-mode-map (kbd "s-m k") 'magit-file-dispatch)
(define-key wullic-mode-map (kbd "s-m l") 'magit-log-buffer-file)
(define-key wullic-mode-map (kbd "s-m b") 'magit-blame)

;;---------------------------------------------------------------------
;; Avy & Jump
;;---------------------------------------------------------------------

(define-key wullic-mode-map (kbd "s-j") nil)
(define-key wullic-mode-map (kbd "s-j m") 'bookmark-set)
(define-key wullic-mode-map (kbd "s-j b") 'bookmark-jump)
(define-key wullic-mode-map (kbd "s-j w") 'avy-goto-word-1)
;; (define-key wullic-mode-map (kbd "s-j s") 'avy-goto-char)
(define-key wullic-mode-map (kbd "s-j j") 'avy-goto-char-timer)
(define-key wullic-mode-map (kbd "s-j c") 'avy-copy-line)
(define-key wullic-mode-map (kbd "s-j k") 'avy-kill-whole-line)
(define-key wullic-mode-map (kbd "s-j r") 'avy-kill-region)
(define-key wullic-mode-map (kbd "s-j l") `goto-line)
(define-key wullic-mode-map (kbd "s-j i") `avy-goto-char-in-line)
(define-key wullic-mode-map (kbd "s-j p") 'pop-global-mark)
(define-key wullic-mode-map (kbd "s-j s") 'exchange-point-and-mark)
(define-key wullic-mode-map (kbd "s-j a") 'my-step-out-backward)
(define-key wullic-mode-map (kbd "s-j e") 'my-step-out-forward)
(define-key wullic-mode-map (kbd "s-;") 'avy-goto-char-timer)

;;---------------------------------------------------------------------
;; Definition & Referencees
;;---------------------------------------------------------------------

;;; Jump symbol definition/references
;; (define-key wullic-mode-map (kbd "s-d") nil)
;; (define-key wullic-mode-map (kbd "s-d d") 'lsp-find-definition)
;; (define-key wullic-mode-map (kbd "s-d r") 'lsp-find-references)


;;---------------------------------------------------------------------
;; FSA
;;---------------------------------------------------------------------
;;; Filter/Select/Action
;; search
(define-key wullic-mode-map (kbd "s-i i") 'consult-imenu)
(define-key key-translation-map (kbd "s-i p") (kbd "C-c p"))
(define-key wullic-mode-map (kbd "s-i g") 'rgrep)
(define-key wullic-mode-map (kbd "s-i f") 'consult-find)
(define-key wullic-mode-map (kbd "s-i j") 'consult-git-grep)
(define-key wullic-mode-map (kbd "s-i k") 'consult-ripgrep)
(define-key wullic-mode-map (kbd "s-i l") 'consult-locate)
(define-key wullic-mode-map (kbd "s-i h") 'consult-history)
;; consult find
(define-key wullic-mode-map (kbd "s-f") 'find-file)
(define-key wullic-mode-map (kbd "s-r") 'consult-recent-file)
;;---------------------------------------------------------------------
;; Common command prefix
;;---------------------------------------------------------------------

;; my-consult-prefix
(defun my-consult-prefix ()
  (interactive)
  (minibuffer-with-setup-hook (lambda () (insert "consult "))
    (call-interactively #'execute-extended-command)))
(define-key wullic-mode-map (kbd "s-i c") 'my-consult-prefix)

;; my-embark-prefix
(defun my-embark-prefix ()
   (interactive)
   (minibuffer-with-setup-hook (lambda () (insert "embark "))
     (call-interactively #'execute-extended-command)))
(define-key wullic-mode-map (kbd "s-i a") 'my-embark-prefix)

;; my-query-prefix
(defun my-query-replace-prefix ()
  (interactive)
  (minibuffer-with-setup-hook (lambda () (insert "query replace "))
    (call-interactively #'execute-extended-command)))
(define-key wullic-mode-map (kbd "s-i r") 'my-query-replace-prefix)

;; my-projectile-prefix
(defun my-projectile-prefix ()
  (interactive)
  (minibuffer-with-setup-hook (lambda () (insert "projectile "))
    (call-interactively #'execute-extended-command)))
(define-key wullic-mode-map (kbd "s-i p") 'my-projectile-prefix)


;;---------------------------------------------------------------------
;; Basic Config, EDit, Config
;;---------------------------------------------------------------------
;; (define-key wullic-mode-map (kbd "C-<return>") 'toggle-frame-fullscreen)
;;; Edit skill & Cursor control
;; meow-mode
;; C-i unbind with <TAB>
(define-key input-decode-map (kbd "C-i") (kbd "H-i"))
(define-key key-translation-map (kbd "H-i") (kbd "<escape>"))
;; (define-key wullic-mode-map (kbd "s-k") 'crux-kill-whole-line)
(define-key wullic-mode-map (kbd "C-a") 'back-to-indentation)
(define-key wullic-mode-map (kbd "s-w") 'ace-window)
(global-set-key (kbd "s-e") 'er/expand-region)
(define-key wullic-mode-map (kbd "C-j") 'join-line)
;; (define-key key-translation-map (kbd "C-j") (kbd "RET"))
;; (define-key key-translation-map (kbd "C-j") (kbd "C-SPC"))
(global-set-key (kbd "C-;") nil)
(define-key wullic-mode-map (kbd "C-;") 'comment-line)
;; (global-set-key (kbd "<backtab>") 'python-indent-shift-left)
;; (global-set-key (kbd "s-w") 'easy-kill)
(define-key key-translation-map (kbd "M-h") (kbd "M-DEL"))
;; newline
(global-set-key (kbd "C-o") `meow-open-above)
(define-key wullic-mode-map (kbd "s-o") nil)
(global-set-key (kbd "s-o") 'meow-open-below)
;; buffer control
(define-key wullic-mode-map (kbd "s-u") nil)
(define-key wullic-mode-map (kbd "s-u v") 'revert-buffer)
(define-key wullic-mode-map (kbd "s-u p") 'previous-buffer)
(define-key wullic-mode-map (kbd "s-u n") 'next-buffer)
(define-key wullic-mode-map (kbd "s-u k") 'kill-buffer)
(define-key wullic-mode-map (kbd "s-u i") 'ibuffer)
(define-key wullic-mode-map (kbd "s-u u") 'consult-buffer)
(define-key wullic-mode-map (kbd "s-u U") 'projectile-ibuffer)
;; window conrtorl
;; (define-key wullic-mode-map (kbd "s-u s") 'crux-swap-windows)
(define-key wullic-mode-map (kbd "s-u 1") 'delete-other-windows)
(define-key wullic-mode-map (kbd "s-u 0") 'delete-window)
(define-key wullic-mode-map (kbd "s-u 2") 'split-window-below)
(define-key wullic-mode-map (kbd "s-u 3") 'split-window-right)
;; workgroups2
(define-key wullic-mode-map (kbd "s-u w c") 'wg-create-workgroup)
(define-key wullic-mode-map (kbd "s-u w o") 'wg-open-workgroup)
(define-key wullic-mode-map (kbd "s-u w k") 'wg-kill-workgroup)
;; tabbar
(define-key wullic-mode-map (kbd "s-u t") 'my-select-tab-by-name)

(define-key wullic-mode-map (kbd "s-p") 'previous-buffer)
(define-key wullic-mode-map (kbd "s-n") 'next-buffer)
(define-key wullic-mode-map (kbd "s-k") 'kill-whole-line)

(global-set-key (kbd "M-p") 'backward-paragraph)
(global-set-key (kbd "M-n") 'forward-paragraph)
(global-set-key (kbd "s-<up>") 'drag-stuff-up)
(global-set-key (kbd "s-<down>") 'drag-stuff-down)
(global-set-key (kbd "s-<left>") 'previous-buffer)
(global-set-key (kbd "s-<right>") 'next-buffer)

;;---------------------------------------------------------------------
;; Help
;;---------------------------------------------------------------------

;;; Help menu
(global-unset-key (kbd "s-h"))
(global-set-key (kbd "s-h") 'help-command)
;; (global-set-key (kbd "C-h") 'sp-backward-delete-char)


;;; Highlight symbol
;; (global-set-key (kbd "s-h s-h") 'help-command)
;; (global-set-key (kbd "s-h h") 'highlight-symbol-at-point)
;; (global-set-key (kbd "s-h n") 'highlight-symbol-next)
;; (global-set-key (kbd "s-h p") 'highlight-symbol-prev)


;;; undo-Redo
;; C-c <left>, C-c <right>
(global-set-key (kbd "C-/") 'undo)
(global-set-key (kbd "C-r") 'undo-redo)


;;; Shell
(global-set-key (kbd "C-x s") 'my-vterm)
;; (global-set-key (kbd "C-q") 'set-mark-command)
;; (global-set-key (kbd "s-s") 'save-buffer)
(global-set-key (kbd "C-x m") 'my-eshell)

(define-minor-mode wullic-mode
  "Minor mode to consolidate Emacs Prelude extensions.

\\{wullic-mode-map}"
  :lighter " *Wullic*"
  :keymap wullic-mode-map
  :global t)

(wullic-mode 1)

(provide 'init-wullic)
;;; init-wullic.el ends here
