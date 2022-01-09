;;; init-meow.el --- Insert description here -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require-package 'meow)
(maybe-require-package 'avy)

(require 'meow) ;; why need require again?
(defun meow-setup ()
  (setq meow-cheatsheet-layout meow-cheatsheet-layout-dvp)
  (meow-leader-define-key
   '("?" . meow-cheatsheet))
  (meow-motion-overwrite-define-key
   ;; custom keybinding for motion state
   '("<escape>" . ignore))
  (meow-normal-define-key
   '("?" . meow-cheatsheet)
   '("*" . meow-expand-0)
   '("=" . meow-expand-9)
   '("!" . meow-expand-8)
   '("[" . meow-expand-7)
   '("]" . meow-expand-6)
   '("{" . meow-expand-5)
   '("+" . meow-expand-4)
   '("}" . meow-expand-3)
   '(")" . meow-expand-2)
   '("(" . meow-expand-1)
   '("1" . digit-argument)
   '("2" . digit-argument)
   '("3" . digit-argument)
   '("4" . digit-argument)
   '("5" . digit-argument)
   '("6" . digit-argument)
   '("7" . digit-argument)
   '("8" . digit-argument)
   '("9" . digit-argument)
   '("0" . digit-argument)
   '("-" . negative-argument)
   '(";" . meow-reverse)
   '("," . meow-bounds-of-thing)
   '("." . meow-inner-of-thing)
   '("<" . meow-beginning-of-thing)
   '(">" . meow-end-of-thing)
   '("a" . meow-append)
   ;; '("a" . crux-move-beginning-of-line)

   '("A" . meow-open-below)
   '("b" . meow-beginning-of-thing)
   '("B" . meow-back-symbol)
   '("c" . meow-change)
   '("d" . sp-delete-char)
   '("D" . meow-backward-delete)
   '("e" . meow-line)
   '("E" . meow-goto-line)
   '("f" . avy-goto-char-in-line)
   ;; '("f" . meow-right-expand)
   '("g" . meow-cancel-selection)
   '("G" . meow-grab)
   '("h" . sp-backward-delete-char)
   '("H" . meow-left-expand)
   '("i" . meow-insert)
   '("I" . meow-open-above)
   ;; '("j" . meow-find)
   '("j" . forward-char)
   '("k" . meow-kill)
   '("l" . backward-char)
   '("m" . meow-mark-word)
   '("M" . meow-mark-symbol)
   '("n" . meow-next)
   '("N" . meow-next-expand)
   '("o" . er/expand-region)
   '("O" . meow-to-block)
   '("p" . meow-prev)
   '("P" . meow-prev-expand)
   '("q" . meow-quit)
   '("r" . meow-back-word)
   '("R" . meow-swap-grab)
   '("s" . meow-search)
   '("t" . meow-end-of-thing)
   '("T" . meow-right-expand)
   '("u" . meow-undo)
   '("U" . meow-undo-in-selection)
   '("v" . meow-visit)
   '("w" . meow-next-word)
   '("W" . meow-next-symbol)
   '("x" . save-buffer)
   '("X" . meow-sync-grab)
   '("y" . yank)
   '("z" . meow-pop-selection)
   '("'" . repeat)
   '("<escape>" . ignore)))

(with-eval-after-load 'meow
  (meow-setup)
  (meow-global-mode 1))

(provide 'init-meow)
;;; init-meow.el ends here
