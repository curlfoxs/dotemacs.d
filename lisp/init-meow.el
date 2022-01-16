;;; init-meow.el --- Insert description here -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require-package 'meow)
(maybe-require-package 'avy)

(require 'meow) ;; why need require again?

(defun wullic/meow--fix-word-selection-mark (pos mark)
  "Return new mark for a word select.
This will shrink the word selection only contains
 word/symbol constituent character and whitespaces."
  (save-mark-and-excursion
    (goto-char pos)
    (if (> mark pos)
	(progn (skip-syntax-forward "w" mark) ;; Just select a word
	       (point))
      (skip-syntax-backward "w" mark)
      (point))))

(defun wullic/meow-append-line ()
  (interactive)
  (end-of-line)
  (meow-insert))

(defun wullic/meow-insert-line ()
  (interactive)
  (back-to-indentation)
  (meow-insert))

(defun wullic/meow-append ()
  "Move to the end of selection, switch to INSERT state."
  (interactive)
  (if meow--temp-normal
      (progn
	(meow--switch-state 'motion))
    (if (not (region-active-p))
	(progn
	  (forward-char 1)
	  (meow-insert))
	;; (when (and meow-use-cursor-position-hack
	;;	   (< (point) (point-max)))
	;;   (forward-char 1))
      (meow--direction-forward)
      (meow--cancel-selection))
    (meow--switch-state 'insert)))

(defun wullic/meow-next-word (n)
  (interactive "p")
  (unless (equal 'word (cdr (meow--selection-type)))
    (meow--cancel-selection))
  (let* ((expand (equal '(expand . word) (meow--selection-type)))

	 (_ (when expand (meow--direction-forward)))
	 (type (if expand '(expand . word) '(select . word)))
	 (m (point))
	 (p (save-mark-and-excursion
	      (when (forward-word n)
		(point)))))
    (when p
      (thread-first
	(meow--make-selection type (wullic/meow--fix-word-selection-mark p m) p expand)
	(meow--select))
      )))

(defun wullic/meow-back-word (n)
  (interactive "p")
  (unless (equal 'word (cdr (meow--selection-type)))
    (meow--cancel-selection))
  (let* ((expand (equal '(expand . word) (meow--selection-type)))
	 (_ (when expand (meow--direction-backward)))
	 (type (if expand '(expand . word) '(select . word)))
	 (m (point))
	 (p (save-mark-and-excursion
	      (when (backward-word n)
		(point)))))
    (when p
      (thread-first
	(meow--make-selection type (meow--fix-word-selection-mark p m) p expand)
	(meow--select))
      )))

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
   '("a" . wullic/meow-append)
   ;; '("a" . crux-move-beginning-of-line)

   '("A" . wullic/meow-append-line)
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
   '("I" . wullic/meow-insert-line)
   ;; '("j" . meow-find)
   '("j" . meow-right)
   '("k" . meow-kill)
   '("l" . meow-left)
   '("m" . meow-mark-word)
   '("M" . meow-mark-symbol)
   '("n" . meow-next)
   '("N" . meow-next-expand)
   '("o" . er/expand-region)
   '("O" . meow-to-block)
   '("p" . meow-prev)
   '("P" . meow-prev-expand)
   '("q" . meow-quit)
   '("r" . wullic/meow-back-word)
   '("R" . meow-swap-grab)
   '("s" . meow-search)
   '("t" . meow-end-of-thing)
   '("T" . meow-right-expand)
   '("u" . meow-undo)
   '("U" . meow-undo-in-selection)
   '("v" . meow-visit)
   '("w" . wullic/meow-next-word)
   '("W" . meow-next-symbol)
   '("x" . save-buffer)
   '("X" . meow-sync-grab)
   '("y" . yank)
   '("z" . meow-pop-selection)
   '("'" . repeat)
   '("<escape>" . ignore)))

(with-eval-after-load 'meow
  (meow-setup)
  (meow-global-mode 1)
)
;; (add-hook 'prog-mode-hook (lambda () (meow-setup)
			    ;; (meow-global-mode t)))
(provide 'init-meow)
;;; init-meow.el ends here
