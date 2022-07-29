;;; init-c-sharp.el --- Insert description here -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(maybe-require-package 'tree-sitter)
(maybe-require-package 'tree-sitter-langs)

(when (maybe-require-package 'csharp-mode)
  (add-to-list 'auto-mode-alist '("\\.cs\\'" . csharp-tree-sitter-mode)))


(provide'init-c-sharp)
;;; init-c-sharp.el ends here
