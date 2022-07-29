;;; init-text.el --- Insert description here -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;---------------------------------------------------------------------
;; Dictionary
;;---------------------------------------------------------------------

(when *is-a-mac*
  (maybe-require-package 'osx-dictionary)
  (maybe-require-package 'chinese-word-at-point)
  (global-set-key (kbd "C-c d") 'osx-dictionary-search-word-at-point)
  (setq osx-dictionary-use-chinese-text-segmentation t))

(provide 'init-text)
;;; init-text.el ends here
