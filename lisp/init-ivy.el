;;; init-ivy.el --- Insert description here -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(when (maybe-require-package 'ivy)
  (ivy-mode)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t))
(provide 'init-ivy)
;;; init-ivy.el ends here
