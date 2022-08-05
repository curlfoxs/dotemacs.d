;;; init-folding.el --- Support code and region folding -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(when (maybe-require-package 'origami)
  (with-eval-after-load 'origami
    (define-key origami-mode-map (kbd "C-c f") 'origami-recursively-toggle-node)
    (define-key origami-mode-map (kbd "C-c F") 'origami-toggle-all-nodes)))

(with-eval-after-load 'origami
  (diminish 'origami-mode))

(add-hook 'prog-mode-hook 'global-origami-mode)

(provide 'init-folding)
;;; init-folding.el ends here
