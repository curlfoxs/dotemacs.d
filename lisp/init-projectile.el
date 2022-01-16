;;; init-projectile.el --- Use Projectile for navigation within projects -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(when (maybe-require-package 'projectile)
  (add-hook 'after-init-hook 'projectile-mode)

;;---------------------------------------------------------------------
;; Customize modeline
;;---------------------------------------------------------------------
(setq-default projectile-mode-line-prefix " P")
(defun wullic/projectile-mode-line ()
  "Report project name and type in the modeline."
  (let ((project-name (projectile-project-name)))
    (format "%s[%s]"
	    projectile-mode-line-prefix
	    project-name)))

(setq projectile-mode-line-function 'wullic/projectile-mode-line)

(when (executable-find "rg")
  (setq-default projectile-generic-command "rg --files --hidden"))

(with-eval-after-load 'projectile
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

(maybe-require-package 'ibuffer-projectile))


(provide 'init-projectile)
;;; init-projectile.el ends here
