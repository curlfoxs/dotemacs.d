;;; init-themes.el --- Defaults for themes -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;------ ---------------------------------------------------------------
;; Font
;;---------------------------------------------------------------------
;; If you don't customize it, this is the theme you get.
(setq-default custom-enabled-themes '(modus-vivendi))
(set-frame-font "Iosevka Fixed-16" nil t) ;; TODO if not install this font?

;;---------------------------------------------------------------------
;; Theme
;;---------------------------------------------------------------------
(require-package 'modus-themes)

;; Don't prompt to confirm theme safety. This avoids problems with
;; first-time startup on Emacs > 26.3.
(setq custom-safe-themes t)

;; Ensure that themes will be applied even if they have not been customized
(defun reapply-themes ()
  "Forcibly load the themes listed in `custom-enabled-themes'."
  (dolist (theme custom-enabled-themes)
    (unless (custom-theme-p theme)
      (load-theme theme)))
  (custom-set-variables `(custom-enabled-themes (quote ,custom-enabled-themes))))

(add-hook 'after-init-hook 'reapply-themes)


;; Toggle between light and dark

(defun light ()
  "Activate a light color theme."
  (interactive)
  (setq custom-enabled-themes '(modus-operandi))
  (reapply-themes))

(defun dark ()
  "Activate a dark color theme."
  (interactive)
  (setq custom-enabled-themes '(vscode-dark-plus))
  (reapply-themes))


(when (maybe-require-package 'dimmer)
  (setq-default dimmer-fraction 0.15)
  (add-hook 'after-init-hook 'dimmer-mode)
  (with-eval-after-load 'dimmer
    ;; TODO: file upstream as a PR
    (advice-add 'frame-set-background-mode :after (lambda (&rest args) (dimmer-process-all))))
  (with-eval-after-load 'dimmer
    ;; Don't dim in terminal windows. Even with 256 colours it can
    ;; lead to poor contrast.  Better would be to vary dimmer-fraction
    ;; according to frame type.
    (defun sanityinc/display-non-graphic-p ()
      (not (display-graphic-p)))
    (add-to-list 'dimmer-exclusion-predicates 'sanityinc/display-non-graphic-p)))


(load-theme 'modus-vivendi)
(provide 'init-themes)
;;; init-themes.el ends here
