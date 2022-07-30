;;; init-themes.el --- Defaults for themes -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require-package 'modus-themes)
(require-package 'doom-themes)
(require-package 'tangotango-theme)
(require-package 'color-theme-sanityinc-tomorrow)
(require-package 'color-theme-sanityinc-solarized)
(require-package 'leuven-theme)
(require-package 'srcery-theme)
(require-package 'solarized-theme)
(require-package 'spacemacs-theme)

;;------ ---------------------------------------------------------------
;; Font
;;---------------------------------------------------------------------
;; If you don't customize it, this is the theme you get.
(setq-default custom-enabled-themes '(modus-vivendi-theme))
;; (set-frame-font "Iosevka NF-16" nil t) ;; TODO if not install this font?
;; (add-to-list 'default-frame-alist '(font . "Iosevka Fixed-16"))

;;---------------------------------------------------------------------
;; Theme
;;---------------------------------------------------------------------

;; Don't prompt to confirm theme safety. This avoids problems with
;; first-time startup on Emacs > 26.3.
(setq custom-safe-themes t)

(defun reapply-theme ()
  "Forcibly load the themes listed in `custom-enabled-themes'."
  (dolist (theme custom-enabled-themes)
    (unless (custom-theme-p theme)
      (mapc #'disable-theme custom-enabled-themes)
      (load-theme theme)))
  (custom-set-variables `(custom-enabled-themes (quote ,custom-enabled-themes))))

(defun set-and-reapply-theme (theme)
  (setq custom-enabled-themes (list theme))
  (reapply-theme))
;;---------------------------------------------------------------------
;; dimmer config
;;---------------------------------------------------------------------

(when (maybe-require-package 'dimmer)
  (dimmer-configure-which-key)
  (setq-default dimmer-fraction 0.25)
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


;;---------------------------------------------------------------------
;; redguardtoo themes config
;;---------------------------------------------------------------------
(defun pickup-random-color-theme (themes)
  "Pickup random color theme from THEMES."
  (let* ((available-themes (mapcar 'symbol-name themes))
	 (theme (nth (random (length available-themes)) available-themes)))
    (set-and-reapply-theme (intern theme))
    (message "Color theme [%s] loaded." theme)))

(defvar wullic-favorite-dark-themes
  '(modus-operandi
    srcery
    doom-dracula
    doom-gruvbox
    doom-molokai
    doom-monokai-classic
    doom-monokai-machine
    doom-monokai-octagon
    doom-monokai-pro
    doom-monokai-spectrum
    doom-material-dark
    doom-moonlight
    doom-xcode
    doom-nova
    doom-nord
    doom-zenburn
    deeper-blue
    tango-dark
    leuven-dark
    solarized-dark-high-contrast
    solarized-gruvbox-dark
    sanityinc-solarized-dark
    sanityinc-tomorrow-blue
    sanityinc-tomorrow-eighties
    sanityinc-tomorrow-night
    spacemacs-dark)
  "My favorite dark themes.")

(defvar wullic-favorite-light-themes
  '(solarized-gruvbox-light
    leuven
    modus-vivendi
    sanityinc-solarized-light
    sanityinc-tomorrow-day
    spacemacs-dark)
  "My favorite light themes.")

(defvar wullic-favorite-color-themes
  (append wullic-favorite-dark-themes wullic-favorite-light-themes)
  "My favorite color themes.")

;; random color theme
(defun wullic/random-favorite-color-theme ()
  "Random color theme."
  (interactive)
  (pickup-random-color-theme (or wullic-favorite-color-themes
				    (custom-enable-themes))))

(defun wullic/random-favorite-light-theme ()
  "Random light theme."
  (interactive)
  (pickup-random-color-theme (or wullic-favorite-light-themes
				 (custom-enable-themes))))

(defun wullic/random-favorite-dark-theme ()
  "Random dark theme."
  (interactive)
  (pickup-random-color-theme (or wullic-favorite-color-themes
				    (custom-enable-themes))))

(defvar wullic-enable-startup-color-theme-p t
  "Enable color theme during Emacs startup.")
;; load color theme
(setq wullic-enable-startup-color-theme-p t)

;; Ensure that themes will be applied even if they have not been customized
(if wullic-enable-startup-color-theme-p
    (add-hook 'after-init-hook 'wullic/random-favorite-color-theme)
  (add-hook 'after-init-hook 'reapply-theme))

(provide 'init-themes)
;;; init-themes.el ends here
