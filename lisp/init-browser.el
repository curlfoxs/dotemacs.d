;;; init-browser.el --- Insert description here -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;---------------------------------------------------------------------
;; eww browser settings
;;---------------------------------------------------------------------
;; (setq
;;  browse-url-browser-function 'eww-browse-url ; Use eww as the default browser
;;  shr-use-fonts  "Iosevka Fixed-18"           ; No special fonts
;;  shr-use-colors nil                          ; No colours
;;  shr-indentation 2                           ; Left-side margin
;;  shr-width 70                                ; Fold text to 70 columns
;;  eww-search-prefix "https://wiby.me/?q=")    ; Use another engine for searching

;;---------------------------------------------------------------------
;; Use Chrome browser
;;---------------------------------------------------------------------
;; @see https://stackoverflow.com/questions/4506249/how-can-i-make-emacs-org-mode-open-links-to-sites-in-google-chrome
(cond (*is-a-mac* (setq browse-url-browser-function 'browse-url-default-macosx-browser))
      (*is-a-ms-windows* (setq browse-url-browser-function 'browse-url-default-windows-browser))
      (t (setq browse-url-browser-function 'browse-url-generic)))
(setq browse-url-generic-program "chromium-browser")

;;---------------------------------------------------------------------
;; PDF tools
;;---------------------------------------------------------------------
;; if you use ms-windows system
;; @see https://github.com/politza/pdf-tools/blob/master/README.org#compilation-and-installation-on-windows
;; @sse https://emacs.stackexchange.com/questions/41784/installing-pdf-tools-on-windows
;; (setenv "PATH" (concat "C:\\Users\\15876\\scoop\\apps\\msys2\\2022-06-03\\mingw64\\bin;" (getenv "PATH")))
(when *is-a-mac*
  (when (require-package 'pdf-tools)
    (pdf-tools-install)))

(provide 'init-browser)
;;; init-browser.el ends here
