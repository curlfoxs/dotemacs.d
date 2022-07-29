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
;; Chrome browser settings
;;---------------------------------------------------------------------
;; Chrome as default browser
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "chromium-browser")

(setq browse-url-browser-function 'browse-url-default-windows-browser)
(setq browse-url-browser-function 'browse-url-default-macosx-browser)
;;---------------------------------------------------------------------
;; PDF tools
;;---------------------------------------------------------------------
;; (maybe-require-package 'pdf-tools)
;; (pdf-tools-install)

(provide 'init-browser)
;;; init-browser.el ends here
