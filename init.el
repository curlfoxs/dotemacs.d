;;; init.el --- Load the full configuration -*- lexical-binding: t -*-

;; Author: wullic
;; This file is not part of GNU Emacs.

;;; Commentary:

;;; Code:

;; Produce backtraces when errors occur: can be helpful to diagnose startup issues
;;(setq debug-on-error t)

(setq user-init-file (or load-file-name (buffer-file-name)))
(setq user-emacs-directory (file-name-directory user-init-file))

;;---------------------------------------------------------------------
;; Enviroment
;;---------------------------------------------------------------------
(defvar current-user
  (getenv
   (if (equal system-type 'windows-nt) "USERNAME" "USER")))

(message "Powering up... Be patient, Master %s!" current-user)
(let ((min-version "25.1"))
  (when (version< emacs-version min-version)
    (error "Requires at least GNU Emacs 25.1, but you're running %s" emacs-version)))
(when (version< emacs-version "26.1")
  (message "Your Emacs is a little old, and some features will be lost. Please upgrade if possible."))
(message "Your Emacs version is %s, so amazing and enjoy it as soon as possible" emacs-version)

;;---------------------------------------------------------------------
;; Load path
;;---------------------------------------------------------------------
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "site-lisp" user-emacs-directory))
(require 'init-benchmarking) ;; Measure startup time and hack it
(setq inhibit-startup-echo-area-message t)

;;---------------------------------------------------------------------
;; Adjust Garbage collection threshold during startup, normal time
;;---------------------------------------------------------------------
(let ((normal-gc-cons-threshold (* 20 1024 1024)) ;; 20M
      (init-gc-cons-threshold (* 128 1024 1024))) ;; 128M
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'emacs-startup-hook
            (lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))

;;---------------------------------------------------------------------
;; Bootstrap core
;;---------------------------------------------------------------------
(require 'init-preload)
(message "Loading bootstrap core ...")
(setq custom-file (locate-user-emacs-file "custom.el"))
(require 'init-utils)     ;; Helper function
;; (require 'init-site-lisp) ;; Must come before elpa, as it may provide package.el
;; Calls (package-initialize)
(message "Loading package info ... ")
(require 'init-elpa)     ;; Auto installing required packages
(message "Loading package info completed")
(require 'init-exec-path) ;; Set up %PATH

(defconst *spell-check-support-enabled* nil) ;; Enable with t if you prefer, it wiil effect start time heavily
(defconst *is-a-mac* (eq system-type 'darwin)) ;; mac osx system
(defconst *is-a-ms-windows* (eq system-type 'windows-nt)) ;; ms windows system
(when *is-a-mac* (set-keyboard-coding-system nil)) ;; The Apple X11 server make confusion when using "Metaq"
(when *is-a-ms-windows* (setq ring-bell-function 'ignore)) ;; Close Windows notify sound
;;---------------------------------------------------------------------
;; Load specific features and modes
;;---------------------------------------------------------------------
(message "Loading magic modules ...")

(maybe-require-package 'scratch)
(require-package 'command-log-mode) ;; Amazing debug tool
(require-package 'diminish) ;; Simplify modebar
(require-package 'general) ;; Amazing keybinding tool
(require-package 'use-package) ;; Good use-package to install package

(require 'init-frame-hooks)
(require 'init-gui)
(require 'init-themes)
(require 'init-term) ; term emulator
;; (require 'init-dired)
(require 'init-wgrep) ;; exclude mode and wgrep-replace!!!
;; (require 'init-ibuffer)
(require 'init-flycheck)
(require 'init-text)
;; (require 'init-flyspell)
(require 'init-recentf)
;; (require 'init-minibuffer)
;; (require 'init-tabbar)
;; (require 'init-windows)
;; (require 'init-sessions)
(require 'init-mmm) ;; only one consistent buffer
;; (require 'init-editing-utils)
(require 'init-sexp)
(require 'init-whitespace)
(require 'init-editing-tools) ;; TODO multiple column mode
(require 'init-git)
;; (require 'init-github)
;; (require 'init-neotree) ;; file tree
(require 'init-projectile)

;; (require 'init-meow)
(require 'init-vertico)
;; (require 'init-ivy)
(require 'init-consult)
(require 'init-embark)
(require 'init-company)

(require 'init-org)
;; (require 'init-org-roam)
;; (require 'init-lsp)
(require 'init-emacs-lisp)
(require 'init-python)
;; (require 'init-c)
(require 'init-c-lsp)
(require 'init-c-sharp)
(require 'init-yasnippet) ;; Newer write code by hand

(require 'init-folding)
(require 'init-dash)
(require 'init-highlight)

(require 'init-browser)
(require 'init-direnv) ;; Enviroment directory useful for almost env & work with nigix

(require 'init-svg-tag) ;; Added in  2207

(require 'init-wullic) ;; wullic personal keymap

(require 'init-evil) ;; evil keymap 
;;---------------------------------------------------------------------
;; Load user custom.el
;;---------------------------------------------------------------------
(when (file-exists-p custom-file)
  (load custom-file))
;; (require 'init-locales)
;; (require 'init-local nil t)

(message "Ready to do thy bidding, Master %s!" current-user)
(provide 'init)
;;; init.el ends here
;; (put 'narrow-to-region 'disabled nil)
