(require-package 'consult)
(maybe-require-package 'marginalia)


;;---------------------------------------------------------------------
;; Basic config
;;---------------------------------------------------------------------
(global-set-key (kbd "C-s") 'consult-line)
(global-set-key (kbd "C-x b") 'consult-buffer)
(global-set-key (kbd "M-y") 'consult-yank-pop)
(global-set-key (kbd "C-c f") 'consult-recent-file)
;; Enable automatic preview at point in the *Completions* buffer. This is
;; relevant when you use the default completion UI. You may want to also
;; enable `consult-preview-at-point-mode` in Embark Collect buffers.
(add-hook 'completion-list-mode 'consult-preview-at-point-mode)

;; Optionally configure the register formatting. This improves the register
;; preview for `consult-register', `consult-register-load',
;; `consult-register-store' and the Emacs built-ins.
(setq register-preview-delay 0
      register-preview-function #'consult-register-format)

;; Optionally tweak the register preview window.
;; This adds thin lines, sorting and hides the mode line of the window.
(advice-add #'register-preview :override #'consult-register-window)

;; Optionally replace `completing-read-multiple' with an enhanced version.
(advice-add #'completing-read-multiple :override #'consult-completing-read-multiple)

;; Use Consult to select xref locations with preview
(setq xref-show-xrefs-function #'consult-xref
      xref-show-definitions-function #'consult-xref)
;;---------------------------------------------------------------------
;; Keymap & Basic config lazily
;;---------------------------------------------------------------------
(with-eval-after-load 'consult
  ;; (setq consult-preview-key (list (kbd "<S-down>") (kbd "<S-up>") (kbd "M-." )))
  ;; Both < and C-+ work reasonably well.
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme
   :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-recent-file consult--source-project-recent-file consult--source-bookmark
   :preview-key (kbd "M-."))

  (setq consult-narrow-key "<") ;; (kbd "C-+")
  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  ;; Optionally configure a function which returns the project root directory.
  (setq consult-project-root-function
        (lambda ()
          (when-let (project (project-current))
            (car (project-roots project)))))
  )

(require-package 'consult-dir)
(require-package 'consult-lsp)
(require-package 'consult-yasnippet)
(require-package 'consult-projectile)
;; (require-package 'consult-company)

(provide 'init-consult)
