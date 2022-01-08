
;;---------------------------------------------------------------------
;; Basic config
;;---------------------------------------------------------------------
(when (maybe-require-package 'orderless)
  (setq completion-styles '(orderless)
      completion-category-defaults nil
      completion-category-overrides '((file (styles partial-completion))))
      )

(when (maybe-require-package 'company)
  (setq company-idle-delay 0.2)
  (setq company-minimum-prefix-length 2)
  (setq company-show-numbers nil)
  (setq company-tooltip-limit 5)
  (setq company-tooltip-align-annotations t)
  )

;;---------------------------------------------------------------------
;; Functions & Hack
;;---------------------------------------------------------------------
;; my/company-complete <tab>
(defun my/company-complete ()
  "Insert the common part of all candidates or the current selection.
The first time this is called, the common part is inserted, the second
time, or when the selection has been changed, the selected candidate is
inserted."
  (interactive)
  (when (company-manual-begin)
    (if (or company-selection-changed
            (and (eq real-last-command 'my/company-complete)
                 (eq last-command 'company-complete-common))
            (not company-common)
            (equal company-prefix company-common))
        (call-interactively 'company-complete-selection)
      (call-interactively 'company-complete-common)
      (when company-candidates
        (setq this-command 'company-complete-common)))))


(with-eval-after-load 'company
  (diminish 'company-mode)
  (define-key company-active-map (kbd "<tab>")
              #'my/company-complete)
  (define-key company-active-map (kbd "<backtab>")
              (lambda ()
                (interactive)
                (company-complete-common-or-cycle -1)))
  (define-key company-active-map (kbd "M-.") 'company-show-location)
  (define-key company-active-map (kbd "M-/") 'company-other-backend)
  (setq-default company-dabbrev-other-buffers 'all
                company-tooltip-align-annotations t))

;; Company completion-styles always inorder
(add-hook 'company-completion-started-hook
          (lambda (&rest ignore)
            (setq completion-styles
                  '(basic))))
                  ;; '(partial-completion substring initials flex))))
(add-hook 'company-after-completion-hook
          (lambda (&rest ignore)
            (setq completion-styles '(orderless))))

;; Config different backend in different mode
(defun my-text-mode-company ()
  (when (boundp 'company-backends)
    (make-local-variable 'company-backends)
    (setq company-backends (delete 'company-ispell company-backends))
    (add-to-list 'company-backends 'company-ispell)))
(add-hook 'text-mode-hook #'my-text-mode-company)

;; company-yasnippet as 1'st backend
;; actually company-capf work?
(setq company-backends '((:separate company-yasnippet company-capf)))

;; company-transformers to sorted?

(global-company-mode 1)

(provide 'init-company)
